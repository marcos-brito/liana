{-# LANGUAGE OverloadedStrings #-}

-- |
-- This is a parser for a little DSL used to test the compiler.
-- It should be temporary. The ideal would be to use the attribute
-- capabilities of liana and then create a pass to run
-- the tests.
--
-- This
--
-- // Identifier
-- //
-- // Tests for valid identifiers
--
-- // pass
-- // diag_count 0
-- // diag_patt "bad"
--
-- should eventually become
--
-- @CompilerTest(
--     name: "Identifier",
--     desc: "Test for valid identifiers",
--     checks: list(
--         Pass,
--         DiagCount(3),
--         DiagPatt("bad"),
--     )
-- )
--
-- == The grammar
--
-- test_info = name, desc, { check } ;
-- name = string, blank_line ;
-- desc = string, blank_line ;
-- check
--     = ( "pass", ":", boolean
--     | "diag_code", ":", string
--     | "diag_count", ":" integer
--     | "diag_pattern", ":", string ),
--     new_line ;
-- integer =  { "0" .. "9" } ;
-- boolean = "true" | "false" ;
-- string = // Except spaces and newlines
-- new_line = "\n" ;
-- blank_line = new_line, new_line ;
module Test.Parser where

import Data.Text as T (Text, pack)
import Data.Void
import Test.Runner
import Text.Megaparsec
import Text.Megaparsec.Char (eol, space1)
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude hiding (fail)

type Parser = Parsec Void Text

type Error = ParseErrorBundle Text Void

parseTest :: FilePath -> Text -> Either Error Test
parseTest file = runParser (testInfo file) file

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

rest :: Parser Text
rest = takeWhileP Nothing (/= '\n')

integer :: Parser Int
integer = L.decimal

boolean :: Parser Bool
boolean = True <$ symbol "true" <|> False <$ symbol "false"

testInfo :: FilePath -> Parser Test
testInfo file =
  Test
    <$> name
    <*> desc
    <*> sepBy check eol
    <*> pure (T.pack file)

name :: Parser Text
name = takeWhileP Nothing (/= '\n')

desc :: Parser Text
desc = takeWhileP Nothing (/= '\n')

check :: Parser Check
check = compiles <|> diagCode <|> diagCount

baseCheck :: (a -> Check) -> Text -> Parser a -> Parser Check
baseCheck con k p = con <$ symbol k <* symbol ":" <*> p

compiles :: Parser Check
compiles = baseCheck Compiles "compiles" boolean

diagCode :: Parser Check
diagCode = baseCheck DiagCode "diag_code" rest

diagCount :: Parser Check
diagCount = baseCheck DiagCount "diag_count" integer
