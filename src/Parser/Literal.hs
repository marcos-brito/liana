module Parser.Literal where

import Parser.Base
import Syntax.Literal
import Text.Parsec hiding (hexDigit)

integerLiteral :: Parsec String st IntegerLiteral
integerLiteral = decimalLiteral <|> binaryLiteral <|> octalLiteral <|> hexLiteral

decimalLiteral :: Parsec String st IntegerLiteral
decimalLiteral = do
  first <- decimalDigit
  rest <- many $ do
    optional (char '_')
    decimalDigit
  return $ DecimalLiteral (first : rest)

binaryLiteral :: Parsec String st IntegerLiteral
binaryLiteral = baseInteger "0b" binaryDigit BinaryLiteral

octalLiteral :: Parsec String st IntegerLiteral
octalLiteral = baseInteger "0o" octalDigit OctalLiteral

hexLiteral :: Parsec String st IntegerLiteral
hexLiteral = baseInteger "0x" hexDigit HexLiteral

baseInteger ::
  String ->
  Parsec String st Char ->
  (String -> IntegerLiteral) ->
  Parsec String st IntegerLiteral
baseInteger prefix parser con = do
  _ <- string prefix
  optional $ char '_'
  first <- parser
  rest <- many $ do
    optional $ char '_'
    parser
  return $ con $ first : rest
