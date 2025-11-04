module Parser.Literal where

import Parser.Base
import Syntax.Literal
import Text.Parsec hiding (hexDigit)
import Prelude hiding (False, True, exponent)

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

floatLiteral :: Parsec String st FloatLiteral
floatLiteral = try fractionalFloatLiteral <|> try exponentFloatLiteral <|> dotFloatLiteral

fractionalFloatLiteral :: Parsec String st FloatLiteral
fractionalFloatLiteral = do
  i <- decimalLiteral
  _ <- char '.'
  f <- decimalLiteral
  e <- optionMaybe exponent
  return $ FloatLiteral (Just i) (Just f) e

exponentFloatLiteral :: Parsec String st FloatLiteral
exponentFloatLiteral = do
  i <- decimalLiteral
  e <- optionMaybe exponent
  return $ FloatLiteral (Just i) Nothing e

dotFloatLiteral :: Parsec String st FloatLiteral
dotFloatLiteral = do
  _ <- char '.'
  f <- decimalLiteral
  e <- optionMaybe exponent
  return $ FloatLiteral Nothing (Just f) e

exponent :: Parsec String st Exponent
exponent = do
  _ <- char 'e'
  sign <-
    optionMaybe $
      do
        char '+' >> return Pos
        <|> (char '-' >> return Neg)
  Exponent sign <$> decimalLiteral

booleanLiteral :: Parsec String st BooleanLiteral
booleanLiteral = (string "true" >> return True) <|> (string "false" >> return False)
