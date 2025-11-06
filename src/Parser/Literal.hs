module Parser.Literal where

import Parser.Base
import Syntax.Literal
import Text.Parsec hiding (hexDigit)
import Prelude hiding (exponent)

literal :: Parser Literal
literal = try floatLiteral <|> integerLiteral <|> booleanLiteral

integerLiteral :: Parser Literal
integerLiteral = decimalLiteral <|> binaryLiteral <|> octalLiteral <|> hexLiteral

decimalLiteral :: Parser Literal
decimalLiteral = do
  first <- decimalDigit
  rest <- many $ do
    optional (char '_')
    decimalDigit
  return $ IntegerLiteral Dec (first : rest)

binaryLiteral :: Parser Literal
binaryLiteral = baseInteger "0b" binaryDigit Bin

octalLiteral :: Parser Literal
octalLiteral = baseInteger "0o" octalDigit Oct

hexLiteral :: Parser Literal
hexLiteral = baseInteger "0x" hexDigit Hex

baseInteger ::
  String ->
  Parsec String st Char ->
  Base ->
  Parsec String st Literal
baseInteger prefix parser base = do
  _ <- string prefix
  optional $ char '_'
  first <- parser
  rest <- many $ do
    optional $ char '_'
    parser
  return $ IntegerLiteral base (first : rest)

floatLiteral :: Parser Literal
floatLiteral = try fractionalFloatLiteral <|> try exponentFloatLiteral <|> dotFloatLiteral

fractionalFloatLiteral :: Parser Literal
fractionalFloatLiteral = do
  i <- decimalLiteral
  _ <- char '.'
  f <- decimalLiteral
  e <- optionMaybe exponent
  return $ FloatLiteral (Just i) (Just f) e

exponentFloatLiteral :: Parser Literal
exponentFloatLiteral = do
  i <- decimalLiteral
  e <- optionMaybe exponent
  return $ FloatLiteral (Just i) Nothing e

dotFloatLiteral :: Parser Literal
dotFloatLiteral = do
  _ <- char '.'
  f <- decimalLiteral
  e <- optionMaybe exponent
  return $ FloatLiteral Nothing (Just f) e

exponent :: Parser Exponent
exponent = do
  _ <- char 'e'
  sign <-
    optionMaybe $
      do
        char '+' >> return Pos
        <|> (char '-' >> return Neg)
  Exponent sign <$> decimalLiteral

booleanLiteral :: Parser Literal
booleanLiteral =
  (string "true" >> return (BooleanLiteral True))
    <|> (string "false" >> return (BooleanLiteral False))
