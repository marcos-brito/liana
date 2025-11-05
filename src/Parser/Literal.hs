module Parser.Literal where

import Parser.Base
import Syntax.Literal
import Text.Parsec hiding (hexDigit)
import Prelude hiding (exponent)

literal :: Parsec String st Literal
literal = integerLiteral <|> floatLiteral <|> booleanLiteral

integerLiteral :: Parsec String st Literal
integerLiteral = decimalLiteral <|> binaryLiteral <|> octalLiteral <|> hexLiteral

decimalLiteral :: Parsec String st Literal
decimalLiteral = do
  first <- decimalDigit
  rest <- many $ do
    optional (char '_')
    decimalDigit
  return $ IntegerLiteral Dec (first : rest)

binaryLiteral :: Parsec String st Literal
binaryLiteral = baseInteger "0b" binaryDigit Bin

octalLiteral :: Parsec String st Literal
octalLiteral = baseInteger "0o" octalDigit Oct

hexLiteral :: Parsec String st Literal
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

floatLiteral :: Parsec String st Literal
floatLiteral = try fractionalFloatLiteral <|> try exponentFloatLiteral <|> dotFloatLiteral

fractionalFloatLiteral :: Parsec String st Literal
fractionalFloatLiteral = do
  i <- decimalLiteral
  _ <- char '.'
  f <- decimalLiteral
  e <- optionMaybe exponent
  return $ FloatLiteral (Just i) (Just f) e

exponentFloatLiteral :: Parsec String st Literal
exponentFloatLiteral = do
  i <- decimalLiteral
  e <- optionMaybe exponent
  return $ FloatLiteral (Just i) Nothing e

dotFloatLiteral :: Parsec String st Literal
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

booleanLiteral :: Parsec String st Literal
booleanLiteral =
  (string "true" >> return (BooleanLiteral True))
    <|> (string "false" >> return (BooleanLiteral False))
