module Parser.Base where

import Data.Char (isLetter)
import Syntax.Base
import Text.Parsec hiding (letter, newline)

type Parser a = Parsec String () a

unicodeLetter :: Parser Char
unicodeLetter = satisfy isLetter

newline :: Parsec String st Char
newline = char '\x2028' <|> char '\x2029'

letter :: Parser Char
letter = char '_' <|> unicodeLetter

decimalDigit :: Parser Char
decimalDigit = oneOf ['0' .. '9']

binaryDigit :: Parser Char
binaryDigit = oneOf "01"

octalDigit :: Parser Char
octalDigit = oneOf ['0' .. '7']

hexDigit :: Parser Char
hexDigit = oneOf ['0' .. '9'] <|> oneOf ['a' .. 'f']

identifier :: Parser Identifier
identifier = do
  first <- letter
  rest <- many (letter <|> decimalDigit)
  return (Identifier (first : rest))

qualifiedIdentifier :: Parser [Identifier]
qualifiedIdentifier = sepBy identifier (char '.')

ty :: Parser Type
ty = genericType <|> concreteType

typeList :: Parser [Type]
typeList = sepBy ty (char ',')

concreteType :: Parser Type
concreteType = do
  ident <- qualifiedIdentifier
  params <- option [] (between (char '[') (char ']') typeList)
  kind <- optionMaybe typeKind
  return (Concrete ident params kind)

genericType :: Parser Type
genericType = do
  ident <- between (char '[') (char ']') identifier
  kind <- optionMaybe typeKind
  return (Generic ident kind)

typeKind :: Parser TypeKind
typeKind = (char '!' >> return Result) <|> (char '?' >> return Option)
