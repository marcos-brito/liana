module Parser.Base where

import Data.Char (isLetter)
import Syntax.Base
import Text.Parsec hiding (letter, newline)

unicodeLetter :: Parsec String st Char
unicodeLetter = satisfy isLetter

newline :: Parsec String st Char
newline = char '\x2028' <|> char '\x2029'

letter :: Parsec String st Char
letter = char '_' <|> unicodeLetter

decimalDigit :: Parsec String st Char
decimalDigit = oneOf ['0'..'9']

binaryDigit :: Parsec String st Char
binaryDigit = oneOf "01"

octalDigit :: Parsec String st Char
octalDigit = oneOf ['0'..'7']

hexDigit :: Parsec String st Char
hexDigit = oneOf ['0'..'9'] <|> oneOf ['a'..'f']

identifier :: Parsec String st Identifier
identifier = do
  first <- letter
  rest <- many (letter <|> decimalDigit)
  return (Identifier (first : rest))

qualifiedIdentifier :: Parsec String st [Identifier]
qualifiedIdentifier = sepBy identifier (char '.')

ty :: Parsec String st Type
ty = genericType <|> concreteType

typeList :: Parsec String st [Type]
typeList =  sepBy ty (char ',')

concreteType :: Parsec String st Type
concreteType = do
  ident <- qualifiedIdentifier
  params <- option [] (between (char '[') (char ']') typeList)
  kind <- optionMaybe typeKind
  return (Concrete ident params kind)

genericType :: Parsec String st Type
genericType = do
  ident <- between (char '[') (char ']') identifier
  kind <- optionMaybe typeKind
  return (Generic ident kind)

typeKind :: Parsec String st TypeKind
typeKind = (char '!' >> return Result) <|> (char '?' >> return Option)
