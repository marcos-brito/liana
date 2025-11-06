module Parser.Declaration where

import Parser.Base
import Parser.Expression (expressionList)
import Parser.Helper
import Syntax.Declaration
import Text.Parsec

mod :: Parser Module
mod = Module <$> many declaration

declaration :: Parser Declaration
declaration = attributeDeclaration

attributeDeclaration :: Parser Declaration
attributeDeclaration =
  AttributeDeclaration
    <$> attributeKind
    <*> qualifiedIdentifier
    <*> optionMaybe (parens expressionList)

attributeKind :: Parser AttributeKind
attributeKind =
  try (AttrTopLevel <$ string "@!") <|> (AttrRegular <$ string "@")
