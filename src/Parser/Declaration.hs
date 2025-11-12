module Parser.Declaration where

import Parser.Base
import Parser.Expression
import Parser.Helper
import Syntax.Base
import Syntax.Declaration
import Text.Parsec

mod :: Parser Module
mod = Module <$> many declaration

declaration :: Parser Declaration
declaration = attributeDeclaration <|> functionDeclaration

attributeDeclaration :: Parser Declaration
attributeDeclaration =
  AttributeDeclaration
    <$> attributeKind
    <*> qualifiedIdentifier
    <*> optionMaybe (parens expressionList)

attributeKind :: Parser AttributeKind
attributeKind =
  try (AttrTopLevel <$ string "@!") <|> (AttrRegular <$ string "@")

functionDeclaration :: Parser Declaration
functionDeclaration =
  FunctionDeclaration
    <$> (reserved "fun" *> identifier)
    <*> parens parameterList
    <*> optionMaybe colonTy
    <*> option [] whereClause
    <*> (optionMaybe compoundExpression <|> (Nothing <$ symbol ";"))

parameter :: Parser Parameter
parameter =
  Parameter
    <$> optionMaybe assignmentKind
    <*> identifier
    <*> colonTy

parameterList :: Parser [Parameter]
parameterList = commaSep parameter

whereClause :: Parser [Bound]
whereClause = reserved "where" *> brackets boundList

boundList :: Parser [Bound]
boundList = commaSep bound

bound :: Parser Bound
bound = Bound <$> identifier <* symbol ":" <*> sepBy ty (symbol "+")
