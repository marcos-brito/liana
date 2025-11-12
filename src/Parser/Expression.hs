module Parser.Expression where

import Data.Functor.Identity (Identity)
import Parser.Base
import Parser.Helper
import Parser.Literal
import Syntax.Expression
import Text.Parsec
import Text.Parsec.Expr

statement :: Parser Statement
statement = (try assignmentStatement <|> expressionStatement) <* char ';'

expressionStatement :: Parser Statement
expressionStatement = ExpressionStatement <$> expression

assignmentStatement :: Parser Statement
assignmentStatement =
  AssignmentStatement
    <$> assignmentKind
    <*> identifier
    <*> optionMaybe colonTy
    <*> optionMaybe (symbol "=" *> expression)

assignmentKind :: Parser AssignmentKind
assignmentKind = Var <$ symbol "var" <|> Val <$ symbol "val"

table :: OperatorTable String () Identity Expression
table =
  [ [ unary "!" (UnaryExpression Not),
      unary "+" (UnaryExpression Pos),
      unary "-" (UnaryExpression Neg)
    ],
    [ binary "*" (BinaryExpression Mul) AssocLeft,
      binary "/" (BinaryExpression Div) AssocLeft,
      binary "%" (BinaryExpression Mod) AssocLeft
    ],
    [ binary "+" (BinaryExpression Plus) AssocLeft,
      binary "-" (BinaryExpression Minus) AssocLeft
    ],
    [ binary ">" (BinaryExpression Gt) AssocLeft,
      binary ">=" (BinaryExpression Ge) AssocLeft,
      binary "<" (BinaryExpression Lt) AssocLeft,
      binary "<=" (BinaryExpression Le) AssocLeft,
      binary "==" (BinaryExpression Eq) AssocLeft,
      binary "!=" (BinaryExpression NotEq) AssocLeft
    ],
    [binary "&&" (BinaryExpression And) AssocLeft],
    [binary "||" (BinaryExpression Or) AssocLeft],
    [binary ">>" (BinaryExpression Compose) AssocLeft],
    [binary "|>" (BinaryExpression Pipe) AssocLeft]
  ]

unary :: String -> (a -> a) -> Operator String () Identity a
unary op con = Prefix (reservedOp op >> return con)

binary :: String -> (a -> a -> a) -> Assoc -> Operator String () Identity a
binary op con = Infix (reservedOp op >> return con)

expression :: Parser Expression
expression = buildExpressionParser table term

term :: Parser Expression
term =
  compoundExpression
    <|> returnExpression
    <|> groupExpression
    <|> literalExpression

literalExpression :: Parser Expression
literalExpression = LiteralExpression <$> literal

compoundExpression :: Parser Expression
compoundExpression = arrowExpression <|> blockExpression

arrowExpression :: Parser Expression
arrowExpression = ArrowExpression <$> (reservedOp "->" *> expression)

blockExpression :: Parser Expression
blockExpression = BlockExpression <$> braces (many statement)

groupExpression :: Parser Expression
groupExpression = GroupExpression <$> parens expression

returnExpression :: Parser Expression
returnExpression = ReturnExpression <$> (symbol "return" >> expression)
expressionList :: Parser ExpressionList
expressionList = try labeledExpressionList <|> rawExpressionList

rawExpressionList :: Parser ExpressionList
rawExpressionList = RawExpressionList <$> commaSep1 expression

labeledExpressionList :: Parser ExpressionList
labeledExpressionList = LabeledExpressionList <$> commaSep1 labeledExpression

labeledExpression :: Parser LabeledExpression
labeledExpression = LabeledExpression <$> identifier <* symbol ":" <*> expression
