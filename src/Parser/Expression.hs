module Parser.Expression where

import Data.Functor.Identity (Identity)
import Parser.Base
import Parser.Literal
import Syntax.Expression
import Text.Parsec
import Text.Parsec.Expr




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
term = literalExpression

literalExpression :: Parser Expression
literalExpression = LiteralExpression <$> literal

expressionList :: Parser ExpressionList
expressionList = try labeledExpressionList <|> rawExpressionList

rawExpressionList :: Parser ExpressionList
rawExpressionList = RawExpressionList <$> commaSep1 expression

labeledExpressionList :: Parser ExpressionList
labeledExpressionList = LabeledExpressionList <$> commaSep1 labeledExpression

labeledExpression :: Parser LabeledExpression
labeledExpression = LabeledExpression <$> identifier <* symbol ":" <*> expression
