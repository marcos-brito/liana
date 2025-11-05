module Parser.Expression where

import Data.Functor.Identity (Identity)
import Parser.Base
import Parser.Literal
import Syntax.Expression
import Text.Parsec
import Text.Parsec.Expr




table :: OperatorTable String st Identity Expression
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

unary :: String -> (a -> a) -> Operator String st Identity a
unary op con = Prefix (string op >> return con)

binary :: String -> (a -> a -> a) -> Assoc -> Operator String st Identity a
binary op con = Infix (string op >> return con)

expression :: Parsec String st Expression
expression = buildExpressionParser table term

term :: Parsec String st Expression
term = literalExpression

literalExpression :: Parsec String st Expression
literalExpression = LiteralExpression <$> literal

expressionList :: Parsec String st ExpressionList
expressionList = try labeledExpressionList <|> rawExpressionList

rawExpressionList :: Parsec String st ExpressionList
rawExpressionList = RawExpressionList <$> sepBy expression (char ',')

labeledExpressionList :: Parsec String st ExpressionList
labeledExpressionList = LabeledExpressionList <$> sepBy labeledExpression (char ',')

labeledExpression :: Parsec String st LabeledExpression
labeledExpression = do
  ident <- identifier
  _ <- char ':'
  expr <- expression
  return $ LabeledExpression ident expr
