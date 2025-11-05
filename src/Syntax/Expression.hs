module Syntax.Expression where

import Syntax.Literal

data Expression
  = UnaryExpression UnaryOperator Expression
  | BinaryExpression BinaryOperator Expression Expression
  | LiteralExpression Literal
  deriving (Eq, Show)

  deriving (Eq, Show)
data UnaryOperator = Not | Neg | Pos
  deriving (Eq, Show)

data BinaryOperator
  = Plus
  | Minus
  | Mul
  | Div
  | And
  | Or
  | Gt
  | Ge
  | Lt
  | Le
  | Eq
  | NotEq
  | Pipe
  | Compose
  | Mod
  deriving (Eq, Show)
