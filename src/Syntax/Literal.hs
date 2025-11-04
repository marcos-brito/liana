module Syntax.Literal where

data Literal = IntegerLiteral IntegerLiteral

data IntegerLiteral
  = DecimalLiteral String
  | BinaryLiteral String
  | OctalLiteral String
  | HexLiteral String
  deriving (Eq, Show)
