module Syntax.Literal where

data Literal
  = IntegerLiteral Base String
  | BooleanLiteral Bool
  | FloatLiteral (Maybe Literal) (Maybe Literal) (Maybe Exponent)
  deriving (Eq, Show)

data Base
  = Dec
  | Bin
  | Oct
  | Hex
  deriving (Eq, Show)

data Sign = Pos | Neg
  deriving (Eq, Show)

data Exponent = Exponent (Maybe Sign) Literal
  deriving (Eq, Show)
