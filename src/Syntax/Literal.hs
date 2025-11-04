module Syntax.Literal where

data Literal = IntegerLiteral IntegerLiteral

data IntegerLiteral
  = DecimalLiteral String
  | BinaryLiteral String
  | OctalLiteral String
  | HexLiteral String
  deriving (Eq, Show)

data BooleanLiteral = True | False
  deriving (Eq, Show)

data FloatLiteral = FloatLiteral
  { intPart :: Maybe IntegerLiteral,
    fracPart :: Maybe IntegerLiteral,
    expo :: Maybe Exponent
  }
  deriving (Show, Eq)

data Sign = Pos | Neg
  deriving (Eq, Show)

data Exponent = Exponent (Maybe Sign) IntegerLiteral
  deriving (Eq, Show)
