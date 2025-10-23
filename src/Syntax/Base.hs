module Syntax.Base where

newtype Identifier = Identifier String
  deriving (Eq, Show)

type QualifiedIdentifier = [Identifier]

data Type
  = Concrete QualifiedIdentifier [Type] (Maybe TypeKind)
  | Generic Identifier (Maybe TypeKind)
  deriving (Eq, Show)

data TypeKind = Result | Option
  deriving (Eq, Show)
