module Syntax.Declaration where

import Syntax.Base
import Syntax.Expression

newtype Module = Module [Declaration]

data Declaration
  = FunctionDeclaration Identifier [Parameter] (Maybe Type) [Bound] (Maybe Expression)
  | AttributeDeclaration AttributeKind QualifiedIdentifier (Maybe ExpressionList)
  deriving (Eq, Show)

data AttributeKind = AttrRegular | AttrTopLevel
  deriving (Eq, Show)

data Bound = Bound Identifier [Type]
  deriving (Eq, Show)

data Parameter = Parameter (Maybe AssignmentKind) Identifier Type
  deriving (Eq, Show)
