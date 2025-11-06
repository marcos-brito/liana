module Syntax.Declaration where

import Syntax.Base
import Syntax.Expression

newtype Module = Module [Declaration]

data Declaration
  = AttributeDeclaration AttributeKind QualifiedIdentifier (Maybe ExpressionList)
  deriving (Eq, Show)

data AttributeKind = AttrRegular | AttrTopLevel
  deriving (Eq, Show)
