module Diagnostic where

import Data.Text as T

type Code = Text

data Severity
  = Error
  | Warn
  | Advice

data Label = Label
  { labelMessage :: Text,
    labelLine :: Int,
    labelStart :: Int,
    labelLenght :: Int
  }

data Diagnostic = Diagnostic
  { diagCode :: Text,
    diagSeverity :: Severity,
    diagMessage :: Text,
    diagSource :: Maybe Text,
    diagHelp :: Maybe Text,
    diagLabels :: [Label]
  }
