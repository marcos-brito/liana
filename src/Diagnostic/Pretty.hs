{-# LANGUAGE OverloadedStrings #-}

module Diagnostic.Pretty (render, renderIo, diag) where

import qualified Data.Text as T
import Diagnostic
import Prettyprinter as P
import Prettyprinter.Render.Terminal
import System.IO (stdout)

render :: Diagnostic -> T.Text
render d = renderStrict $ layoutPretty defaultLayoutOptions (diag d)

renderIo :: Diagnostic -> IO ()
renderIo d = renderIO stdout (layoutPretty defaultLayoutOptions (diag d))

severity :: Severity -> Doc AnsiStyle
severity Error = annotate (color Red) "error"
severity Warn = annotate (color Yellow) "warn"
severity Advice = annotate (color Blue) "advice"

code :: T.Text -> Doc AnsiStyle
code c = parens $ annotate (color Magenta) (pretty c)

help :: Diagnostic -> Doc AnsiStyle
help d =
  case diagHelp d of
    Just h -> annotate (color Green) "help" <> ":" <+> pretty h
    Nothing -> emptyDoc

diag :: Diagnostic -> Doc AnsiStyle
diag d =
  vcat
    [ severity (diagSeverity d)
        <> code (diagCode d)
        <> ":"
        <+> pretty (diagMessage d),
      help d
    ]
