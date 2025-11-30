{-# LANGUAGE OverloadedStrings #-}

module Test.Pretty (renderIo) where

import Diagnostic.Pretty (diag)
import Prettyprinter as P
import Prettyprinter.Render.Terminal
import System.IO (stdout)
import Test.Report
import Test.Runner

renderIo :: Report -> IO ()
renderIo r = renderIO stdout (layoutPretty defaultLayoutOptions (report r))

report :: Report -> Doc AnsiStyle
report r =
  vcat
    [ vcat $ map testResult (reportTests r),
      line,
      vcat $ map failedTest (failedTests r),
      summary (reportSumm r)
    ]

summary :: Summary -> Doc AnsiStyle
summary summ =
  vcat
    [ annotate (color Blue) ("Ran " <> pretty (summTotal summ) <> "tests"),
      nest
        4
        ( vcat
            [ annotate
                (color Green)
                ( pretty
                    (summSucceded summ)
                    <> "succeded"
                ),
              annotate
                (color Red)
                ( pretty
                    (summSucceded summ)
                    <> "failed"
                )
            ]
        ),
      annotate (color Magenta) "finished in" <> pretty (summTime summ)
    ]

testResult :: TestResult -> Doc AnsiStyle
testResult r =
  vcat
    [ status (resultStatus r)
        <+> pretty (resultTestFile r)
        <+> parens (pretty $ resultTime r),
      nest 4 (vcat $ map checkResult (resultChecks r))
    ]

failedTest :: TestResult -> Doc AnsiStyle
failedTest r =
  vcat
    [ status (resultStatus r)
        <+> pretty (resultTestFile r),
      nest 4 (vcat diags)
    ]
  where
    diags = [diag d | CheckResult (Failure (Just d)) _ <- resultChecks r]

status :: Status -> Doc AnsiStyle
status Sucess = annotate (color Green) "Succeced"
status (Failure _) = annotate (color Red) "Failed"

checkResult :: CheckResult -> Doc AnsiStyle
checkResult (CheckResult s c) = status s <+> check c

check :: Check -> Doc AnsiStyle
check (DiagCode code) = "Diagnostic code" <+> pretty code
check (DiagCount count) = "Diagnostic count" <+> pretty count
check (DiagPattern patt) = "Diagnostic pattern" <+> pretty patt
check (Compiles b) = "Compiles" <+> pretty b
