{-# LANGUAGE OverloadedStrings #-}

module Test.Runner where

import qualified Data.Text as T
import Diagnostic
import Driver

data Test = Test
  { testName :: T.Text,
    testDesc :: T.Text,
    testChecks :: [Check],
    testFile :: T.Text
  }

data TestResult = TestResult
  { resultTestName :: T.Text,
    resultTestFile :: T.Text,
    resultTime :: Int,
    resultStatus :: Status,
    resultChecks :: [CheckResult]
  }

data CheckResult = CheckResult Status Check

data Status = Sucess | Failure (Maybe Diagnostic)

data Check
  = DiagCode T.Text
  | DiagCount Int
  | DiagPattern T.Text
  | Compiles Bool

data AstSource = String | Module

run :: Test -> T.Text -> TestResult
run test src =
  newTestResult test results
  where
    session = compile newSession (T.unpack $ testFile test) src
    statuses = map (runCheck session) (testChecks test)
    results = zipWith CheckResult statuses (testChecks test)

newTestResult :: Test -> [CheckResult] -> TestResult
newTestResult t r =
  TestResult
    { resultTestName = testName t,
      resultTestFile = testFile t,
      resultTime = 0,
      resultStatus = collect $ map (\(CheckResult s _) -> s) r,
      resultChecks = r
    }

collect :: [Status] -> Status
collect = foldr step Sucess
  where
    step (Failure _) _ = Failure Nothing
    step Sucess _ = Sucess

runCheck :: Session -> Check -> Status
runCheck sess (DiagCode code) =
  if any (/= code) codes
    then Sucess
    else Failure Nothing
  where
    codes = map diagCode (sessionDiags sess)
runCheck sess (DiagCount count) =
  if sessCount == count
    then Sucess
    else Failure Nothing
  where
    sessCount = length $ sessionDiags sess
runCheck sess (DiagPattern patt) =
  if any (T.isInfixOf patt) messages
    then Sucess
    else Failure Nothing
  where
    messages = map diagMessage (sessionDiags sess)
runCheck sess (Compiles True) =
  if null $ sessionDiags sess
    then Sucess
    else Failure Nothing
runCheck sess (Compiles False) =
  if not $ null $ sessionDiags sess
    then Sucess
    else Failure Nothing
