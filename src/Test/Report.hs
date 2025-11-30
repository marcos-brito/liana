module Test.Report where

import Test.Runner

data Summary = Summary
  { summTotal :: Int,
    summSucceded :: Int,
    summFailed :: Int,
    summTime :: Int
  }

instance Semigroup Summary where
  s1 <> s2 =
    Summary
      { summTotal = summTotal s1 + summTotal s2,
        summFailed = summFailed s1 + summFailed s2,
        summSucceded = summSucceded s1 + summSucceded s2,
        summTime = summTime s1 + summTime s2
      }

emptySummary :: Summary
emptySummary =
  Summary
    { summTotal = 0,
      summSucceded = 0,
      summFailed = 0,
      summTime = 0
    }

fromTestResult :: TestResult -> Summary
fromTestResult r =
  Summary
    { summTotal = 1,
      summSucceded = if not $ failed r then 1 else 0,
      summFailed = if failed r then 1 else 0,
      summTime = resultTime r
    }

data Report = Report
  { reportSumm :: Summary,
    reportTests :: [TestResult]
  }

instance Semigroup Report where
  r1 <> r2 =
    Report
      { reportSumm = reportSumm r1 <> reportSumm r2,
        reportTests = reportTests r1 <> reportTests r2
      }

emptyReport :: Report
emptyReport =
  Report
    { reportSumm = emptySummary,
      reportTests = []
    }

failed :: TestResult -> Bool
failed t = case resultStatus t of
  Sucess -> False
  Failure _ -> True

failedTests :: Report -> [TestResult]
failedTests r = filter failed (reportTests r)

addResult :: Report -> TestResult -> Report
addResult r t =
  r
    { reportSumm = reportSumm r <> fromTestResult t,
      reportTests = t : reportTests r
    }
