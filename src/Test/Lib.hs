module Test.Lib where

data TestResult = Success | Failure (Maybe String)
data Test = Test String TestResult
data TestSuite = TestSuite String [Test]
type TestLib i o = String -> i -> o -> Test
data TestReport = TestReport Int Int

-- Utilities to define tests

basicTestLib :: (i -> TestResult) -> i -> String -> Test
basicTestLib f input name = Test name (f input)

basicAssertLib :: (Eq o, Show o) => (i -> o) -> o -> i -> String -> Test
basicAssertLib f expectedOutput = basicTestLib assertResult
  where assertResult input = detailedTestResult assertFailureText testPassed
          where testOutput = f input
                testPassed = (testOutput == expectedOutput)
                assertFailureText = unlines [
                  "> Expected: " ++ (show expectedOutput),
                  "> Found:    " ++ (show testOutput)
                  ]

basicTestResult :: Bool -> TestResult
basicTestResult True = Success
basicTestResult False = Failure Nothing

detailedTestResult :: String -> Bool -> TestResult
detailedTestResult _ True = Success
detailedTestResult explanation False = Failure (Just explanation)
