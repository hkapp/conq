module ParserTest where

import RegexParser(isValidRegex)
import RegexOpTree
import RegexEval
import Data.Bool(bool)

data TestResult = Success | Failure (Maybe String)
data Test = Test String TestResult
data TestSuite = TestSuite String [Test]
type TestLib i o = String -> i -> o -> Test
data TestReport = TestReport Int Int

-- Exported 'run' utilities

runAllTests :: IO ()
runAllTests = runSuite parserSuite

runSuite :: TestSuite -> IO ()
runSuite (TestSuite name tests) = do
  printSuiteStart name
  report <- foldl runAndBuildReport initReport tests
  printSuiteEnd name report
  where
    initReport = return (TestReport 0 0)
    runAndBuildReport report nextTest = do
      TestReport success failures <- report
      testPassed <- runTest nextTest
      if testPassed
        then return (TestReport (success + 1) failures)
        else return (TestReport success (failures + 1))

runTest :: Test -> IO (Bool)
runTest test@(Test name result) = do
  printTestResult test
  return (isSuccess result)

isSuccess :: TestResult -> Bool
isSuccess Success = True
isSuccess (Failure _) = False

-- Printers

printTestResult :: Test -> IO ()
printTestResult (Test name result) = do
  putStrLn ((testLinePrefix (isSuccess result)) ++ sep ++ name)
  printExplanation result
    where sep = " "
          testLinePrefix True = "[OK]"
          testLinePrefix False = "[XX]"
          printExplanation (Failure (Just explanation)) = putStrLn explanation
          printExplanation _ = return ()

printSuiteStart :: String -> IO ()
printSuiteStart name =
  putStrLn ("Suite " ++ name ++ ":")

printSuiteEnd :: String -> TestReport -> IO ()
printSuiteEnd name (TestReport success failures) =
  putStrLn (linePrefix ++ sep ++ reportString)
  where
    linePrefix = "Suite " ++ name ++ ":"
    sep = " "
    reportString
      | (failures == 0) = "All tests passed"
      | otherwise = "TESTS FAILED: " ++ (show failures) ++ " (tests passed: " ++ (show success) ++ ")"

printTreeRes :: Show t => Maybe t -> IO ()
printTreeRes = maybe doNothing printTree
  where doNothing = return ()
        printTree tree = putStrLn ("> Generated tree: " ++ (show tree))

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

-- Test suites

parserSuite = TestSuite "Parser" [
  valid "a",
  valid "ab",
  valid "a|a",
  valid "a|a|a",
  valid "[a]",
  invalid "[",
  invalid "][",
  invalid "[]",
  invalid "|",
  invalid "a|",
  invalid "|a",
  valid "a|[a]",
  invalid "[a|a]",
  invalid "[[a]]",
  invalid "a||a",
  valid "[a][b]"
  ]
  where valid = validRegexTest True
        invalid = validRegexTest False

validRegexTest expectedValidity inputString =
  basicAssertLib isValidRegex expectedValidity inputString testName
  where
    testName = (wrapWithQuotes inputString) ++ sep ++ (isOrIsNot expectedValidity) ++ sep ++ commonSuffix
    sep = " "
    wrapWithQuotes str = '"' : str ++ '"' : []
    isOrIsNot = bool "is" "is not"
    commonSuffix = "a valid regex"
