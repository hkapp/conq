module ParserTest where

import RegexParser
import RegexOpTree
import RegexEval
import Data.Bool(bool)

data Test = Test String Bool
data TestSuite = TestSuite String [Test]
type TestLib i o = String -> i -> o -> Test
data TestReport = TestReport Int Int

-- New test infrastructure

basicTestLib :: (i -> Bool) -> i -> String -> Test
basicTestLib f input name = Test name (f input)

basicAssertLib :: (Eq o) => (i -> o) -> o -> i -> String -> Test
basicAssertLib f expectedOutput = basicTestLib assertResult
  where assertResult input = (f input == expectedOutput)

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
  return result

-- Test suites

runAllTests :: IO ()
runAllTests = runSuite parserSuite

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

-- Printers

printTestResult :: Test -> IO ()
printTestResult (Test name result) =
  putStrLn ((testLinePrefix result) ++ sep ++ name)
    where sep = " "
          testLinePrefix True = "[OK]"
          testLinePrefix False = "[XX]"

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
