module Test.TestMain where

import Test.Lib


import qualified Test.Parser as Parser
import qualified Test.RegexIR as RegexIR
import qualified Test.BlockIR as BlockIR

import Utils.Foldable
import Utils.Prelude
import Utils.PrettyPrint ((+--+), quoted)

import Data.Foldable (foldl')


-- Exported 'run' utilities

runAllTests :: IO ()
runAllTests = do
  allSuitesSucceeded <- traverseWhile runSuite allSuites
  if allSuitesSucceeded
    then return ()
    else putStrLn "Failure! Stopping the test execution..."

allSuites :: [TestSuite]
allSuites = mconcat [
  Parser.testSuites,
  RegexIR.testSuites,
  BlockIR.testSuites
  ]

runSuite :: TestSuite -> IO (Bool)
runSuite (TestSuite name tests) = do
  printSuiteStart name
  report <- foldl' runAndBuildReport initReport tests
  printSuiteEnd name report
  return (allSucceeded report)
  where
    initReport = return (TestReport 0 0)
    runAndBuildReport getCurrentReport nextTest = do
      report <- getCurrentReport
      testResult <- runTest nextTest
      return (updateReport report testResult)

runTest :: Test -> IO (TestResult)
runTest test@(Test name result) = do
  printTestResult test
  return result

isSuccess :: TestResult -> Bool
isSuccess Success = True
isSuccess (Failure _) = False

allSucceeded :: TestReport -> Bool
allSucceeded (TestReport _ f) = (f == 0)

updateReport :: TestReport -> TestResult -> TestReport
updateReport (TestReport s f) Success = TestReport (s + 1) f
updateReport (TestReport s f) (Failure _) = TestReport s (f + 1)

-- Printers

printTestResult :: Test -> IO ()
printTestResult (Test name result) = do
  putStrLn ((testLinePrefix (isSuccess result)) +--+ name)
  printExplanation result
    where testLinePrefix True = "[OK]"
          testLinePrefix False = "[XX]"
          printExplanation (Failure (Just explanation)) = putStrLn explanation
          printExplanation _ = return ()

printSuiteStart :: String -> IO ()
printSuiteStart name = do
  putStrLn (replicate 20 '-')
  putStrLn ("Suite " ++ name ++ ":")

printSuiteEnd :: String -> TestReport -> IO ()
printSuiteEnd name (TestReport success failures) =
  putStrLn (linePrefix +--+ reportString)
  where
    linePrefix = "Suite " ++ name ++ ":"
    reportString
      | (failures == 0) = "All tests passed"
      | otherwise = "TESTS FAILED: " ++ (show failures) ++ " (tests passed: " ++ (show success) ++ ")"

printTreeRes :: Show t => Maybe t -> IO ()
printTreeRes = maybe doNothing printTree
  where printTree tree = putStrLn ("> Generated tree: " ++ (show tree))
