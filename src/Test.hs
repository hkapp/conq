module Test where

import RegexParser(isValidRegex, parseRegex)
import RegexOpTree
import RegexEval
import Data.Bool(bool)
import Data.Set as Set (fromList)
import Data.Maybe(isNothing, fromJust)
import qualified Data.Char as Char
import qualified Parser
import StateMachine

data TestResult = Success | Failure (Maybe String)
data Test = Test String TestResult
data TestSuite = TestSuite String [Test]
type TestLib i o = String -> i -> o -> Test
data TestReport = TestReport Int Int

-- Exported 'run' utilities

runAllTests :: IO ()
runAllTests = do
  runSuite parserSuite
  runSuite regexParserSuite
  runSuite opTreeSuite
  runSuite evalSuite
  runSuite dummyPrintCodeSuite

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
printSuiteStart name = do
  putStrLn (replicate 20 '-')
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

parserSuite = TestSuite "Parser" (concat [
  parserRepeatUntilFailureTests,
  parserParseInSequenceTests
  ])

parserRepeatUntilFailureTests = allValid [
  "abc",
  ""
  ]
  where
    allValid = map validStringTest
    validStringTest s = parserTest testedParser funName (Just s) s
    testedParser = Parser.repeatUntilFailure (Parser.parseOneChar Char.isLetter)
    funName = "repeatUntilFailure"

parserParseInSequenceTests = [
  idParse "abc"
  ]
  where
    idParse s = parserTest (parserFor s) funName (Just s) s
    parserFor s = Parser.parseInSequence (fmap Parser.exactChar s)
    funName = "parseInSequence"

parserTest parser funName expectedOutput inputString =
  basicAssertLib testedFun expectedOutput inputString testName
  where
    testedFun = Parser.parseString parser
    testName = (wrapWithQuotes inputString) *- expectedOutputText *- commonSuffix
    sep = " "
    wrapWithQuotes str = '"' : str ++ '"' : []
    expectedOutputText = maybe "fails to parse" (const "parses properly") expectedOutput
    commonSuffix = "under" *- funName
    (*-) :: String -> String -> String
    pre *- post = pre ++ sep ++ post


regexParserSuite = TestSuite "RegexParser" [
  valid "a",
  valid "ab",
  valid "a|b",
  valid "a|b|c",
  valid "[a]",
  invalid "[",
  invalid "][",
  invalid "[]",
  invalid "|",
  invalid "a|",
  invalid "|a",
  valid "b|[a]",
  invalid "[a|c]",
  invalid "[[a]]",
  invalid "a||b",
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
    isOrIsNot = bool "is not" "is"
    commonSuffix = "a valid regex"


opTreeSuite = TestSuite "RegexOpTree" [
  "a"      |-> a,
  "ab"     |-> RegexString "ab",
  "a|b"    |-> RegexAlternative a b,
  "a|b|c"  |-> RegexAlternative a (RegexAlternative b c),
  "[a]"    |-> charclass "a",
  invalid "[",
  invalid "][",
  invalid "[]",
  invalid "|",
  invalid "a|",
  invalid "|a",
  "a|[a]"  |-> RegexAlternative a (charclass "a"),
  invalid "[a|a]",
  invalid "[[a]]",
  invalid "a||a",
  "[a][b]" |-> RegexSequence [charclass "a", charclass "b"]
  ]
  where (|->) input expectedOutput = assertResultingTree (Just expectedOutput) input
        invalid = assertResultingTree (Nothing :: Maybe RegexOpTree)
        a = RegexString "a"
        b = RegexString "b"
        c = RegexString "c"
        charclass = RegexCharClass . Set.fromList

assertResultingTree expectedResult inputString =
  basicAssertLib parseRegex expectedResult inputString testName
  where
    testName = (wrapWithQuotes inputString) ++ sep ++ testText
    wrapWithQuotes str = '"' : str ++ '"' : []
    sep = " "
    testText = bool "produces the expected tree" "doesn't produce any tree" (isNothing expectedResult)


evalSuite = TestSuite "RegexEval" (concat [
  usingRegex "a" [
    matchAll "a",
    invalid "b"
    ],
  usingRegex "ab" [
    matchAll "ab",
    invalid "a",
    invalid "b",
    "abb" `matches` "ab",
    "aab" `matches` "ab"
    ],
  usingRegex "a|b" [
    "a" `matches` "a",
    "b" `matches` "b",
    invalid "c",
    "cab" `matches` "a"
    ],
  usingRegex "a|b|c" [
    "a" `matches` "a",
    "b" `matches` "b",
    "c" `matches` "c",
    "0a" `matches` "a",
    "1c" `matches` "c",
    invalid "01"
    ],
  usingRegex "[a]" [
    matchAll "a",
    invalid "b"
    ],
  usingRegex "a|[a]" [
    matchAll "a",
    invalid "b"
    ],
  usingRegex "[a][b]" [
    matchAll "ab",
    invalid "a",
    invalid "b",
    "abb" `matches` "ab",
    "aab" `matches` "ab"
    ]
  ])
  where
    usingRegex :: String -> [(String -> Test)] -> [Test]
    usingRegex regexDef = map ($ regexDef)
    matchAll inputString = inputString `matches` inputString
    matches inputString expectedMatch = evalTest (Just expectedMatch) inputString
    invalid = evalTest Nothing

evalTest expectedResult inputString regexDef =
  basicAssertLib regexMatch expectedResult inputString testName
  where
    regex = fromJust (parseRegex regexDef)
    regexMatch = getRegexMatch regex
    testName = regexPrefix ++ sep ++ (wrapWithQuotes inputString) ++ sep ++ (matchText expectedResult)
    regexPrefix = "regex " ++ (wrapWithQuotes regexDef) ++ " ::"
    wrapWithQuotes str = '"' : str ++ '"' : []
    sep = " "
    matchText (Just match) = "matches " ++ (wrapWithQuotes match)
    matchText Nothing = "doesn't match"


dummyPrintCodeSuite = TestSuite "DummyPrintCode" [dummyPrintTestCode "abc", dummyPrintTestCode "a|bc"]

dummyPrintTestCode regexDef =
  basicAssertLib printCode expectedResult regexDef testName
  where
    printCode = fmap (printC "valid" "invalid") . parseRegex
    expectedResult = Just ""
    testName = "code for " ++ (wrapWithQuotes regexDef)
    wrapWithQuotes str = '"' : str ++ '"' : []
    sep = " "
