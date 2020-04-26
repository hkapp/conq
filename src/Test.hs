module Test where


import Utils

import RegexParser(isValidRegex, parseRegex)
import RegexOpTree (RegexOpTree(..))
import qualified RegexOpTree
import qualified RegexEval
import qualified Parser
import qualified BlockIR

import Data.Bool(bool)
import Data.Set as Set (fromList)
import Data.Maybe(isNothing, fromJust)
import qualified Data.Char as Char
import Data.Foldable (foldl')


data TestResult = Success | Failure (Maybe String)
data Test = Test String TestResult
data TestSuite = TestSuite String [Test]
type TestLib i o = String -> i -> o -> Test
data TestReport = TestReport Int Int

-- Exported 'run' utilities

runAllTests :: IO ()
runAllTests = do
  allSuitesSucceeded <- traverseWhile runSuite allSuites
  if allSuitesSucceeded
    then return ()
    else putStrLn "Failure! Stopping the test execution..."

allSuites :: [TestSuite]
allSuites = [
  parserSuite,
  regexParserSuite,
  opTreeSuite,
  evalSuite,
  blockTreeEvalSuite
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

-- Text utilities

sep :: String
sep = " "

quoted :: String -> String
quoted str = '"' : str ++ '"' : []

(+--+) :: String -> String -> String
prefix +--+ suffix = prefix ++ sep ++ suffix

-- Test suites

-- Parser suite

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
    testName = (quoted inputString) +--+ expectedOutputText +--+ commonSuffix
    expectedOutputText = maybe "fails to parse" (const "parses properly") expectedOutput
    commonSuffix = "under" +--+ funName

-- RegexParser suite

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
  valid "[a][b]",
  valid "[a]|b",
  valid "a|[b]",
  valid "[a]b",
  valid "[a]b|[c]"
  ]
  where valid = validRegexTest True
        invalid = validRegexTest False

validRegexTest expectedValidity inputString =
  basicAssertLib isValidRegex expectedValidity inputString testName
  where
    testName = (quoted inputString) +--+ (isOrIsNot expectedValidity) +--+ commonSuffix
    isOrIsNot = bool "is not" "is"
    commonSuffix = "a valid regex"

-- RegexOpTree suite

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
    testName = (quoted inputString) +--+ testText
    testText = bool "produces the expected tree" "doesn't produce any tree" (isNothing expectedResult)

-- RegexEval suite

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
    regexMatch = RegexEval.getRegexMatch regex
    testName = regexPrefix +--+ (quoted inputString) +--+ (matchText expectedResult)
    regexPrefix = "regex " ++ (quoted regexDef) ++ " ::"
    matchText (Just match) = "matches " ++ (quoted match)
    matchText Nothing = "doesn't match"

-- BlockIR suite

blockTreeEvalSuite = TestSuite "BlockTreeEval" (generateTestMatrix
  (regexes [
    "a",
    "ab",
    "a|b",
    "a|b|c",
    "[a]",
    "a|[a]",
    "[a][b]"
    ])
  (inputs [
    "a",
    "b",
    "ab",
    "abb",
    "aab",
    "c",
    "cab",
    "0a",
    "1c",
    "01"
    ])
  )
  where
    generateTestMatrix rs is =
      [assertEvalEquivalence regex input | regex <- rs, input <- is]
    regexes = id
    inputs = id

assertEvalEquivalence regexDef input =
  basicAssertLib blockTreeEval opTreeResult input testName
  where
    regexOpTree = fromJust (RegexParser.parseRegex regexDef)
    blockTreeEval = BlockIR.evalIRTree (BlockIR.buildIRTree regexOpTree)
    opTreeResult = RegexEval.getRegexMatch regexOpTree input
    testName = "regex " ++ (quoted regexDef) ++ ", input " ++ (quoted input) ++ " : evaluation results match"
    
