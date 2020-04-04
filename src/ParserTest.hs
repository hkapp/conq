module ParserTest where

import RegexParser
import RegexOpTree
import RegexEval

-- Basic test infrastructure

runAllTests :: IO ()
runAllTests = do
  allPassed <- runEachTest
  printGlobalRes allPassed

runEachTest :: IO Bool
runEachTest = foldl1 thenRun allTests


allTests :: [IO Bool]
allTests =
  [
  testMatch "a",
  testMatch "ab",
  testMatch "a|a",
  testMatch "a|a|a",
  testMatch "[a]",
  testNoMatch "[",
  testNoMatch "][",
  testNoMatch "[]",
  testNoMatch "|",
  testNoMatch "a|",
  testNoMatch "|a",
  testMatch "a|[a]",
  testNoMatch "[a|a]",
  testNoMatch "[[a]]",
  testNoMatch "a||a",
  testMatch "[a][b]"
  ]

thenRun :: IO Bool -> IO Bool -> IO Bool
thenRun prevAction nextAction = do
  prevRes <- prevAction
  nextRes <- nextAction
  return (prevRes && nextRes)

expect :: Bool -> Bool -> Bool
expect a b = a == b

testStr :: Bool -> String -> IO Bool
testStr expRes str = do
  let doesMatch = isValidRegex str
      testPass = expect expRes doesMatch
      treeRes = buildRegexOpTree str
  printTestRes testPass str doesMatch
  printTreeRes treeRes
  return testPass


testMatch :: String -> IO Bool
testMatch = testStr True

testNoMatch :: String -> IO Bool
testNoMatch = testStr False

printTestRes :: Bool -> String -> Bool -> IO ()

printTestRes testPass inputStr inputMatches =
  putStrLn ((passMsg testPass) ++ sep ++ (inputMsg inputStr) ++ sep ++ (matchMsg inputMatches))
    where sep = " "

passMsg :: Bool -> String
passMsg True = "[OK]"
passMsg False = "[XX]"

inputMsg :: String -> String
inputMsg str = '"' : str ++ '"' : []

matchMsg :: Bool -> String
matchMsg True = "is a valid regex"
matchMsg False = "is not a valid regex"

printGlobalRes :: Bool -> IO ()
printGlobalRes True = putStrLn "All tests passed"
printGlobalRes False = putStrLn "!! Some tests failed !!"

printTreeRes :: Show t => Maybe t -> IO ()
printTreeRes = maybe doNothing printTree
  where doNothing = return ()
        printTree tree = putStrLn ("> Generated tree: " ++ (show tree))
