module Test.Parser where

import Test.Lib

-- import Utils

import Parser.RegexParser (isValidRegex, parseRegex)
import qualified Parser.Combinatorics as Parser
import Utils.PrettyPrint ((+--+), quoted)

import Data.Bool(bool)
import qualified Data.Char as Char

testSuites = [
  parserSuite,
  regexParserSuite
  ]

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
