module Test.RegexIR where

import Test.Lib

import Parser.RegexParser (parseRegex)
import RegexIR.RegexOpTree (RegexOpTree(..))
import qualified RegexIR.Eval as RegexEval

import Utils.PrettyPrint ((+--+), quoted, )

import Data.Bool (bool)
import Data.Maybe (fromJust, isNothing)
import qualified Data.Set as Set

testSuites = [
  opTreeSuite,
  evalSuite
  ]

-- RegexIR.RegexOpTree

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

-- RegexIR.RegexEval

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
