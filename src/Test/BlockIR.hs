module Test.BlockIR where

import Test.Lib

import qualified Parser.RegexParser as RegexParser
import qualified RegexIR.Eval as RegexEval
import qualified BlockIR.BlockTree as BlockTree
import Utils.PrettyPrint ((+--+), quoted)

import Data.Maybe (isNothing, fromJust)

testSuites = [
  blockTreeEvalSuite
  ]

-- BlockIR.BlockTree

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
    blockTreeEval = BlockTree.evalIRTree (BlockTree.buildIRTree regexOpTree)
    opTreeResult = RegexEval.getRegexMatch regexOpTree input
    testName = "regex " ++ (quoted regexDef) ++ ", input " ++ (quoted input) ++ " : evaluation results match"
    
