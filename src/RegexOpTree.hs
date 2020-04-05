module RegexOpTree where

import RegexParser
import Parser
import Data.Set(Set, fromList)

data RegexOpTree =
  RegexString String
  | RegexCharClass (Set Char)
  | RegexSequence [RegexOpTree]
  | RegexAlternative RegexOpTree RegexOpTree
  deriving (Show, Eq)


instance RegexTreeBuilder RegexOpTree where
  buildCharNode c = RegexString [c]
  buildCharClassNode options = RegexCharClass (fromList (map extractChar options))
    where extractChar (RegexString (c:[])) = c
  buildConcatNode opNodes = normalizedRegexSequence opNodes
  buildAltNode = RegexAlternative

buildRegexOpTree :: String -> Maybe RegexOpTree
buildRegexOpTree = parseString regexParser

normalizedRegexSequence :: [RegexOpTree] -> RegexOpTree
normalizedRegexSequence = buildNormalized . foldr normalize []
  where buildNormalized (node:[]) = node
        buildNormalized nodes = RegexSequence nodes
        normalize (RegexString prefix) ((RegexString suffix):moreNodes) = (RegexString (prefix ++ suffix)):moreNodes
        normalize newNode someNodes = newNode:someNodes
