module RegexIR.Eval where

import RegexIR.RegexOpTree

import Data.Set(Set, member)
import Parser.Combinatorics as Parser
import Utils.Prelude
import Utils.Set

-- We don't use 'parseString' because a regex match can be incomplete
getRegexMatch :: RegexOpTree -> String -> Maybe String
getRegexMatch = partiallyParseString . equivalentParser

-- We don't use 'isValidString' because a regex match can be incomplete
regexMatches :: RegexOpTree -> String -> Bool
regexMatches = partiallyValidString . equivalentParser

equivalentParser :: RegexOpTree -> Parser String
equivalentParser = canStartAnywhere . evalParser

evalParser :: RegexOpTree -> Parser String

evalParser (RegexString expectedString) = exactPrefix expectedString

evalParser (RegexCharClass charclass) = parseOneChar (belongsTo charclass) <&> pure

evalParser (RegexSequence opNodes) = parseInSequence opParsers <&> concat
  where opParsers = map evalParser opNodes

evalParser (RegexAlternative left right) = parseAny [parseLeft, parseRight]
  where
    parseLeft = evalParser left
    parseRight = evalParser right
