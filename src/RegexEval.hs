module RegexEval where

import RegexOpTree
import Parser
import Data.Set(member)

getRegexMatch :: RegexOpTree -> String -> Maybe String
getRegexMatch = parseString . equivalentParser

regexMatches :: RegexOpTree -> String -> Bool
regexMatches = isValidString . equivalentParser

equivalentParser :: RegexOpTree -> Parser String

equivalentParser (RegexString expectedString) = parseInSequence (fmap exactChar expectedString)

equivalentParser (RegexCharClass charclass) = fmap return (parseOneChar (`member` charclass))

equivalentParser (RegexSequence opNodes) = mapParser concat (parseInSequence opParsers)
  where opParsers = map equivalentParser opNodes

equivalentParser (RegexAlternative left right) = parseAny [parseLeft, parseRight]
  where
    parseLeft = equivalentParser left
    parseRight = equivalentParser right
