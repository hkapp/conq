module RegexEval where

import RegexOpTree
import Parser
import Data.Set(member)

regexMatch :: RegexOpTree -> String -> Maybe String
regexMatch = parseString . equivalentParser

regexMatches :: RegexOpTree -> String -> Bool
regexMatches = isValidString . equivalentParser

equivalentParser :: RegexOpTree -> Parser String

equivalentParser (RegexString expectedString) inputString =
  let
    expectedLength = length expectedString
    (inputPrefix, inputSuffix) = splitAt expectedLength inputString
  in
    successIf (expectedString == inputPrefix) inputPrefix inputSuffix

equivalentParser (RegexCharClass charclass) (c:remainingInput) =
  successIf (member c charclass) [c] remainingInput

equivalentParser (RegexSequence opNodes) s = parseInSequence opParsers concat s
  where opParsers = map equivalentParser opNodes
    -- foldl fbab z opNodes
    -- where
      -- fbab b a = b
      -- fbab Failure a = Failure
      -- z = Success [] inputString

equivalentParser (RegexAlternative left right) s = parseAny [parseLeft, parseRight] s
  where
    parseLeft = equivalentParser left
    parseRight = equivalentParser right

successIf :: Bool -> t -> String -> ParseResult t
successIf True successTree remainingInput = Success successTree remainingInput
successIf False _ _ = Failure
