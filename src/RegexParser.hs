{-|
 Module : RegexParser
 Description : Parser for Pearl regular expressions.
               Based on the Parser module.
               Can generate any AST as a result using the type class RegexTreeBuilder.
-}
module RegexParser where

import Parser

-- How to restrict to only those types that have a TreeBuilder?
-- e.g. RegexTreeBuilder t => Parser t
-- -> not possible (https://stackoverflow.com/questions/22945348/why-class-constraint-in-type-synonym-needs-rankntypes)
type RegexParser t = Parser t

-- data RegexTreeBuilder t = RegexTreeBuilder {
class RegexTreeBuilder t where
  buildCharNode      :: Char -> t
  buildCharClassNode :: [t] -> t
  buildConcatNode    :: [t] -> t
  buildAltNode       :: t -> t -> t


instance RegexTreeBuilder () where
  buildCharNode _      = ()
  buildCharClassNode _ = ()
  buildConcatNode _    = ()
  buildAltNode _ _     = ()



-- Exported functions

regexParser :: RegexTreeBuilder t => RegexParser t
regexParser = regexParserAccepting allRegexConstructs

parseRegex :: RegexTreeBuilder t => String -> Maybe t
parseRegex = parseString regexParser

isValidRegex :: String -> Bool
isValidRegex = isValidString (regexParser :: Parser ())


-- Top-level definition for the construction of the regex parser

regexParserAccepting :: RegexTreeBuilder t => [RegexParser t] -> RegexParser t
regexParserAccepting acceptedConstructs =
  parseRepetition (parseAny acceptedConstructs) buildConcatNode

allRegexConstructs :: RegexTreeBuilder t => [RegexParser t]
allRegexConstructs = [parseRegexCharClass, parseAlternation, parseAnyLetter]


-- Helper functions

rule :: [RegexParser t] -> ([t] -> t) -> RegexParser t
rule = parseInSequence

char :: RegexTreeBuilder t => Char -> Parser t
char c = parseChar c buildCharNode


-- Base elements

parseAnyLetter :: RegexTreeBuilder t => RegexParser t
parseAnyLetter = parseCharMatching isletter buildCharNode

isletter c = c >= 'a' && c <= 'z'


-- Complex elements

-- parseRegexCharClass -> '[' charlist ']'
parseRegexCharClass :: RegexTreeBuilder t => RegexParser t
parseRegexCharClass = rule [char '[', anyLetterSequence, char ']'] keepSecond
  where
    keepSecond xs = head (tail xs)  -- can't use x1:x2:xs because need also the other cases (less than 3 items in list)
    anyLetterSequence = parseRepetition parseAnyLetter buildCharClassNode

parseAlternation :: RegexTreeBuilder t => RegexParser t
parseAlternation = rule [left, char '|', right] buildAltNodeLR
  where
    left = regexParserAccepting [parseRegexCharClass, parseAnyLetter]
    right = regexParser
    buildAltNodeLR (left:sep:right:[]) = buildAltNode left right
