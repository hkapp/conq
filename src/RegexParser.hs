module RegexParser where

import Parser

-- How to restrict to only those types that have a TreeBuilder?
-- e.g. RegexTreeBuilder t => Parser t
-- -> not possible (https://stackoverflow.com/questions/22945348/why-class-constraint-in-type-synonym-needs-rankntypes)
type RegexParser t = Parser t

-- data RegexTreeBuilder t = RegexTreeBuilder {
class RegexTreeBuilder t where
  buildEmptyTree     :: t
  buildCharNode      :: Char -> t
  buildCharClassNode :: [t] -> t
  buildConcatNode    :: [t] -> t
  buildAltNode       :: t -> t -> t


instance RegexTreeBuilder () where
  buildEmptyTree       = ()
  buildCharNode _      = ()
  buildCharClassNode _ = ()
  buildConcatNode _    = ()
  buildAltNode _ _     = ()


-- Helper functions

--buildNothing :: RegexTreeBuilder t => (a -> t)
--buildNothing tb = (\x -> buildEmpty tb)

char :: RegexTreeBuilder t => Char -> Parser t
char c = singleChar c buildCharNode

rule :: [RegexParser t] -> ([t] -> t) -> RegexParser t
rule genParsers aggregator = sq genParsers aggregator


-- Exported functions

parse :: RegexTreeBuilder t => RegexParser t
parse = wholeParserFor parserList

wholeParserFor :: RegexTreeBuilder t => [RegexParser t] -> RegexParser t
wholeParserFor ps = rep (anymatch ps) buildConcatNode

parserList :: RegexTreeBuilder t => [RegexParser t]
parserList = startingKwParsers ++ alt : anyletter : []

startingKwParsers :: RegexTreeBuilder t => [RegexParser t]
startingKwParsers = [charclass]


match :: Parser ()
match = parse

matches :: String -> Bool
matches input = case match input of
  Success () [] -> True
  _ -> False


-- Base elements

anyletter :: RegexTreeBuilder t => RegexParser t
anyletter = singleCharMatch isletter buildCharNode

isletter c = c >= 'a' && c <= 'z'


-- Complex elements

-- charclass -> '[' charlist ']'
charclass :: RegexTreeBuilder t => RegexParser t
charclass = rule [char '[', repeatAnyLetter, char ']'] keepSecond
  where
    keepSecond xs = head (tail xs)
    repeatAnyLetter = rep anyletter buildCharClassNode

alt :: RegexTreeBuilder t => RegexParser t
alt = rule [left, char '|', right] buildAltNodeLR
  where
    left = wholeParserFor (startingKwParsers ++ anyletter : [])
    right = parse
    buildAltNodeLR (left:sep:right:[]) = buildAltNode left right
