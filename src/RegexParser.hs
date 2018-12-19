module RegexParser where

import Parser


type RegexParser t = TreeBuilder t -> Parser t

data TreeBuilder t = TreeBuilder {
  buildEmpty     :: t,
  buildChar      :: Char -> t,
  buildCharClass :: [t] -> t,
  buildConcat    :: [t] -> t,
  buildAlt       :: t -> t -> t
}


noTree :: TreeBuilder ()
noTree = TreeBuilder () noop noop noop noop2
  where
    noop _ = ()
    noop2 _ _ = ()


-- Helper functions

buildNothing :: TreeBuilder t -> (a -> t)
buildNothing tb = (\x -> buildEmpty tb)

char :: Char -> TreeBuilder t -> Parser t
char c tb = singleChar c (buildNothing tb)

rule :: [RegexParser t] -> (TreeBuilder t -> [t] -> t) -> RegexParser t
rule genParsers aggregator tb = sq (map ($ tb) genParsers) (aggregator tb)


-- Exported functions

parse :: RegexParser t
parse = wholeParserFor parserList

wholeParserFor :: [RegexParser t] -> RegexParser t
wholeParserFor ps tb = rep (anymatch parseOrder) concatSubExp
  where
    parseOrder = map ($ tb) ps
    concatSubExp = buildConcat tb

parserList :: [RegexParser t]
parserList = startingKwParsers ++ alt : anyletter : []

startingKwParsers :: [RegexParser t]
startingKwParsers = [charclass]


match :: Parser ()
match = parse noTree

matches :: String -> Bool
matches input = case match input of
  Success () [] -> True
  _ -> False


-- Base elements

anyletter :: RegexParser t
anyletter tb = singleCharMatch isletter (buildChar tb)

isletter c = c >= 'a' && c <= 'z'


-- Complex elements

-- charclass -> '[' charlist ']'
charclass :: RegexParser t
charclass = rule [char '[', repeatAnyLetter, char ']'] keepSecond
  where
    keepSecond tb xs = head (tail xs)
    repeatAnyLetter tb = rep (anyletter tb) (buildCharClass tb)

alt :: RegexParser t
alt = rule [left, char '|', right] buildAltNode
  where
    left = wholeParserFor (startingKwParsers ++ anyletter : [])
    right = parse
    buildAltNode tb (left:sep:right:[]) = (buildAlt tb) left right
