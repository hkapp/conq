module RegexParser where

import Parser


type RegexParser t = TreeBuilder t -> Parser t

data TreeBuilder t = TreeBuilder {
  buildEmpty     :: t,
  buildChar      :: Char -> t,
  buildCharClass :: [t] -> t,
  buildConcat    :: [t] -> t
}


noTree :: TreeBuilder ()
noTree = TreeBuilder () noop noop noop
  where noop _ = ()


-- Helper functions

buildNothing :: TreeBuilder t -> (a -> t)
buildNothing tb = (\x -> buildEmpty tb)

char :: Char -> TreeBuilder t -> Parser t
char c tb = singleChar c (buildNothing tb)

rule :: [(TreeBuilder t -> Parser t)] -> ([t] -> t) -> TreeBuilder t -> Parser t
rule genParsers aggregator tb = sq (map ($ tb) genParsers) aggregator


-- Exported functions

parse :: RegexParser t
parse tb = rep (anymatch [anyletter tb, charclass tb]) (buildConcat tb)

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
    keepSecond xs = head (tail xs)
    repeatAnyLetter tb = rep (anyletter tb) (buildCharClass tb)
