module Parser where

-- Types

type Parser t = TreeBuilder t -> String -> ParseResult t

data ParseResult t = Success t String | Failure

data TreeBuilder t = TreeBuilder {
  buildChar      :: Char -> t,
  buildCharClass :: [t] -> t,
  buildConcat    :: [t] -> t
}

noTree = TreeBuilder dontCare dontCare dontCare
  where dontCare _ = ()

-- Exported functions

parse :: Parser t
parse tb = rep (anymatch [anyletter, charclass]) (buildConcat tb) tb

matches :: String -> Bool
matches input = case parse noTree input of
  Success () [] -> True
  _ -> False


-- Parser combinators

anymatch :: [Parser t] -> Parser t

anymatch (parseHead : remParsers) tb input = case parseHead tb input of
  Success t s -> Success t s
  Failure     -> anymatch remParsers tb input

anymatch [] _ _ = Failure


sq :: [Parser t] -> ([t] -> t) -> Parser t

sq elems merge tb input =
  let
    buildRes (True, rem, ts) = Success (merge ts) rem
    buildRes (False, _, _)   = Failure
    treeList = foldl parseInSeq initVal elems
      where
        initVal = (True, input, [])
        parseInSeq prevRes parseNext = case prevRes of
          (True, rem, ts) -> combineRes ts (parseNext tb rem)
          (False, _, _)   -> prevRes
          where
            combineRes ts (Success t rem) = (True, rem, t:ts)
            combineRes ts Failure         = (False, input, [])
  in buildRes treeList


rep :: Parser t -> ([t] -> t) -> Parser t

rep parseRepExp combineRes tb input =
  let
    recParse []       = ([], [])
    recParse recInput = case parseOnce of
      Success tree rem -> buildRes tree (recParse rem)
      Failure          -> ([], recInput)
      where
        parseOnce               = parseRepExp tb recInput
        buildRes t (ts, subrem) = (t : ts, subrem)
  in
  case recParse input of
    (resList@(t:ts), rem) -> Success (combineRes resList) rem
    ([], rem)             -> Failure


allowChars :: (Char -> Bool) -> Parser t

allowChars predicate t input = case input of
  c : rem | predicate c -> Success (buildChar t c) rem
  _ -> Failure

char :: Char -> Parser t

char c = allowChars (\z -> z == c)


-- Base elements

anyletter :: Parser t
anyletter = allowChars isletter

isletter c = c >= 'a' && c <= 'z'


-- Complex elements

-- charclass -> '[' charlist ']'
charclass :: Parser t
charclass tb = sq [char '[', rep anyletter (buildCharClass tb), char ']'] getSecond tb
  where getSecond xs = head (tail xs)
