module Parser where


-- Types

type Parser t = String -> ParseResult t

data ParseResult t = Success t String | Failure


-- Combinators

anymatch :: [Parser t] -> Parser t

anymatch (parseHead : remParsers) input = case parseHead input of
  Success t s -> Success t s
  Failure     -> anymatch remParsers input

anymatch [] _ = Failure


sq :: [Parser t] -> ([t] -> t) -> Parser t

sq elems merge input =
  let
    buildRes (True, rem, ts) = Success (merge ts) rem
    buildRes (False, _, _)   = Failure
    treeList = foldl parseInSeq initVal elems
      where
        initVal = (True, input, [])
        parseInSeq prevRes parseNext = case prevRes of
          (True, rem, ts) -> combineRes ts (parseNext rem)
          (False, _, _)   -> prevRes
          where
            combineRes ts (Success t rem) = (True, rem, t:ts)
            combineRes ts Failure         = (False, input, [])
  in buildRes treeList



rep :: Parser t -> ([t] -> t) -> Parser t

rep parseRepExp combineRes input =
  let
    recParse []       = ([], [])
    recParse recInput = case parseOnce of
      Success tree rem -> buildRes tree (recParse rem)
      Failure          -> ([], recInput)
      where
        parseOnce               = parseRepExp recInput
        buildRes t (ts, subrem) = (t : ts, subrem)
  in
  case recParse input of
    (resList@(t:ts), rem) -> Success (combineRes resList) rem
    ([], rem)             -> Failure


-- Helpers

singleCharMatch :: (Char -> Bool) -> (Char -> t) -> Parser t

singleCharMatch predicate buildRes input = case input of
  c : rem | predicate c -> Success (buildRes c) rem
  _ -> Failure


singleChar :: Char -> (Char -> t) -> Parser t

singleChar c f = singleCharMatch (\z -> z == c) f
