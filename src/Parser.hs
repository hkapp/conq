{-|
 Module      : Parser
 Description : Contains generic functions and types to implement a CFG parser
-}
module Parser where

import Data.Maybe

-- Types

type Parser t = String -> ParseResult t

data ParseResult t = Success t String | Failure


-- Top-level parser operations

parseString :: Parser t -> String -> Maybe t
parseString parse input =
  case parse input of
    Success finalTree [] -> Just finalTree
    _ -> Nothing

isValidString :: Parser t -> String -> Bool
isValidString = isJust `appliedTo` parseString
  where
    appliedTo = (.) . (.)

-- Parser combinators

parseAny :: [Parser t] -> Parser t

parseAny (parseHead : remParsers) input = case parseHead input of
  Success t s -> Success t s
  Failure     -> parseAny remParsers input

parseAny [] _ = Failure


parseInSequence :: [Parser t] -> ([t] -> t) -> Parser t

parseInSequence elems merge input =
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



parseRepetition :: Parser t -> ([t] -> t) -> Parser t

parseRepetition parseRepExp combineRes input =
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


-- Char-based parsers

parseCharMatching :: (Char -> Bool) -> (Char -> t) -> Parser t

parseCharMatching predicate buildRes input = case input of
  c : rem | predicate c -> Success (buildRes c) rem
  _ -> Failure


parseChar :: Char -> (Char -> t) -> Parser t

parseChar c f = parseCharMatching (\z -> z == c) f
