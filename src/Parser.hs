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
parseInSequence elems merge input = buildSeqRes $ foldl seqParse (Success [] input) elems
  where
    seqParse (Success prevTrees remainingInput) p = case p remainingInput of
      Success t remainingAfterParse -> Success (t:prevTrees) remainingAfterParse
      Failure -> Failure
    seqParse Failure _ = Failure
    buildSeqRes (Success treeStack remainingInput) = Success (merge (reverse treeStack)) remainingInput
    buildSeqRes Failure = Failure
-- foldl = (a op b) op c
-- foldr = a op (b op c)

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
