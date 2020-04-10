{-|
 Module      : Parser
 Description : Contains generic functions and types to implement a CFG parser
-}
module Parser where

import Data.Maybe
import Data.Semigroup
import Data.Foldable (find)
import Control.Applicative
import Data.Monoid hiding ((<>))
import Utils


--  PUBLIC API

-- Top-level parser operations

parseString :: Parser t -> String -> Maybe t
parseString parse = extractFinalResult . parse

isValidString :: Parser t -> String -> Bool
isValidString = isJust `compose2` parseString

-- Parser combinators

parseAny :: (Foldable f) => f (Parser t) -> Parser t
parseAny parsers input = foldMap ($ input) parsers

parseInSequence :: [Parser t] -> Parser [t]
parseInSequence = foldr (combineParsers (:)) (\s -> Success [] s)

parseInMonoidicStructure :: (Applicative f, Monoid (f t), Foldable f) => f (Parser t) -> Parser (f t)
parseInMonoidicStructure = foldr (combineParsers combineResults) (Success mempty)
  where combineResults val = mappend (pure val)

repeatUntilFailure :: Parser t -> Parser [t]
repeatUntilFailure parse input = recParse [] input (parse input)
  where
    recParse resultStack _ (Success newTree remainingInput) = recParse (newTree : resultStack) remainingInput (parse remainingInput)
    recParse resultStack savedInput Failure = Success (reverse resultStack) savedInput

repeatAtLeastOnce :: Parser t -> Parser [t]
repeatAtLeastOnce = onSuccess failIfResultIsEmpty .: repeatUntilFailure
  where
    failIfResultIsEmpty ([], _) = Failure
    failIfResultIsEmpty (nonEmptyResult, stringAfterParse) = Success nonEmptyResult stringAfterParse

-- Char-based parsers

-- parseCharMatching :: (Char -> Bool) -> (Char -> t) -> Parser t
-- parseCharMatching predicate buildRes input = case input of
  -- c : rem | predicate c -> Success (buildRes c) rem
  -- _ -> Failure

parseOneChar :: (Char -> Bool) -> Parser Char
parseOneChar accept (c:cs)
  | (accept c) = Success c cs
  | otherwise = Failure

-- parseChar :: Char -> (Char -> t) -> Parser t
-- parseChar c = parseCharMatching (\z -> z == c)

exactChar :: Char -> Parser Char
exactChar c = parseOneChar exactlyC
  where exactlyC = (==) c


--  INTERNAL CODE

-- Data types

type Parser t = String -> ParseResult t

data ParseResult t = Success t String | Failure

-- ParseResult t is:
-- a Monoid
-- a SemiGroup
-- a Foldable
-- a Functor

-- ParseResult t is not:
-- a Monad
-- a Traversable
-- an Applicative
-- an Alternative

-- ParseResult operations

-- The ParseResult Semigroup returns the first success found
instance Semigroup (ParseResult t) where
  Failure <> b = b
  a <> Failure = a
  a@(Success _ _) <> b = a

instance Monoid (ParseResult t) where
  mempty = Failure
  mappend = (<>)
  mconcat = fromMaybe Failure . find isSuccess

instance Functor ParseResult where
  fmap f (Success a s) = Success (f a) s
  fmap _ Failure = Failure

isSuccess :: ParseResult t -> Bool
isSuccess (Success _ _) = True
isSuccess _ = False

onSuccess :: ((a, String) -> ParseResult b) -> ParseResult a -> ParseResult b
onSuccess f (Success a s) = f (a, s)
onSuccess _ Failure = Failure

onFailure :: ParseResult t -> ParseResult t -> ParseResult t
onFailure defaultResult testedResult = testedResult <> defaultResult

continueParsing :: (a -> b -> c) -> ParseResult a -> Parser b -> ParseResult c
continueParsing combineResults stem parseNext = onSuccess parseAndCombine stem
  where
    parseAndCombine (stemResult, remainingString) = fmap (combineResults stemResult) (parseNext remainingString)

extractFinalResult :: ParseResult t -> Maybe t
extractFinalResult (Success finalTree []) = Just finalTree
extractFinalResult _ = Nothing

-- Parser operations

combineParsers :: (a -> b -> c) -> (Parser a -> Parser b -> Parser c)
combineParsers combineResults parseFirst parseSecond input =
  continueParsing combineResults (parseFirst input) parseSecond

mapParser :: (a -> b) -> Parser a -> Parser b
mapParser f p input = fmap f (p input)
