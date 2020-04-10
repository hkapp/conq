{-|
 Module      : Parser
 Description : Contains generic functions and types to implement a CFG parser
-}
module Parser where

import Data.Maybe
import Data.Semigroup
import Data.Foldable (find)
import Data.Functor
import Control.Applicative (liftA2)
import Data.Monoid hiding ((<>))
import Utils


--  PUBLIC API

-- Top-level parser operations

parseString :: Parser t -> String -> Maybe t
parseString = extractFinalResult .: applyParser

isValidString :: Parser t -> String -> Bool
isValidString = isJust `compose2` parseString

-- Parser combinators

parseAny :: (Foldable f) => f (Parser t) -> Parser t
parseAny parsers = Parser (\input -> foldMap (startingWithString input) parsers)

parseInSequence :: [Parser t] -> Parser [t]
parseInSequence = foldr (combineParsers (:)) (pure [])

parseInMonoidicStructure :: (Applicative f, Monoid (f t), Foldable f) => f (Parser t) -> Parser (f t)
parseInMonoidicStructure = foldr (combineParsers combineResults) (pure mempty)
  where combineResults val = mappend (pure val)

repeatUntilFailure :: Parser t -> Parser [t]
repeatUntilFailure parse = Parser (\input -> recParse [] input (parse <@ input))
  where
    recParse resultStack _ (Success newTree remainingInput) = recParse (newTree : resultStack) remainingInput (parse <@ remainingInput)
    recParse resultStack savedInput Failure = Success (reverse resultStack) savedInput

repeatAtLeastOnce :: Parser t -> Parser [t]
repeatAtLeastOnce parser = mapParserResult (onSuccess failIfResultIsEmpty) (repeatUntilFailure parser)
  where
    failIfResultIsEmpty ([], _) = Failure
    failIfResultIsEmpty (nonEmptyResult, stringAfterParse) = Success nonEmptyResult stringAfterParse

-- Char-based parsers

parseOneChar :: (Char -> Bool) -> Parser Char
parseOneChar accept = Parser (\(c:cs) ->
  if (accept c) then Success c cs else Failure)

exactChar :: Char -> Parser Char
exactChar c = parseOneChar exactlyC
  where exactlyC = (==) c


--  INTERNAL CODE

-- Data types

newtype Parser t = Parser (String -> ParseResult t)

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
continueParsing combineResults stem nextParser = onSuccess parseAndCombine stem
  where
    parseAndCombine (stemResult, remainingString) = fmap (combineResults stemResult) (remainingString @> nextParser)

extractFinalResult :: ParseResult t -> Maybe t
extractFinalResult (Success finalTree []) = Just finalTree
extractFinalResult _ = Nothing

-- Parser operations

-- We can map the result of a Parser, so it's a functor
instance Functor Parser where
  fmap f (Parser parse) = Parser (\s -> fmap f (parse s))

-- We can combine the results of sequential Parsers, so it's an Applicative
instance Applicative Parser where
  pure x = Parser (\s -> Success x s)
  (<*>) = combineParsers id
  -- reuse the basic definition from the latest Base package
  -- liftA2 = combineParsers
  -- unfortunately with this version of ghc and the base package, liftA2 is only a function
  -- defined in Control.Applicative, not a class method

applyParser :: Parser t -> String -> ParseResult t
applyParser (Parser f) = f

startingWithString :: String -> Parser t -> ParseResult t
startingWithString = flip applyParser

(@>) :: String -> Parser t -> ParseResult t
s @> p = applyParser p s

(<@) :: Parser t -> String -> ParseResult t
p <@ s = applyParser p s

combineParsers :: (a -> b -> c) -> (Parser a -> Parser b -> Parser c)
combineParsers combineResults firstParser secondParser = Parser (\input ->
  continueParsing combineResults (firstParser <@ input) secondParser)

mapParser :: (a -> b) -> Parser a -> Parser b
mapParser = fmap

mapParserResult :: (ParseResult a -> ParseResult b) -> Parser a -> Parser b
mapParserResult f p = Parser (\s -> f (p <@ s))
