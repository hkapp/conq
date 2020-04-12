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
parseAny = foldr fallbacksTo alwaysFail

parseInSequence :: [Parser t] -> Parser [t]
parseInSequence = fmap reverse . reverseSequenceParser
  where reverseSequenceParser = foldr (combineParsers (:)) (alwaysSucceed [])

parseInMonoidicStructure :: (Applicative f, Monoid (f t), Foldable f) => f (Parser t) -> Parser (f t)
parseInMonoidicStructure = foldr (combineParsers combineResults) (pure mempty)
  where combineResults val = mappend (pure val)

repeatUntilFailure :: Parser t -> Parser [t]
repeatUntilFailure simpleParser = (repeatAtLeastOnce simpleParser) `fallbacksTo` (alwaysSucceed [])

repeatAtLeastOnce :: Parser t -> Parser [t]
repeatAtLeastOnce simpleParser = mapParserResult (recParse [] Nothing) simpleParser
  where
    recParse prevTrees _ (Success newTree newSafePoint) =
      newSafePoint @> mapParserResult (recParse (newTree : prevTrees) (Just newSafePoint)) simpleParser
    recParse [] Nothing Failure = Failure
    recParse resultStack (Just savedInput) Failure = Success (reverse resultStack) savedInput

-- Char-based parsers

parseOneChar :: (Char -> Bool) -> Parser Char
parseOneChar accept = Parser (\(c:cs) -> if (accept c) then Success c cs else Failure)

exactChar :: Char -> Parser Char
exactChar c = parseOneChar (\x -> x == c)


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
-- instance Semigroup (ParseResult t) where
  -- Failure <> b = b
  -- a <> Failure = a
  -- a@(Success _ _) <> b = a

-- instance Monoid (ParseResult t) where
  -- mempty = Failure
  -- mappend = (<>)
  -- mconcat = fromMaybe Failure . find isSuccess

instance Functor ParseResult where
  fmap f (Success a s) = Success (f a) s
  fmap _ Failure = Failure

isSuccess :: ParseResult t -> Bool
isSuccess (Success _ _) = True
isSuccess _ = False

onSuccess :: ((a, String) -> ParseResult b) -> ParseResult a -> ParseResult b
onSuccess f (Success a s) = f (a, s)
onSuccess _ Failure = Failure

onFailureUse :: ParseResult t -> ParseResult t -> ParseResult t
onFailureUse defaultResult mainResult = if (isSuccess mainResult) then mainResult else defaultResult

onFailure :: ParseResult t -> ParseResult t -> ParseResult t
onFailure = flip onFailureUse

continueParsing :: (a -> b -> c) -> Parser b -> ParseResult a -> ParseResult c
continueParsing combineResults nextParser = onSuccess parseAndCombine
  where
    parseAndCombine (lastTree, remainingString) = (remainingString @> nextParser) <&> (combineResults lastTree)

extractFinalResult :: ParseResult t -> Maybe t
extractFinalResult (Success finalTree []) = Just finalTree
extractFinalResult _ = Nothing


-- Parser operations

-- We can map the result of a Parser, so it's a functor
instance Functor Parser where
  fmap = mapParserResult . fmap

-- We can combine the results of sequential Parsers, so it's an Applicative
instance Applicative Parser where
  pure = alwaysSucceed
  (<*>) = combineParsers id
  -- reuse the basic definition from the latest Base package
  -- liftA2 = combineParsers
  -- unfortunately with this version of ghc and the base package, liftA2 is only a function
  -- defined in Control.Applicative, not a class method

applyParser :: Parser t -> String -> ParseResult t
applyParser (Parser f) = f

startingWith :: String -> Parser t -> ParseResult t
startingWith = flip applyParser

(@>) :: String -> Parser t -> ParseResult t
s @> p = applyParser p s

(<@) :: Parser t -> String -> ParseResult t
p <@ s = applyParser p s

combineParsers :: (a -> b -> c) -> (Parser a -> Parser b -> Parser c)
combineParsers combineResults firstParser secondParser =
  mapParserResult (continueParsing combineResults secondParser) firstParser

mapParser :: (a -> b) -> Parser a -> Parser b
mapParser = fmap

mapParserResult :: (ParseResult a -> ParseResult b) -> Parser a -> Parser b
mapParserResult f p = Parser (\s -> f (p <@ s))

fallback :: Parser t -> Parser t -> Parser t
fallback fallbackParser mainParser = Parser (\s -> (s @> mainParser) `onFailure` (s @> fallbackParser))

fallbacksTo :: Parser t -> Parser t -> Parser t
fallbacksTo = flip fallback

alwaysFail :: Parser t
alwaysFail = Parser (pure Failure)

alwaysSucceed :: t -> Parser t
alwaysSucceed x = Parser (\s -> Success x s)
