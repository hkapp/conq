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
parseString parse input = extractFinalResult (parse input)

isValidString :: Parser t -> String -> Bool
isValidString = isJust `compose2` parseString

-- Parser combinators

parseAny :: (Foldable f) => f (Parser t) -> Parser t
parseAny parsers input = foldMap ($ input) parsers

parseInSequence :: [Parser t] -> ([t] -> t) -> Parser t
parseInSequence parsers combineResults = mapParser combineResults (parseInSequence2 parsers)

parseInSequence2 :: [Parser t] -> Parser [t]
parseInSequence2 = foldr (combineParsers (:)) (\s -> Success [] s)

parseInMonoidicStructure :: (Applicative f, Monoid (f t), Foldable f) => f (Parser t) -> Parser (f t)
parseInMonoidicStructure = foldr (combineParsers combineResults) (Success mempty)
  where combineResults val = mappend (pure val)


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

parseCharMatching :: (Char -> Bool) -> (Char -> t) -> Parser t
parseCharMatching predicate buildRes input = case input of
  c : rem | predicate c -> Success (buildRes c) rem
  _ -> Failure

parseOneChar :: (Char -> Bool) -> Parser Char
parseOneChar accept (c:cs)
  | (accept c) = Success c cs
  | otherwise = Failure

parseChar :: Char -> (Char -> t) -> Parser t
parseChar c f = parseCharMatching (\z -> z == c) f

exactChar :: Char -> Parser ()
exactChar c = ignoreResult $ parseOneChar exactlyC
  where
    ignoreResult = mapParser (\_ -> ())
    exactlyC = (==) c


--  INTERNAL CODE

-- Data types

type Parser t = String -> ParseResult t

data ParseResult t = Success t String | Failure

-- ParseResult t is:
-- a Monoid
-- a SemiGroup
-- a Foldable

-- ParseResult t is not:
-- a Monad
-- a Functor
-- a Traversable
-- an Applicative
-- an Alternative

-- Functions we can implement:
-- continueParsing :: ((a,String) -> ParseResult b) -> ParseResult a -> ParseResult b
-- continueParsing :: (String -> a -> ParseResult b) -> ParseResult a -> ParseResult b
-- ~~> Almost a Monad, but not quite
-- getTreeWithDefault :: a -> ParseResult a -> a
-- --> same as fromMaybe
-- firstSuccess :: [ParseResult a] -> ParseResult a
-- => Monoid.mconcat
-- keepSuccess :: ParseResult a -> ParseResult a -> ParseResult a
-- => MonadPlus.mplus, Alternative.(<|>)
-- empty :: ParseResult a
-- => Alternative.empty, Monoid.mempty
-- return :: a -> ParseResult a
-- => Applicative.pure, Monad.return, Maybe.Just
-- parseAfter :: (a -> b -> b) -> Parser b -> ParseResult a -> ParseResult b
-- parseAfter :: (a -> Parser b) -> ParseResult a -> ParseResult b

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

extractFinalResult :: ParseResult t -> Maybe t
extractFinalResult (Success finalTree []) = Just finalTree
extractFinalResult _ = Nothing

isSuccess :: ParseResult t -> Bool
isSuccess (Success _ _) = True
isSuccess _ = False

continueParsing :: (a -> b -> c) -> ParseResult a -> Parser b -> ParseResult c
continueParsing combineResults stem parseNext =
  case stem of
    Success ta remainingString -> fmap (combineResults ta) (parseNext remainingString)
    Failure -> Failure

onSuccess :: ((a, String) -> ParseResult b) -> ParseResult a -> ParseResult b
onSuccess f (Success a s) = f (a, s)
onSuccess _ Failure = Failure

-- Parser operations

combineParsers :: (a -> b -> c) -> (Parser a -> Parser b -> Parser c)
combineParsers combineResults parseFirst parseSecond input =
  continueParsing combineResults (parseFirst input) parseSecond

mapParser :: (a -> b) -> Parser a -> Parser b
mapParser f p input = fmap f (p input)
