module RegexIR.RegexOpTree where

import Parser.RegexParser
import Parser.Combinatorics
import Data.Set as Set (Set, fromList)
import Data.Foldable (foldl')

data RegexOpTree =
  RegexString String
  | RegexCharClass (Set Char)
  | RegexSequence [RegexOpTree]
  | RegexAlternative RegexOpTree RegexOpTree
  deriving (Show, Eq)


instance RegexTreeBuilder RegexOpTree where
  buildCharNode c = RegexString [c]
  buildCharClassNode options = RegexCharClass $ Set.fromList (map extractChar options)
    where extractChar (RegexString (c:[])) = c
  buildConcatNode = normalizedRegexSequence
  buildAltNode = RegexAlternative

normalizedRegexSequence :: [RegexOpTree] -> RegexOpTree
normalizedRegexSequence = buildNormalized . foldl' concatStringNodes []
  where buildNormalized (node:[]) = node
        buildNormalized nodeStack = RegexSequence (reverse nodeStack)
        concatStringNodes ((RegexString prefix):moreNodes) (RegexString suffix) =
          (RegexString (prefix ++ suffix)):moreNodes
        concatStringNodes nodeStack newNode = newNode:nodeStack

-- normalizedRegexSequence2 :: [RegexOpTree] -> RegexOpTree
-- normalizedRegexSequence2 = buildNormalized . stackBasedFoldl stackAct id
  -- where buildNormalized (node:[]) = node
        -- buildNormalized nodes = RegexSequence nodes
        -- stackAct (RegexString prefix) (RegexString suffix) = Replace $ RegexString (prefix ++ suffix)
        -- stackAct _ newNode = Stack newNode

-- data StackAction a = Ignore | Replace a | Stack a

-- stackBasedFoldl :: (Foldable t) => (b -> a -> StackAction b) -> (a -> b) -> t a -> [b]
-- stackBasedFoldl actionFor kickstart = reverse . foldl handleStack []
  -- where
    -- handleStack [] elem = [kickstart elem]
    -- handleStack prevStack elem =
      -- case actionFor (head prevStack) elem of
        -- Ignore -> prevStack
        -- Replace newHead -> newHead : (tail prevStack)
        -- Stack newHead -> newHead : prevStack

-- stackBasedFoldr :: (Foldable t) => (a -> b -> StackAction b) -> (a -> b) -> t a -> [b]
-- stackBasedFoldr actionFor kickstart = foldr handleStack []
  -- where
    -- handleStack firstElem [] = [kickstart firstElem]
    -- handleStack newElem prevStack =
      -- case actionFor newElem (head prevStack) of
        -- Ignore -> prevStack
        -- Replace newHead -> newHead : (tail prevStack)
        -- Stack newHead -> newHead : prevStack
