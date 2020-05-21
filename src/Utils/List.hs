module Utils.List where

import qualified Data.Set as Set
import Data.Tuple (swap)

noDuplicates :: (Ord a) => [a] -> [a]
noDuplicates = Set.toList . Set.fromList

-- Zip

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex = zip [0..]

zipWithIndexRight :: [a] -> [(a, Int)]
zipWithIndexRight = map swap . zipWithIndex
