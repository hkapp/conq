module Utils.Set where

import Data.Set (Set)
import qualified Data.Set as Set

belongsTo :: (Ord a) => Set a -> a -> Bool
belongsTo = flip Set.member
