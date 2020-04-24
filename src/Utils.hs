module Utils where

import Data.Set (Set)
import qualified Data.Set as Set

compose2 = (.) . (.)

(.:) = compose2

-- Useful functions introduced in latest Haskell versions

(<&>) :: Functor f => f a -> (a -> b) -> f b
as <&> f = f <$> as

infixl 1 <&>

belongsTo :: (Ord a) => Set a -> a -> Bool
belongsTo = flip Set.member

doNothing :: IO ()
doNothing = return ()
