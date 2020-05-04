module Utils where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)

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

ignore :: a -> ()
ignore = const ()

traverseWhile :: (Traversable t, Monad m) => (a -> m Bool) -> t a -> m Bool
traverseWhile f xs = sequenceWhile (fmap f xs)

sequenceWhile :: (Foldable t, Monad m) => t (m Bool) -> m Bool
sequenceWhile = foldr condExec (return True)
  where condExec thisAction execRest = do
          success <- thisAction
          if success
            then execRest
            else return success

sequenceWhile_ :: (Foldable t, Monad m) => t (m Bool) -> m ()
sequenceWhile_ = fmap ignore . sequenceWhile

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex = zip [0..]

zipWithIndexRight :: [a] -> [(a, Int)]
zipWithIndexRight = map swap . zipWithIndex

-- Data.Map

mapFromValues :: (Ord k) => (a -> k) -> [a] -> Map k a
mapFromValues f = Map.fromList . map (\x -> (f x, x))
