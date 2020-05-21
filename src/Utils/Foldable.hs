module Utils.Foldable where

import Utils.Prelude (ignore)

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
