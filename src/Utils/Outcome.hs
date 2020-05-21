module Utils.Outcome where

data Outcome a = Outcome a a

success :: Outcome a -> a
success (Outcome s _) = s

failure :: Outcome a -> a
failure (Outcome _ f) = f
