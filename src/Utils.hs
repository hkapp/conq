module Utils where

compose2 = (.) . (.)

(.:) = compose2

-- Useful functions introduced in latest Haskell versions

(<&>) :: Functor f => f a -> (a -> b) -> f b
as <&> f = f <$> as

infixl 1 <&>
