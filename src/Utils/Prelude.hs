{-# LANGUAGE CPP #-}
module Utils.Prelude where

#if __GLASGOW_HASKELL__ >= 808
import qualified Data.Functor as Functor
#endif

compose2 = (.) . (.)

(.:) = compose2

-- Useful functions introduced in latest Haskell versions

(<&>) :: Functor f => f a -> (a -> b) -> f b
#if __GLASGOW_HASKELL__ < 808
as <&> f = f <$> as

infixl 1 <&>
#else
(<&>) = (Functor.<&>)
#endif

-- Functions

doNothing :: IO ()
doNothing = return ()

ignore :: a -> ()
ignore = const ()
