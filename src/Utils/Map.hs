module Utils.Map where

import Data.Map (Map)
import qualified Data.Map as Map

mapFromValues :: (Ord k) => (a -> k) -> [a] -> Map k a
mapFromValues f = Map.fromList . map (\x -> (f x, x))
