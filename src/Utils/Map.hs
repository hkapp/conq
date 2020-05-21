module Utils.Map where

import Utils.List (groupByKeyVal)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Semigroup (Semigroup, (<>))

mapFromValues :: (Ord k) => (a -> k) -> [a] -> Map k a
mapFromValues f = Map.fromList . map (\x -> (f x, x))

newtype SGMap k sg = SGMap (Map k sg)
newtype ManyMap k v = ManyMap (Map k [v])

joinMaps :: (Ord k) => Map k a -> Map k b -> Map k (Maybe a, Maybe b)
joinMaps left right =
  let
    allKeys = (Map.keys left) ++ (Map.keys right)
    lookupBoth key = (key, (Map.lookup key left, Map.lookup key right))
  in
    Map.fromList $ map lookupBoth allKeys

instance (Ord k) => Semigroup (ManyMap k v) where
  left <> right = toManyMap combinedMap
    where combinedMap = combineMany <$> joinedMaps
          joinedMaps = joinMaps (fromManyMap left) (fromManyMap right)
          combineMany (l,r) = fromMaybe [] (l <> r)  -- Maybe semigroup will use list semigroup

toManyMap :: Map k [v] -> ManyMap k v
toManyMap = ManyMap

fromManyMap :: ManyMap k v -> Map k [v]
fromManyMap (ManyMap m) = m

manyMapFromFlatList :: (Ord k) => (a -> k) -> (a -> v) -> [a] -> ManyMap k v
manyMapFromFlatList key val xs = toManyMap $ Map.fromList (groupByKeyVal key val xs)

conflictMapFromList :: (Ord k) => [(k, v)] -> Map k [v]
conflictMapFromList ((k,v):kvs) =
  Map.alter (\mv -> Just $ v : (fromMaybe [] mv))
            k
            (conflictMapFromList kvs)
conflictMapFromList [] = Map.empty
