module AbstractGraph where

import Utils ((<&>), zipWithIndexRight)

import Data.Bifunctor (Bifunctor(..))
import Data.Semigroup (Semigroup(..))
import Data.Map (Map, (!))
import qualified Data.Map as Map

-- type definition

data Graph v e = Graph [v] [(v, e, v)]

-- typeclass instances

instance Bifunctor Graph where
  bimap fv fe = mapGraphTriplets fv (mapEdgeFromTriplet fe)
      
  second fe = mapEdgeTriplets (mapEdgeFromTriplet fe)

-- Semigroup on graphs represent the union
instance Semigroup (Graph v e) where
  (Graph lv le) <> (Graph rv re) = Graph (lv <> rv) (le <> re)

instance Monoid (Graph v e) where
  mempty = Graph [] []
  mappend = (<>)

-- constructors

fromTree :: (t -> v) -> (t -> [(e, t)]) -> t -> Graph v e
fromTree toV children root =
  let
    rootV = toV root
    rootChildren = children root
    edgeTriplets = map (\(edge, dest) -> (rootV, edge, toV dest)) rootChildren
    subtreeGraphs = map (\(_, subtree) -> fromTree toV children subtree) rootChildren
  in
    (Graph [rootV] edgeTriplets) <> (mconcat subtreeGraphs)
    
fromTree_ :: (t -> v) -> (t -> [t]) -> t -> Graph v ()
fromTree_ toV children = fromTree toV childrenVoid
  where childrenVoid t = map (\child -> ((), child)) (children t)

singleton :: v -> Graph v e
singleton v = Graph [v] []

-- accessors

vertices :: Graph v e -> [v]
vertices (Graph vs _) = vs

edgeTriplets :: Graph v e -> [(v, e, v)]
edgeTriplets (Graph _ es) = es

-- mappers

mapVertices :: (v -> w) -> Graph v e -> Graph w e
mapVertices = first

mapEdges :: (e -> f) -> Graph v e -> Graph v f
mapEdges = second

mapEdgeTriplets :: ((v, e, v) -> f) -> Graph v e -> Graph v f
mapEdgeTriplets f (Graph vs es) = Graph vs (es <&> (\(s, e, d) -> (s, f (s, e, d), d)))

mapGraphTriplets :: (v1 -> v2) -> ((v1, e1, v1) -> e2) -> Graph v1 e1 -> Graph v2 e2
mapGraphTriplets fv fe (Graph vs es) = Graph (map fv vs) (map fvev es)
  where fvev (s, e, d) = (fv s, fe (s, e, d), fv d)

mapEdgeInTriplet :: (e -> f) -> (v, e, v) -> (v, f, v)
mapEdgeInTriplet f (s, e, d) = (s, f e, d)

mapEdgeFromTriplet :: (e -> f) -> (v, e, v) -> f
mapEdgeFromTriplet f (_, e, _) = f e

assignUniqueIds :: (Ord v) => Graph v e -> (Graph (v, Int) e, Map v Int)
assignUniqueIds g =
  let
    idMap = Map.fromList (zipWithIndexRight (vertices g))
    gWithIds = mapVertices (\v -> (v, idMap ! v)) g
  in
    (gWithIds, idMap)
