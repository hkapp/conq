{-|
 Module      : Dot
 Description : Helpers to print graph to a dot format
-}
module Dot where

import AbstractGraph (vertices, edgeTriplets, mapGraphTriplets)
import qualified AbstractGraph as Abstract

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Public types

type DotConfig = Map String String
type NodeConfig = DotConfig
type EdgeConfig = DotConfig
type GraphConfig = (NodeConfig, EdgeConfig)

type NodeId = String
data Node = Node NodeId NodeConfig
data Edge = Edge Node Node EdgeConfig

data GraphKind = Digraph
data DotGraph = DotGraph GraphKind String [Node] [Edge] GraphConfig

-- With abstract graphs

fromAbstractGraph :: Abstract.Graph Node EdgeConfig -> DotGraph
fromAbstractGraph g =
  DotGraph Digraph defaultGraphName (vertices g) (edgeFromTriplet <$> edgeTriplets g) emptyGraphConfig

edgeFromTriplet :: (Node, EdgeConfig, Node) -> Edge
edgeFromTriplet (src, edgeConf, dest) = Edge src dest edgeConf

fromAnyAbstractGraph :: (v -> Node) -> ((v, e, v) -> EdgeConfig) -> Abstract.Graph v e -> DotGraph
fromAnyAbstractGraph buildNode getEdgeConfig g =
  fromAbstractGraph $ mapGraphTriplets buildNode getEdgeConfig g

-- Config primitives

emptyConfig :: DotConfig
emptyConfig = Map.empty

emptyGraphConfig :: GraphConfig
emptyGraphConfig = (emptyConfig, emptyConfig)

-- Graph primitives

emptyDigraph :: DotGraph
emptyDigraph = DotGraph Digraph defaultGraphName [] [] emptyGraphConfig

defaultGraphName :: String
defaultGraphName = "G"

-- Node and Edge primitives
