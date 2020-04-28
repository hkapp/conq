{-|
 Module      : Dot
 Description : Helpers to print graph to a dot format
-}
module Dot where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Public types

type DotConfig = Map String String
type NodeConfig = DotConfig
type EdgeConfig = DotConfig
type GraphConfig = DotConfig

type NodeId = String
data Node = Node NodeId NodeConfig
data Edge = Edge Node Node EdgeConfig

data GraphKind = Digraph
data Graph = Graph GraphKind String [Node] [Edge] GraphConfig

-- Public API

buildGraph :: (Eq t) => (t -> Int) -> (t -> NodeConfig) -> (t -> [(EdgeConfig, t)]) -> t -> Graph
buildGraph singleNodeHash singleNodeRep children tree =
  getGraph (recBuildGraph singleNodeHash singleNodeRep children newBuilder tree)

-- Config primitives

emptyConfig :: DotConfig
emptyConfig = Map.empty

-- Graph primitives

emptyDigraph :: Graph
emptyDigraph = Graph Digraph defaultGraphName [] [] emptyConfig

defaultGraphName :: String
defaultGraphName = "G"

-- Node and Edge primitives

-- GraphBuilder

recBuildGraph :: (Eq t) => (t -> Int) -> (t -> NodeConfig) -> (t -> [(EdgeConfig, t)]) -> GraphBuilder t -> t -> GraphBuilder t
recBuildGraph singleNodeHash singleNodeRep children (Builder allocator currGraph) elem =
  let
    nodeConfig = singleNodeRep elem
    (allocator2, nodeId) = allocId allocator elem
    node =
    elemChildren = children elem
    edges =

allNodes :: t -> [t]
allNodesWithKey :: (t -> k) -> [t] -> [(t, k)]
-- Doesn't work because if the keys are the same the whole thing would be expected to be equal
-- Might need to implement our own ImperfectHashMap
-- Can use Map k [t]
-- Works with PartialOrd
-- Can get rid of the keyed function, and simply hash the NodeConfig returned
-- We can then use Hashed a
-- And define a PartialOrdering on Hashed a
-- Just use a HashMap!
allocateIds :: (Eq t, Ord k) => [(t, k)] -> Map (t, k) NodeId
getNodeId :: (Eq t, Ord k) => (t -> k) -> Map (t, k) NodeId -> t -> NodeId

dotNode :: (t -> NodeConfig) -> (t -> NodeId) -> t -> Node

allNodes :: t -> [t]
allNodesWithKey :: (t -> HashedNode t) -> [t] -> [HashedNode t]
-- Doesn't work because if the keys are the same the whole thing would be expected to be equal
-- Might need to implement our own ImperfectHashMap
-- Can use Map k [t]
-- Works with PartialOrd
-- Can get rid of the keyed function, and simply hash the NodeConfig returned
-- We can then use Hashed a
-- And define a PartialOrdering on Hashed a
-- Just use a HashMap!
hashNode :: (Hashable k) => (t -> k) -> t -> HashedNode t
allocateIds :: (Eq t) => [HashedNode t] -> Map (HashedNode t) NodeId
getNodeId :: (Eq t, Ord k) => (t -> k) -> Map (t, k) NodeId -> t -> NodeId

dotNode :: (t -> NodeConfig) -> (t -> NodeId) -> t -> Node

data GraphBuilder m = Builder (IdAllocator m) Graph

newBuilder :: GraphBuilder m
newBuilder = Builder newAllocator emptyDigraph

-- IdAllocator

type HashedNode m = (Int, m)
data IdAllocator m = Allocator Int (Set (HashedNode m))

newAllocator :: IdAllocator m
newAllocator = Allocator 0 Set.empty
