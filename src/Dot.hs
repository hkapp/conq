{-|
 Module      : Dot
 Description : Helpers to print graph to a dot format
-}
module Dot where

import AbstractGraph (vertices, edgeTriplets, mapGraphTriplets)
import qualified AbstractGraph as Abstract
import Utils ((<&>))
import PrettyPrint ((+--+), quoted, emptyLine, formatStringList, indent)

import Data.Bifunctor (second)
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

-- Construction

fromAbstractGraph :: Abstract.Graph Node EdgeConfig -> DotGraph
fromAbstractGraph g =
  DotGraph Digraph defaultGraphName (vertices g) (edgeFromTriplet <$> edgeTriplets g) emptyGraphConfig

edgeFromTriplet :: (Node, EdgeConfig, Node) -> Edge
edgeFromTriplet (src, edgeConf, dest) = Edge src dest edgeConf

fromAnyAbstractGraph :: (v -> Node) -> ((v, e, v) -> EdgeConfig) -> Abstract.Graph v e -> DotGraph
fromAnyAbstractGraph buildNode getEdgeConfig g =
  fromAbstractGraph $ mapGraphTriplets buildNode getEdgeConfig g

-- Pretty printing

prettyPrint :: DotGraph -> String
prettyPrint g = unlines [
  prettyKind (graphKind g) +--+ graphName g +--+ "{",
  indent $ prettyNodes (allNodes g),
  emptyLine,
  indent $ prettyEdges (allEdges g),
  "}"
  ]

prettyKind :: GraphKind -> String
prettyKind Digraph = "digraph"

prettyNodes :: [Node] -> String
prettyNodes nodes = unlines (map prettyNode nodes)

prettyNode :: Node -> String
prettyNode (Node id conf) = id +--+ (prettyConf conf) ++ lineEnding

prettyEdges :: [Edge] -> String
prettyEdges edges = unlines (map prettyEdge edges)

prettyEdge :: Edge -> String
prettyEdge (Edge src dst conf) =
  (nodeId src) +--+ "->" +--+ (nodeId dst) +--+ (prettyConf conf) ++ lineEnding

prettyConf :: DotConfig -> String
prettyConf conf
  | (null conf) = ""
  | otherwise =
      let
        items = Map.toList conf
        dotConfItems = items <&> (\(k, v) -> k ++ "=" ++ show v)
      in
        formatStringList "[" ", " "]" dotConfItems

lineEnding :: String
lineEnding = ";"

-- Config primitives

emptyConfig :: DotConfig
emptyConfig = Map.empty

emptyGraphConfig :: GraphConfig
emptyGraphConfig = (emptyConfig, emptyConfig)

labelConfig :: String -> DotConfig
labelConfig label = Map.singleton "label" label

horizontalRecord :: [String] -> NodeConfig
horizontalRecord content =
  let
    label = formatStringList "{" "|" "}" content
    config1 = labelConfig label
    config2 = Map.insert "shape" "record" config1
  in
    config2

-- Graph primitives

emptyDigraph :: DotGraph
emptyDigraph = DotGraph Digraph defaultGraphName [] [] emptyGraphConfig

defaultGraphName :: String
defaultGraphName = "G"

graphKind :: DotGraph -> GraphKind
graphKind (DotGraph kind _ _ _ _) = kind

graphName :: DotGraph -> String
graphName (DotGraph _ name _ _ _) = name

allNodes :: DotGraph -> [Node]
allNodes (DotGraph _ _ nodes _ _) = nodes

allEdges :: DotGraph -> [Edge]
allEdges (DotGraph _ _ _ edges _) = edges

-- Node and Edge primitives

nodeId :: Node -> NodeId
nodeId (Node id _) = id

nodeWithLabel :: NodeId -> String -> Node
nodeWithLabel id label = Node id (labelConfig label)

horizontalRecordNode :: NodeId -> [String] -> Node
horizontalRecordNode id content = Node id (horizontalRecord content)
