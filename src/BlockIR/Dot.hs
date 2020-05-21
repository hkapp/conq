module BlockIR.Dot where

import BlockIR.BlockIR

import qualified Utils.AbstractGraph as Abstract
import Utils.Dot (DotGraph)
import qualified Utils.Dot as Dot
import Utils.Map (mapFromValues)

import Data.Foldable (find)
import Data.Map (Map, (!))
import Data.Maybe (fromMaybe)
import Data.Semigroup (Semigroup, (<>))

type VertexRep = Block

data EdgeOutcome = Success | Failure | Always
data InlineMode = Inlined Bool
data EdgeRep = EdgeRep EdgeOutcome InlineMode

type GraphRep = Abstract.Graph VertexRep EdgeRep

toAbstractGraph :: [Block] -> GraphRep
toAbstractGraph blocks = foldMap (localAbstractGraph blockMap) blocks
  where blockMap = mapFromValues getBlockId blocks

edgeRepJmp :: EdgeOutcome -> UncondJump -> EdgeRep
edgeRepJmp outcome jmp = EdgeRep outcome $ case jmp of Goto _ -> Inlined False
                                                       Inline _ -> Inlined True

localAbstractGraph :: Map BlockId Block -> Block -> GraphRep
localAbstractGraph blockMap block = Abstract.Graph [block] abstractEdges
  where
    abstractEdges =
      case getContinuation block of
        Branch _ succDst failDst ->
          [(block, edgeRepJmp Success succDst , blockMap ! (uncondDest succDst)),
           (block, edgeRepJmp Failure failDst, blockMap ! (uncondDest failDst))]
        Uncond jmp -> [(block, edgeRepJmp Always jmp, blockMap ! (uncondDest jmp))]
        Final b -> []

toDotGraph :: Program -> DotGraph
toDotGraph program = addInputNode program blocksGraph
  where
    abstractGraph = toAbstractGraph $ programBlocks program
    blocksGraph = Dot.fromAnyAbstractGraph dotNode edgeConf abstractGraph
  
    dotNode :: VertexRep -> Dot.Node
    dotNode (Block id stmts cont) = Dot.Node (show id) (nodeConf stmts cont)

    nodeConf :: [Statement] -> Continuation -> Dot.NodeConfig
    nodeConf stmts cont
      | (null stmts) = Dot.labelConfig $ fromMaybe "<nothing>" (contLabel cont)
      | otherwise = let stmtStr = map show stmts
                        nodeContent = stmtStr ++ (maybe [] pure (contLabel cont))
                    in Dot.horizontalRecord nodeContent

    contLabel :: Continuation -> Maybe String
    contLabel (Branch exp _ _) = Just (show exp)
    contLabel (Uncond dest) = Nothing
    contLabel (Final b) = Just $ if b then "success" else "failure"

    edgeConf :: (VertexRep, EdgeRep, VertexRep) -> Dot.EdgeConfig
    edgeConf (_, edgeRep, _) =
      case edgeRep of
        EdgeRep Success _ -> Dot.edgeEnd Dot.Normal
        EdgeRep Failure _ -> Dot.edgeEnd Dot.BoxArrow
        EdgeRep Always  _ -> Dot.emptyConfig
      <>
      case edgeRep of
        EdgeRep _ (Inlined True)  -> Dot.edgeBegin Dot.Dot
        EdgeRep _ (Inlined False) -> Dot.emptyConfig

addInputNode :: Program -> DotGraph -> DotGraph
addInputNode p dotGraph =
  let
    allIds = getBlockId <$> programBlocks p
    uniqueId = (minimum allIds) - 1
    nodeConf = Dot.labelConfig "" <> Dot.nodeShape Dot.DoubleCircle
    inputNode = Dot.Node (show uniqueId) nodeConf
    graphWithInputNode = Dot.addNode dotGraph inputNode
    
    startId = programStartId p
    startNodeFound = find (\n -> Dot.nodeId n == show startId) (Dot.allNodes dotGraph)
    startNode = fromMaybe (error "Start node not found") startNodeFound
    edgeConf = Dot.emptyConfig
    inputEdge = Dot.Edge inputNode startNode edgeConf
    graphWithInputEdge = Dot.addEdge graphWithInputNode inputEdge
  in
    graphWithInputEdge
