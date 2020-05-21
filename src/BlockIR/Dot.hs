module BlockIR.Dot where

import BlockIR.BlockIR

-- import BlockIR.BlockTree (BlockTree)
-- import qualified BlockIR.BlockTree as BlockTree

-- import RegexIR.RegexOpTree (RegexOpTree(..))
-- import qualified RegexIR.RegexOpTree as Regex

import qualified Utils.AbstractGraph as Abstract
-- import Utils.PrettyPrint (quoted, (%%), (+\\+), indent, properUnlines, concatWithSep)
import Utils.Dot (DotGraph)
import qualified Utils.Dot as Dot
-- import qualified Utils.CodeGen as C
-- import Utils.Prelude
import Utils.Map (mapFromValues)
-- import Utils.List

-- import Control.Monad.Trans.State as State (State, evalState)
-- import qualified Control.Monad.Trans.State as State

-- import Data.Bool (bool)
import Data.Foldable (find)
-- import Data.Function ((&))
-- import qualified Data.List as List
import Data.Map (Map, (!))
-- import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Semigroup (Semigroup, (<>))
-- import qualified Data.Set as Set

-- Dot utilities

toAbstractGraph :: [Block] -> Abstract.Graph Block (Maybe Bool)
toAbstractGraph blocks = foldMap (localAbstractGraph blockMap) blocks
  where blockMap = mapFromValues getBlockId blocks

localAbstractGraph :: Map BlockId Block -> Block -> Abstract.Graph Block (Maybe Bool)
localAbstractGraph blockMap block = Abstract.Graph [block] abstractEdges
  where
    abstractEdges = case getContinuation block of
                      Branch _ succDst failDst ->
                        [(block, Just True, blockMap ! (uncondDest succDst)),
                         (block, Just False, blockMap ! (uncondDest failDst))]
                      Uncond jmp -> [(block, Nothing, blockMap ! (uncondDest jmp))]
                      Final b -> []

toDotGraph :: Program -> DotGraph
toDotGraph program = addInputNode program blocksGraph
  where
    abstractGraph = toAbstractGraph $ programBlocks program
    blocksGraph = Dot.fromAnyAbstractGraph dotNode edgeConf abstractGraph
  
    dotNode :: Block -> Dot.Node
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

    edgeConf :: (Block, Maybe Bool, Block) -> Dot.EdgeConfig
    edgeConf (_, Just True, _) = Dot.edgeEnd Dot.Normal
    edgeConf (_, Just False, _) = Dot.edgeEnd Dot.BoxArrow
    edgeConf (_, Nothing, _) = Dot.emptyConfig

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
