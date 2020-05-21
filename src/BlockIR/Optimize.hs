module BlockIR.Optimize where

import BlockIR.BlockIR

import Utils.Prelude
import qualified Utils.Map as MapUtils

import Data.Function ((&))
import qualified Data.List as List
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Semigroup (Semigroup, (<>))
import Data.Tuple (swap)

optimize :: Program -> Program
optimize p = foldl (&) p allOptimizations

type Optimization = Program -> Program

allOptimizations :: [Optimization]
allOptimizations = [ performInliningOn ]

-- Inlining

performInliningOn :: Program -> Program
performInliningOn p = let revCFG = reverseControlFlowGraph p
                          jumpCounts = Map.map length revCFG
                      in mapProgramJumps (tryToInline jumpCounts) p

mapBlockCont :: (Continuation -> Continuation) -> Block -> Block
mapBlockCont f (Block id stmts cont) = Block id stmts (f cont)

mapProgramConts :: (Continuation -> Continuation) -> Program -> Program
mapProgramConts f p = mapProgramBlocks (mapBlockCont f) p

mapContJumps :: (UncondJump -> UncondJump) -> Continuation -> Continuation
mapContJumps f (Branch expr jmpSucc jmpFail) =
  Branch expr (f jmpSucc) (f jmpFail)
mapContJumps f (Uncond jmp) = Uncond (f jmp)
mapContJumps _ unchanged@(Final _) = unchanged

mapBlockJumps :: (UncondJump -> UncondJump) -> Block -> Block
mapBlockJumps f = mapBlockCont (mapContJumps f)

mapProgramJumps :: (UncondJump -> UncondJump) -> Program -> Program
mapProgramJumps f (Program startJmp blocks) =
  Program (f startJmp) (blocks <&> mapBlockJumps f)

tryToInline :: Map BlockId Int -> UncondJump -> UncondJump
tryToInline jumpCount (Goto destId)
  | ((jumpCount ! destId) == 1) = Inline destId
  | otherwise = Goto destId
tryToInline _ unchanged@(Inline _) = unchanged

-- Compute the reverse CFG

data Caller = AnotherBlock BlockId | ProgramStart
  deriving (Eq, Ord)
type Callee = BlockId

type ForwardCFG = Map Caller [Callee]
type ReverseCFG = Map Callee [Caller]

controlFlowGraph :: Program -> Map Caller [Callee]
controlFlowGraph = buildCFG id

reverseControlFlowGraph :: Program -> Map Callee [Caller]
reverseControlFlowGraph = buildCFG swap

buildCFG :: (Ord k) => ((Caller, Callee) -> (k, v)) -> Program -> Map k [v]
buildCFG f p = let allEdges = programFlowEdges p <&> f
               in MapUtils.conflictMapFromList allEdges

programFlowEdges :: Program -> [(Caller, Callee)]
programFlowEdges (Program start blocks) =
  let
    blockEdges = concatMap controlFlowEdges blocks
    startEdge = (ProgramStart, uncondDest start)
  in
    startEdge : blockEdges

controlFlowEdges :: Block -> [(Caller, Callee)]
controlFlowEdges (Block id _ cont) =
  map (\dst -> (AnotherBlock id, dst)) (controlFlowDests cont)

controlFlowDests :: Continuation -> [Callee]
controlFlowDests c = (contJumps c) <&> uncondDest
