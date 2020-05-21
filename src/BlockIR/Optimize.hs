module BlockIR.Optimize where

import BlockIR.BlockIR

-- import BlockIR.BlockTree (BlockTree)
-- import qualified BlockIR.BlockTree as BlockTree

-- import RegexIR.RegexOpTree (RegexOpTree(..))
-- import qualified RegexIR.RegexOpTree as Regex

-- import qualified Utils.AbstractGraph as Abstract
-- import Utils.PrettyPrint (quoted, (%%), (+\\+), indent, properUnlines, concatWithSep)
-- import Utils.Dot (DotGraph)
-- import qualified Utils.Dot as Dot
-- import qualified Utils.CodeGen as C
import Utils.Prelude
-- import Utils.Map
-- import Utils.List

-- import Control.Monad.Trans.State as State (State, evalState)
-- import qualified Control.Monad.Trans.State as State

-- import Data.Bool (bool)
-- import Data.Foldable (find)
import Data.Function ((&))
import qualified Data.List as List
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Semigroup (Semigroup, (<>))
-- import qualified Data.Set as Set

-- 3. Optimize

optimize :: Program -> Program
optimize p = foldl (&) p allOptimizations

type Optimization = Program -> Program

allOptimizations :: [Optimization]
allOptimizations = [ performInliningOn ]

-- 3a. Compute the reverse CFG

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

data Caller = AnotherBlock BlockId | ProgramStart
  deriving (Eq, Ord)
type Callee = BlockId

type ForwardCFG = Map Caller [Callee]
type ReverseCFG = Map Callee [Caller]

groupByKeyVal :: (Eq k) => (a -> k) -> (a -> v) -> [a] -> [(k, [v])]
groupByKeyVal key val xs = map transformGroup naiveGroups
  where
    naiveGroups = List.groupBy (\x1 x2 -> (key x1) == (key x2)) xs
    transformGroup thisGroup = (thisGroupKey, thisGroupVals)
      where thisGroupKey = key (head thisGroup)
            thisGroupVals = val <$> thisGroup

manyMapFromFlatList :: (Ord k) => (a -> k) -> (a -> v) -> [a] -> ManyMap k v
manyMapFromFlatList key val xs = toManyMap $ Map.fromList (groupByKeyVal key val xs)

controlFlowGraph :: Program -> Map Caller [Callee]
controlFlowGraph = buildCFG fst snd

reverseControlFlowGraph :: Program -> Map Callee [Caller]
reverseControlFlowGraph = buildCFG snd fst

buildCFG :: (Ord k) => ((Caller, Callee) -> k) -> ((Caller, Callee) -> v) -> Program -> Map k [v]
buildCFG key val p = let allEdges = programFlowEdges p
                     in fromManyMap $ manyMapFromFlatList key val allEdges

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
controlFlowDests (Branch _ succId failId) = [uncondDest succId, uncondDest failId]
controlFlowDests (Uncond jmp) = [uncondDest jmp]
controlFlowDests (Final _) = []

-- 3a. Differentiate between UncondJump = Goto BlockId | Inline BlockId
--     Also change Program UncondJump [Block]

-- 3b. Actually perform inlining

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
