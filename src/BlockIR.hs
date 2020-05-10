module BlockIR where

import Utils

import qualified AbstractGraph as Abstract
import BlockTreeIR (BlockTree)
import qualified BlockTreeIR as BlockTree

import PrettyPrint (quoted)
import Dot (DotGraph)
import qualified Dot

import Data.Bool (bool)
import Data.Foldable (find)
import Data.Map (Map, (!))
import Data.Maybe (fromMaybe)

-- A control-flow oriented IR, which should cover everything

data Block = Block BlockId [Statement] Continuation
type BlockId = Int
type Statement = ()  -- for now we don't have any
type BoolExpr = BlockTree.Expr
data Continuation = Branch BoolExpr BlockId BlockId | Goto BlockId | Final Bool

instance Eq Block where
  b1 == b2 = getBlockId b1 == getBlockId b2

instance Ord Block where
  compare b1 b2 = compare (getBlockId b1) (getBlockId b2)

-- data Program = Program BlockId (Map BlockId Block) [Decl]
-- type Decl = String

-- 1. Extract from BlockTree form

toBlockList :: BlockTree -> [Block]
toBlockList = noDuplicates . fromAbstractTreeWithId . remapFinalIds . BlockTree.toAbstractTreeWithId

type AbstractBlockTree = Abstract.Tree (Either Bool BlockTree.Expr, Int) Bool

fromAbstractTreeWithId :: AbstractBlockTree -> [Block]

fromAbstractTreeWithId (Abstract.Tree (Right expr, id) children) =
  let
    findChild errorMessage edgeValue = maybe (childNotFoundError errorMessage) snd (find (\(b, _) -> b == edgeValue) children)
    succChild = findChild "success" True
    failChild = findChild "failure" False
    childNotFoundError message = error $ "Child node not found: " ++ quoted ("on " ++ message)
    childId child = snd (Abstract.getNode child)
    thisBlock = Block id [] (Branch expr (childId succChild) (childId failChild))
  in
    thisBlock : (fromAbstractTreeWithId succChild) ++ (fromAbstractTreeWithId failChild)

fromAbstractTreeWithId (Abstract.Tree (Left finalResult, id) []) = [ Block id [] (Final finalResult) ]

remapFinalIds :: AbstractBlockTree -> AbstractBlockTree
remapFinalIds abstractTree = Abstract.mapTreeNodes remapFinalId abstractTree
  where
    (finalSuccId, finalFailId) = finalBlockIds abstractTree
    newId = bool finalFailId finalSuccId
    remapFinalId (Left finalRes, oldId) = (Left finalRes, newId finalRes)
    remapFinalId x = x

finalBlockIds :: AbstractBlockTree -> (BlockId, BlockId)
finalBlockIds abstractTree = (succId, failId)
  where
    isFinalWith expectedResult (node, id) = either (== expectedResult) (const False) node
    findFinalNode finalRes = find (isFinalWith finalRes) (Abstract.allTreeNodes abstractTree)
    finalNode errorMessage finalRes =
      fromMaybe (childNotFoundError errorMessage) (findFinalNode finalRes)
    childNotFoundError message = error $ "Child node not found: " ++ quoted ("on " ++ message)
    finalNodeId = snd .: finalNode
    succId = finalNodeId "success" True
    failId = finalNodeId "failure" False

-- 2. Add input string management
-- TODO remember root node Id

mergeBlockLists :: [Block] -> [Block]

data Program = Program BlockId [Block]

addBlocks :: Program -> [Block] -> Program

-- need to keep the original ids unchanged
-- avoids rewrites everywhere or keeping track of the reassignment map

-- Can we actually turn the whole id allocation process into an Applicative?
-- -> I don't tink so
-- But how can we use traverse?

newtype IdAllocator = Allocator BlockId

allocateStartingFrom :: [Block] -> IdAllocator
allocateStartingFrom blocks = max (getBlockId <$> blocks) + 1

allocAssign :: IdAllocator -> Block -> (Block, IdAllocator)
allocAssign allocator block = allocMap (\newId -> reassignId newId block) allocator

reassignId :: BlockId -> Block -> Block
reassignId newId (Block _ stmts cont) = Block newId stmts cont

allocId :: Allocator -> (BlockId, Allocator)
allocId (Allocator nextId) = (nextId, Allocator (nextId + 1))

allocMap :: (BlockId -> a) -> Allocator -> (a, Allocator)
allocMap f allocator = Bifunctor.first f (allocId allocator)

allocMonad :: (BlockId -> a) -> GenAllocator a -> GenAllocator a

do
  id1 <-

addStringManagement :: [Block] -> [Block]
addStringManagement blocks = fst $ seqAddPatterns (allocateStartingFrom blocks) blocks
  where
    seqAddPatterns :: IdAllocator -> [Block] -> ([Block], IdAllocator)
    seqAddPatterns allocator (block : moreBlocks) =
      case (toCFGattern allocator block) of
        (expandedBlocks, newAllocatorState) -> expandedBlocks ++ (seqAddPatterns newAllocatorState moreBlocks)

toCFGPattern :: IdAllocator -> Block -> ([Block], IdAllocator)
toCFGattern allocator (Block id stmts cont) =
  case expandContPattern allocator cont of
    ()

expandContPattern :: IdAllocator -> Continuation -> (Continuation, [Block], IdAllocator)
expandContPattern allocator (Branch cond thenBranch elseBranch) = -- Can't be done without having knowledge from the BlockTree IR!

-- 3. Add main "start anywhere" loop

-- 4. Pretty print to C

-- Dot utilities

toAbstractGraph :: [Block] -> Abstract.Graph Block (Maybe Bool)
toAbstractGraph blocks = foldMap (localAbstractGraph blockMap) blocks
  where blockMap = mapFromValues getBlockId blocks

localAbstractGraph :: Map BlockId Block -> Block -> Abstract.Graph Block (Maybe Bool)
localAbstractGraph blockMap block = Abstract.Graph [block] abstractEdges
  where
    abstractEdges = case getContinuation block of
                      Branch _ succId failId ->
                        [(block, Just True, blockMap ! succId),
                         (block, Just False, blockMap ! failId)]
                      Goto nextId -> [(block, Nothing, blockMap ! nextId)]
                      Final b -> []

toDotGraph :: [Block] -> DotGraph
toDotGraph blocks = Dot.fromAnyAbstractGraph dotNode edgeConf (toAbstractGraph blocks)
  where
    dotNode :: Block -> Dot.Node
    dotNode (Block id _ cont) = Dot.Node (show id) (contConf cont)

    contConf (Branch exp _ _) = Dot.labelConfig (show exp)
    contConf (Goto _) = Dot.emptyConfig
    contConf (Final b) = Dot.labelConfig (if b then "success" else "failure")

    edgeConf :: (Block, Maybe Bool, Block) -> Dot.EdgeConfig
    edgeConf _ = Dot.emptyConfig

-- Accessors

getBlockId :: Block -> BlockId
getBlockId (Block id _ _) = id

getContinuation :: Block -> Continuation
getContinuation (Block _ _ cont) = cont
