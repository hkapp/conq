module BlockIR where

import Utils

import qualified AbstractGraph as Abstract
import BlockTreeIR (BlockTree)
import qualified BlockTreeIR as BlockTree
import RegexOpTree (RegexOpTree(..))
import qualified RegexOpTree as Regex

import PrettyPrint (quoted)
import Dot (DotGraph)
import qualified Dot

import Data.Bool (bool)
import Data.Foldable (find)
import Data.Map (Map, (!))
import Data.Maybe (fromMaybe)
import Control.Monad.Trans.State as State (State, evalState)
import qualified Control.Monad.Trans.State as State

-- A control-flow oriented IR, which should cover everything

data Block = Block BlockId [Statement] Continuation
type BlockId = Int
data Statement = Advance Int
  deriving Show
type BoolExpr = BlockTree.Expr
data Continuation = Branch BoolExpr BlockId BlockId | Goto BlockId | Final Bool

instance Eq Block where
  b1 == b2 = getBlockId b1 == getBlockId b2

instance Ord Block where
  compare b1 b2 = compare (getBlockId b1) (getBlockId b2)

-- 1. Extract BlockIR from RegexOpTree

data Program = Program BlockId [Block]

fromRegexOpTree :: RegexOpTree -> Program
fromRegexOpTree regex = evalState (buildFromRegex regex) newBuilderState

type ProgramBuilder = BlockId

newBuilderState :: ProgramBuilder
newBuilderState = 0

buildFromRegex :: RegexOpTree -> State ProgramBuilder Program
buildFromRegex regexTree = do
  succId <- allocateId
  failId <- allocateId
  let succBlock = finalBlock succId True
  let failBlock = finalBlock failId False
  let finalOutcome = Outcome succId failId
  Program beginId blocks <- blockPattern finalOutcome regexTree
  return $ Program beginId ([succBlock, failBlock] ++ blocks)

allocateId :: State ProgramBuilder BlockId
allocateId = do
  nextId <- State.get
  State.put (nextId + 1)
  return nextId

blockPattern :: Outcome BlockId -> RegexOpTree -> State ProgramBuilder Program

-- Branch if the prefix equals the given string
blockPattern branch (RegexString s) =
  branchAndAdvancePattern
    branch
    (BlockTree.StringEq s)
    (length s)

-- Branch if the first character is in the char class
blockPattern branch (RegexCharClass cc) =
  branchAndAdvancePattern
    branch
    (BlockTree.FirstCharIn cc)
    1  -- advance by one

blockPattern branch (RegexSequence regexSubtrees) =
  let
    buildChain (firstRegex : nextRegexes) = do
      Program chainStart chainBlocks <- buildChain nextRegexes
      -- The first regex does the following:
      --   On success, start the rest of the chain
      --   On failure, go to failure
      let firstRegexOutcome = Outcome chainStart (failure branch)
      Program firstRegexStart firstRegexBlocks <- blockPattern firstRegexOutcome firstRegex
      return $ Program firstRegexStart (firstRegexBlocks ++ chainBlocks)
    
    buildChain [] = pure $ Program (success branch) []
  in
    buildChain regexSubtrees

-- Start left
--   On success, go to success
--   On failure, start right
blockPattern branch (RegexAlternative left right) = do
  Program rightId rightBlocks <- blockPattern branch right
  let leftOutcome = Outcome (success branch) rightId
  Program leftId leftBlocks <- blockPattern leftOutcome left
  return $ Program leftId (leftBlocks ++ rightBlocks)

-- Basic branching pattern
--   If the expression succeeds, advance of the given amount and go to success
--   If the expression fails, go to failure
branchAndAdvancePattern :: Outcome BlockId -> BoolExpr -> Int -> State ProgramBuilder Program
branchAndAdvancePattern branch expr advanceCount = do
  exprBlockId <- allocateId
  advBlockId <- allocateId
  let exprBlock = branchBlock
                    exprBlockId
                    (Outcome advBlockId (failure branch))
                    expr
  let advBlock = stmtBlock
                   advBlockId
                   (success branch)
                   [Advance advanceCount]
  return $ Program exprBlockId [exprBlock, advBlock]
  

branchBlock :: BlockId -> Outcome BlockId -> BoolExpr -> Block
branchBlock id branch expr = Block id [] (Branch expr (success branch) (failure branch))

stmtBlock :: BlockId -> BlockId -> [Statement] -> Block
stmtBlock id dest content = Block id content (Goto dest)

finalBlock :: BlockId -> Bool -> Block
finalBlock id res = Block id [] (Final res)

data Outcome a = Outcome a a

success :: Outcome a -> a
success (Outcome s _) = s

failure :: Outcome a -> a
failure (Outcome _ f) = f

-- 2. Add main "start anywhere" loop

-- 3. Pretty print to C

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

toDotGraph :: Program -> DotGraph
toDotGraph program = Dot.fromAnyAbstractGraph dotNode edgeConf (toAbstractGraph $ programBlocks program)
  where
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
    contLabel (Goto dest) = Nothing
    contLabel (Final b) = Just $ if b then "success" else "failure"

    edgeConf :: (Block, Maybe Bool, Block) -> Dot.EdgeConfig
    edgeConf (_, Just True, _) = Dot.edgeEnd Dot.Normal
    edgeConf (_, Just False, _) = Dot.edgeEnd Dot.Box
    edgeConf (_, Nothing, _) = Dot.emptyConfig

-- Accessors

getBlockId :: Block -> BlockId
getBlockId (Block id _ _) = id

getContinuation :: Block -> Continuation
getContinuation (Block _ _ cont) = cont

programBlocks :: Program -> [Block]
programBlocks (Program _ blocks) = blocks


-- Deprecated: BlockIR extraction from BlockTree

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
