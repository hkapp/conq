module BlockIR where

import Utils

import qualified AbstractGraph as Abstract
import BlockTreeIR (BlockTree)
import qualified BlockTreeIR as BlockTree
import RegexOpTree (RegexOpTree(..))
import qualified RegexOpTree as Regex

import PrettyPrint (quoted, (%%), (+\\+), indent, properUnlines, concatWithSep)
import Dot (DotGraph)
import qualified Dot
import qualified CodeGen as C

import Control.Monad.Trans.State as State (State, evalState)
import qualified Control.Monad.Trans.State as State

import Data.Bool (bool)
import Data.Foldable (find)
import Data.Function ((&))
import qualified Data.List as List
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Semigroup (Semigroup, (<>))
import qualified Data.Set as Set

-- A control-flow oriented IR, which should cover everything

data Block = Block BlockId [Statement] Continuation
type BlockId = Int
data Statement = Advance Int | Init | StartMatch
  deriving Show
type BoolExpr = BlockTree.Expr
data Continuation = Branch BoolExpr UncondJump UncondJump | Uncond UncondJump | Final Bool
data UncondJump = Goto BlockId | Inline BlockId

instance Eq Block where
  b1 == b2 = getBlockId b1 == getBlockId b2

instance Ord Block where
  compare b1 b2 = compare (getBlockId b1) (getBlockId b2)

-- 1. Extract BlockIR from RegexOpTree

data Program = Program UncondJump [Block]

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

blockPattern :: Outcome UncondJump -> RegexOpTree -> State ProgramBuilder Program

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
branchAndAdvancePattern :: Outcome UncondJump -> BoolExpr -> Int -> State ProgramBuilder Program
branchAndAdvancePattern branch expr advanceCount = do
  exprBlockId <- allocateId
  advBlockId <- allocateId
  let exprBlock = branchBlock
                    exprBlockId
                    (Outcome (Goto advBlockId) (failure branch))
                    expr
  let advBlock = stmtBlock
                   advBlockId
                   (success branch)
                   [Advance advanceCount]
  return $ Program (Goto exprBlockId) [exprBlock, advBlock]
  

branchBlock :: BlockId -> Outcome UncondJump -> BoolExpr -> Block
branchBlock id branch expr = Block id [] (Branch expr (success branch) (failure branch))

stmtBlock :: BlockId -> UncondJump -> [Statement] -> Block
stmtBlock id dest content = Block id content (Uncond dest)

finalBlock :: BlockId -> Bool -> Block
finalBlock id res = Block id [] (Final res)

data Outcome a = Outcome a a

success :: Outcome a -> a
success (Outcome s _) = s

failure :: Outcome a -> a
failure (Outcome _ f) = f

-- 2. Lower the block IR

type Lowering = Program -> Program

lower :: Program -> Program
lower p = foldl (&) p lowerings

lowerings :: [Lowering]
lowerings = [
  startAnywhere,
  addInitStatement
  ]

-- 2a. Add main "start anywhere" loop
startAnywhere :: Program -> Program
startAnywhere basicProgram =
  let
    maxId = maximum (getBlockId <$> programBlocks basicProgram)
    newStartId = maxId + 1
    newFailureId = maxId + 2
    oldFailureBlock = findFinalBlock basicProgram False
    oldFailureId = getBlockId oldFailureBlock
    oldStartId = programStart basicProgram
    
    -- At the start of the program, check if there is more input
    --   If there is more input, start matching the basic program
    --   If there isn't, go to failure
    newStartOutcome = Outcome oldStartId newFailureId
    newStartBlock = Block
                      newStartId
                      [StartMatch]
                      (Branch
                        BlockTree.HasMoreInput
                        (Goto oldStartId)
                        (Goto newFailureId))
    
    -- On failure of the basic program, advance and loop back
    -- to checking if there's more input.
    -- Reuse the old failure block id to act as failure sink for
    -- the basic program.
    catchFailureBlock = stmtBlock oldFailureId (Goto newStartId) [Advance 1]
    
    -- Remap the old final failure's block id to not conflict with
    -- catchFailureBlock
    remapFinalId (Block id stmts (Final False)) =
      Block newFailureId stmts (Final False)
    remapFinalId b = b
    remappedFinalId = remapFinalId <$> programBlocks basicProgram
  in
    Program (Goto newStartId) (newStartBlock : catchFailureBlock : remappedFinalId)
    
-- 2b. Add INIT statement at the very beginning
-- Note: do not add the stmt in the start block, as it is looped over
addInitStatement :: Program -> Program
addInitStatement p = addNewStart initBlock p
  where initBlock = stmtBlock (Goto $ nextFreeId p) (programStart p) [Init]

-- 3. Optimize

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

-- 4. Pretty print to C

generateCode :: Program -> C.Code
generateCode p = wrapInTemplate fullGenCode
  where
    fullGenCode = (indent $ genGoto (programStart p)) +\\+ blocksCode
    blocksCode = properUnlines $ map generateBlock (programBlocks p)

wrapInTemplate :: C.Code -> C.Code
wrapInTemplate code = templatePrefix +\\+ code +\\+ templateSuffix
  where
    templatePrefix = properUnlines [
      "#include \"conq.h\"",
      "",
      "int main(int argc, char *argv[]) {"
      ]
    templateSuffix = "}\n"

generateBlock :: Block -> C.Code
generateBlock (Block id stmts cont) = label +\\+ indent blockCode
  where
    label = C.label (labelFor id)
    blockCode = if (null stmtsCode) then contCode else stmtsCode +\\+ contCode
    stmtsCode = properUnlines (generateStatement <$> stmts)
    contCode = generateCont cont

labelFor :: BlockId -> C.Code
labelFor id = "l" ++ show id

-- Each statement is generated as a C macro
generateStatement :: Statement -> C.Code
generateStatement (Advance n) = "ADVANCE(" %% n ++ ")"
generateStatement Init = "INIT"
generateStatement StartMatch = "START_MATCH"

generateCont :: Continuation -> C.Code

generateCont (Branch expr succId failId) =
  C.if_ (generateExpr expr)
    (genGoto succId)
    (genGoto failId)

generateCont (Goto id) = genGoto id

generateCont (Final True) = "FINAL_SUCCESS"
generateCont (Final False) = "FINAL_FAILURE"

genGoto :: BlockId -> C.Code
genGoto id = C.goto $ labelFor id

-- The result produced must be a one-line expression (fits into an if)
generateExpr :: BoolExpr -> C.Code

generateExpr (BlockTree.StringEq s) =
  "PREFIX_EQUALS(" ++ (show s) ++ ", " %% (length s) ++ ")"
  
generateExpr (BlockTree.FirstCharIn charclass) =
  let
    acceptedCharList = Set.toList charclass
    charPred c = "FIRST_CHAR == '" ++ (show c) ++ "'"
    allCharPreds = map charPred acceptedCharList
  in
    concatWithSep " && " allCharPreds

generateExpr BlockTree.HasMoreInput = "HAS_MORE_INPUT"

-- Accessors

getBlockId :: Block -> BlockId
getBlockId (Block id _ _) = id

getContinuation :: Block -> Continuation
getContinuation (Block _ _ cont) = cont

mapBlockStmts :: ([Statement] -> [Statement]) -> Block -> Block
mapBlockStmts f (Block id stmts cont) = Block id (f stmts) cont

programStart :: Program -> BlockId
programStart (Program id _) = id

programBlocks :: Program -> [Block]
programBlocks (Program _ blocks) = blocks

nextFreeId :: Program -> Int
nextFreeId p = maximum (getBlockId <$> programBlocks p) + 1

uncondDest :: UncondJump -> BlockId
uncondDest (Goto id) = id
uncondDest (Inline id) = id

findFinalBlock :: Program -> Bool -> Block
findFinalBlock p searchedVal =
  let
    pred (Final finalRes) = searchedVal == finalRes
    pred _ = False
    found = find (pred . getContinuation) (programBlocks p)
    notFoundError = error $ "Not found final block with result=" %% searchedVal
  in
    fromMaybe notFoundError found

mapProgramBlocks :: (Block -> Block) -> Program -> Program
mapProgramBlocks f (Program startId blocks) = Program startId (blocks <&> f)

mapBlockWithId :: BlockId -> (Block -> Block) -> Program -> Program
mapBlockWithId idToMap f = mapProgramBlocks f2
  where
    f2 block
      | (getBlockId block == idToMap) = f block
      | otherwise = block

mapStartBlock :: (Block -> Block) -> Program -> Program
mapStartBlock f p = mapBlockWithId (programStart p) f p

addNewStart :: Block -> Program -> Program
addNewStart newStartBlock p =
  Program (getBlockId newStartBlock) (newStartBlock : (programBlocks p))

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
    contLabel (Goto dest) = Nothing
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
    
    startId = programStart p
    startNodeFound = find (\n -> Dot.nodeId n == show startId) (Dot.allNodes dotGraph)
    startNode = fromMaybe (error "Start node not found") startNodeFound
    edgeConf = Dot.emptyConfig
    inputEdge = Dot.Edge inputNode startNode edgeConf
    graphWithInputEdge = Dot.addEdge graphWithInputNode inputEdge
  in
    graphWithInputEdge

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
