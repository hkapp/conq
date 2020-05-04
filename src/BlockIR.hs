module BlockIR where

import Utils

import qualified AbstractGraph as Abstract
import BlockTreeIR (BlockTree)
import qualified BlockTreeIR as BlockTree

import PrettyPrint (quoted)
import Dot (DotGraph)
import qualified Dot

import Data.Foldable (find)
import Data.Map (Map, (!))

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

toBlockList :: BlockTree -> [Block]
toBlockList = noDuplicates . fromAbstractTreeWithId . BlockTree.toAbstractTreeWithId

fromAbstractTreeWithId :: Abstract.Tree (Either Bool BlockTree.Expr, Int) Bool -> [Block]

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
