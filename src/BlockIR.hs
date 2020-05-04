module BlockIR where

import qualified AbstractGraph as Abstract
import BlockTreeIR (BlockTree)
import qualified BlockTreeIR as BlockTree

import PrettyPrint (quoted)

import Data.Foldable (find)

-- A control-flow oriented IR, which should cover everything

data Block = Block BlockId [Statement] Continuation
type BlockId = Int
type Statement = ()  -- for now we don't have any
type BoolExpr = BlockTree.Expr
data Continuation = Branch BoolExpr BlockId BlockId | Goto BlockId | Final Bool

-- data Program = Program BlockId (Map BlockId Block) [Decl]
-- type Decl = String

toBlockList :: BlockTree -> [Block]
toBlockList = fromAbstractTreeWithId . BlockTree.toAbstractTreeWithId

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
