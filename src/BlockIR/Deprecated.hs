module BlockIR.Deprecated where

-- import BlockIR.BlockTree (BlockTree)
-- import qualified BlockIR.BlockTree as BlockTree

-- import RegexIR.RegexOpTree (RegexOpTree(..))
-- import qualified RegexIR.RegexOpTree as Regex

-- import qualified Utils.AbstractGraph as Abstract
-- import Utils.PrettyPrint (quoted, (%%), (+\\+), indent, properUnlines, concatWithSep)
-- import Utils.Dot (DotGraph)
-- import qualified Utils.Dot as Dot
-- import qualified Utils.CodeGen as C
-- import Utils.Prelude
-- import Utils.Map
-- import Utils.List

-- import Control.Monad.Trans.State as State (State, evalState)
-- import qualified Control.Monad.Trans.State as State

-- import Data.Bool (bool)
-- import Data.Foldable (find)
-- import Data.Function ((&))
-- import qualified Data.List as List
-- import Data.Map (Map, (!))
-- import qualified Data.Map as Map
-- import Data.Maybe (fromMaybe)
-- import Data.Semigroup (Semigroup, (<>))
-- import qualified Data.Set as Set

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
    thisBlock = Block id [] (Branch expr
                                    (Goto $ childId succChild)
                                    (Goto $ childId failChild))
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
