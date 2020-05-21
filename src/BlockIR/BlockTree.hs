module BlockIR.BlockTree where

import Utils.Prelude

import Parser.Combinatorics (Parser(..), partiallyParseString, (@>))
import qualified Parser.Combinatorics as Parser
import RegexIR.RegexOpTree (RegexOpTree(..))

import Utils.Dot (DotGraph)
import qualified Utils.Dot as Dot
import Utils.PrettyPrint (indent, commaSeparated, (%%), quoted)
import qualified Utils.AbstractGraph as Abstract
import Utils.CodeGen as C
import Utils.Set

import Data.Foldable (foldl', find)
import Data.Maybe (maybe)
import Data.Semigroup (Semigroup, (<>))
import Data.Set (Set)
import qualified Data.Set as Set

-- First, a simple tree-based IR

data BlockTree = BlockNode Expr BlockTree BlockTree | FinalSuccess | FinalFailure
  deriving Show
data Expr = StringEq String | FirstCharIn (Set Char) | HasMoreInput
  deriving (Show, Eq, Ord)

buildIRTree :: RegexOpTree -> BlockTree
buildIRTree regex = buildIR regex FinalSuccess FinalFailure

buildIR :: RegexOpTree -> BlockTree -> BlockTree -> BlockTree

buildIR (RegexString s) sc fl = BlockNode (StringEq s) sc fl

buildIR (RegexCharClass charclass) sc fl = BlockNode (FirstCharIn charclass) sc fl

buildIR (RegexSequence nodes) sc fl = foldr sequentialExec sc nodes
  where sequentialExec regexNode successCont = buildIR regexNode successCont fl

buildIR (RegexAlternative left right) sc fl = buildIR left sc tryRight
  where tryRight = buildIR right sc fl

-- Dot utilities

type AbstractNode = Either Bool Expr

toAbstractTree :: BlockTree -> Abstract.Tree AbstractNode Bool
toAbstractTree blockTree = Abstract.buildTree toVertex childEdges blockTree
  where
    toVertex :: BlockTree -> Either Bool Expr
    toVertex (BlockNode exp _ _) = Right exp
    toVertex FinalSuccess = Left True
    toVertex FinalFailure = Left False

    childEdges :: BlockTree -> [(Bool, BlockTree)]
    childEdges (BlockNode _ succChild failChild) = [(True, succChild), (False, failChild)]
    childEdges FinalSuccess = []
    childEdges FinalFailure = []

toAbstractTreeWithId :: BlockTree -> Abstract.Tree (AbstractNode, Int) Bool
toAbstractTreeWithId = Abstract.assignTreeIds . toAbstractTree

toAbstractGraph :: BlockTree -> Abstract.Graph (AbstractNode, Int) Bool
toAbstractGraph = removeDuplicateFinalNodes . Abstract.graphFromTree . toAbstractTreeWithId

removeDuplicateFinalNodes :: Abstract.Graph (AbstractNode, Int) Bool -> Abstract.Graph (AbstractNode, Int) Bool
removeDuplicateFinalNodes g =
  let
    expect thisValue (Left thatValue, _) = thisValue == thatValue
    expect _ (Right _, _) = False
    findFinal b = find (expect b) (Abstract.vertices g)
    findFinalId b = maybe 0 snd (findFinal b)
    replaceId (Left b, _) = (Left b, findFinalId b)
    replaceId anythingElse = anythingElse
  in
    Abstract.eliminateDuplicateVertices (Abstract.mapVertices replaceId g)

toDotGraph :: BlockTree -> DotGraph
toDotGraph = Dot.fromAnyAbstractGraph toDotNode toDotEdgeConfig . toAbstractGraph
  where
    toDotNode :: (Either Bool Expr, Int) -> Dot.Node
    toDotNode ((Right exp), id) = Dot.nodeWithLabel (show id) (show exp)
    toDotNode ((Left True), id) = Dot.nodeWithLabel (show id) "success!"
    toDotNode ((Left False), id) = Dot.nodeWithLabel (show id) "failure"

    toDotEdgeConfig = const Dot.emptyConfig

-- Evaluating the simple tree-based IR

evalIRTree :: BlockTree -> String -> Maybe String
evalIRTree t = partiallyParseString $ Parser.canStartAnywhere (treeBasedParser t)

treeBasedParser :: BlockTree -> Parser String
treeBasedParser (BlockNode exp successBranch failureBranch) =
  Parser.parserBranch
    (treeBasedParser failureBranch)
    (\expMatch -> treeBasedParser successBranch <&> (\succBranchMatch -> expMatch ++ succBranchMatch))
    (treeExpParser exp)
treeBasedParser FinalSuccess = Parser.alwaysSucceed ""
treeBasedParser FinalFailure = Parser.alwaysFail

treeExpParser :: Expr -> Parser String
treeExpParser (StringEq s) = Parser.exactPrefix s
treeExpParser (FirstCharIn allowed) = Parser.parseOneChar (belongsTo allowed) <&> pure

-- Printing the simple tree-based IR

printCTree :: BlockTree -> C.Code
printCTree (BlockNode e sc fl) = C.if_ (printCExpr e) (printCTree sc) (printCTree fl)
printCTree FinalSuccess = c_global_success
printCTree FinalFailure = c_global_failure

printCExpr :: Expr -> C.Code
printCExpr (StringEq s) = "compare(" ++ s ++ ")"
printCExpr (FirstCharIn s) = "firstCharIn(" %% s ++ ")"

printPseudoTree :: String -> BlockTree -> String
printPseudoTree indentation node = indentation ++
  case node of
    BlockNode e sc fl ->
      (printCExpr e) ++ (printSubtree sc) ++ (printSubtree fl)
        where printSubtree t = '\n' : (printPseudoTree ("  " ++ indentation) t)
    FinalSuccess -> "success!"
    FinalFailure -> "failure"
