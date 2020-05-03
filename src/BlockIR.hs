module BlockIR where

import Utils

import Parser (Parser(..), partiallyParseString, (@>))
import qualified Parser
import RegexOpTree (RegexOpTree(..))
import qualified Dot
import PrettyPrint (indent, commaSeparated, (%%))
import qualified AbstractGraph as Abstract
import Dot (DotGraph)
import qualified Dot

import Data.Foldable (foldl')
import Data.Semigroup (Semigroup, (<>))
import Data.Set (Set)
import qualified Data.Set as Set

-- data Block = Block BlockId [Statement] Expr Continuation
-- type BlockId = Int
-- type Statement = ()  -- for now we don't have any
-- data Expr = StringEq String | FirstCharIn (Set Char)
-- type Continuation = (Bool -> BlockId)

-- data Program = Program BlockId (Map BlockId Block) [Decl]
-- type Decl = String

-- type CCode = String

-- data ProgramBuilder = ProgramBuilder BlockId Continuation Program

-- First, a simple tree-based IR

data BlockTree = BlockNode Expr BlockTree BlockTree | FinalSuccess | FinalFailure
  deriving Show
data Expr = StringEq String | FirstCharIn (Set Char)
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


-- Second, a more complete IR that supports everything we need

-- Dot utilities

toAbstractGraph :: BlockTree -> Abstract.Graph (Either Bool Expr, Int) Bool
toAbstractGraph tree = Abstract.graphFromTree abstractTreeWithId
  where
    abstractTreeWithId = Abstract.assignTreeIds abstractTree
    abstractTree = Abstract.buildTree toVertex childEdges tree
  
    toVertex :: BlockTree -> Either Bool Expr
    toVertex (BlockNode exp _ _) = Right exp
    toVertex FinalSuccess = Left True
    toVertex FinalFailure = Left False
    
    childEdges :: BlockTree -> [(Bool, BlockTree)]
    childEdges (BlockNode _ succChild failChild) = [(True, succChild), (False, failChild)]
    childEdges FinalSuccess = []
    childEdges FinalFailure = []

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

type CCode = String

printCTree :: BlockTree -> CCode
printCTree (BlockNode e sc fl) = c_if (printCExpr e) (printCTree sc) (printCTree fl)
printCTree FinalSuccess = c_global_success
printCTree FinalFailure = c_global_failure

printCExpr :: Expr -> CCode
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

c_global_success :: String
c_global_success = "goto regex_success;"

c_global_failure :: String
c_global_failure = "goto regex_failure;"

c_if :: CCode -> CCode -> CCode -> CCode
c_if cond thenBranch elseBranch = unlines [
  "if (" ++ cond ++ ") {",
  indent thenBranch,
  "}",
  "else {",
  indent elseBranch,
  "}"
  ]

c_fcall :: CCode -> [CCode] -> CCode
c_fcall fName fArgs = fName ++ "(" ++ (commaSeparated fArgs) ++ ")"

c_strncmp :: String -> String -> Int -> String
c_strncmp testedString expectedString strLen = c_fcall "strncmp" [testedString, expectedString, (show strLen)]
