module BlockIR.CodeGen where

import BlockIR.BlockIR

-- import BlockIR.BlockTree (BlockTree)
import qualified BlockIR.BlockTree as BlockTree

-- import RegexIR.RegexOpTree (RegexOpTree(..))
-- import qualified RegexIR.RegexOpTree as Regex

-- import qualified Utils.AbstractGraph as Abstract
import Utils.PrettyPrint (quoted, (%%), (+\\+), indent, properUnlines, concatWithSep)
-- import Utils.Dot (DotGraph)
-- import qualified Utils.Dot as Dot
import qualified Utils.CodeGen as C
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
import qualified Data.Set as Set

-- 4. Pretty print to C

generateCode :: Program -> C.Code
generateCode p = wrapInTemplate fullGenCode
  where
    fullGenCode = (indent $ genJmp (programStart p)) +\\+ blocksCode
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

generateCont (Branch expr succJmp failJmp) =
  C.if_ (generateExpr expr)
    (genJmp succJmp)
    (genJmp failJmp)

generateCont (Uncond jmp) = genJmp jmp

generateCont (Final True) = "FINAL_SUCCESS"
generateCont (Final False) = "FINAL_FAILURE"

genJmp :: UncondJump -> C.Code
genJmp (Goto id) = C.goto $ labelFor id

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
