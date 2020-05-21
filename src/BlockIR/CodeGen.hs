module BlockIR.CodeGen where

import BlockIR.BlockIR
import qualified BlockIR.BlockTree as BlockTree

import qualified Utils.CodeGen as C
import Utils.PrettyPrint (quoted, (%%), (+\\+), indent, properUnlines, concatWithSep)
import qualified Utils.Set as SetUtils

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set, member)
import qualified Data.Set as Set

-- 4. Pretty print to C

generateCode :: Program -> C.Code
generateCode p = wrapInTemplate $ generateBody p

wrapInTemplate :: C.Code -> C.Code
wrapInTemplate code = templatePrefix +\\+ code +\\+ templateSuffix
  where
    templatePrefix = properUnlines [
      "#include \"conq.h\"",
      "",
      "int main(int argc, char *argv[]) {"
      ]
    templateSuffix = "}\n"

type BlockMap = Map BlockId Block

generateBody :: Program -> C.Code
generateBody p = let fastBlockAccess = blockMap p
                     startCode = generateStartCode p fastBlockAccess
                     blocksCode = generateBlocksCode p fastBlockAccess
                 in startCode +\\+ blocksCode

generateStartCode :: Program -> BlockMap -> C.Code
generateStartCode p blockMap = indent $ genJmp (programStart p) blockMap

generateBlocksCode :: Program -> BlockMap -> C.Code
generateBlocksCode p blockMap =
  properUnlines $
    map (\b -> generateBlockWithLabel b blockMap)
        (nonInlinedBlocks p)

generateBlockWithLabel :: Block -> BlockMap -> C.Code
generateBlockWithLabel block blockMap = label +\\+ indent blockCode
  where
    label = C.label (labelFor $ getBlockId block)
    blockCode = generateBlockContent block blockMap

generateBlockContent :: Block -> BlockMap -> C.Code
generateBlockContent (Block _ stmts cont) blockMap = blockCode
  where
    blockCode = if (null stmtsCode)
                  then contCode
                  else stmtsCode +\\+ contCode
    stmtsCode = properUnlines (generateStatement <$> stmts)
    contCode = generateCont cont blockMap

labelFor :: BlockId -> C.Code
labelFor id = "l" ++ show id

-- Each statement is generated as a C macro
generateStatement :: Statement -> C.Code
generateStatement (Advance n) = "ADVANCE(" %% n ++ ")"
generateStatement Init = "INIT"
generateStatement StartMatch = "START_MATCH"

generateCont :: Continuation -> BlockMap -> C.Code

generateCont (Branch expr succJmp failJmp) blockMap =
  C.if_ (generateExpr expr)
    (genJmp succJmp blockMap)
    (genJmp failJmp blockMap)

generateCont (Uncond jmp) blockMap = genJmp jmp blockMap

generateCont (Final True) _ = "FINAL_SUCCESS"
generateCont (Final False) _ = "FINAL_FAILURE"

genJmp :: UncondJump -> BlockMap -> C.Code
genJmp (Goto id) _ = C.goto $ labelFor id
genJmp (Inline id) blockMap = generateBlockContent (blockMap ! id) blockMap

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

-- Add support for inlined blocks

nonInlinedBlocks :: Program -> [Block]
nonInlinedBlocks p = let nonInlinedIds = allGotoDests p
                         blockIsNeverInlined b = getBlockId b `member` nonInlinedIds
                     in filter blockIsNeverInlined (programBlocks p)

allGotoDests :: Program -> Set BlockId
allGotoDests p = foldr addGotoDest Set.empty (allJumps p)

addGotoDest :: UncondJump -> Set BlockId -> Set BlockId
addGotoDest (Goto destId) s = Set.insert destId s
addGotoDest (Inline _) s = s
