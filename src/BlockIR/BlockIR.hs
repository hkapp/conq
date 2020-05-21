module BlockIR.BlockIR where

import qualified BlockIR.BlockTree as BlockTree

import Utils.Prelude
import Utils.PrettyPrint ((%%))

import Data.Foldable (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

-- Types

data Program = Program
                 UncondJump
                 [Block]

data Block = Block
               BlockId
               [Statement]
               Continuation

type BlockId = Int

data Statement =
  Advance Int
  | Init
  | StartMatch
  deriving Show

type BoolExpr = BlockTree.Expr

data Continuation =
  Branch BoolExpr UncondJump UncondJump
  | Uncond UncondJump
  | Final Bool

data UncondJump =
  Goto BlockId
  | Inline BlockId

-- Program

programStart :: Program -> UncondJump
programStart (Program start _) = start

programStartId :: Program -> BlockId
programStartId = uncondDest . programStart

programBlocks :: Program -> [Block]
programBlocks (Program _ blocks) = blocks

nextFreeId :: Program -> Int
nextFreeId p = maximum (getBlockId <$> programBlocks p) + 1

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
mapStartBlock f p = mapBlockWithId (programStartId p) f p

addNewStart :: Block -> Program -> Program
addNewStart newStartBlock p =
  Program
    (Goto $ getBlockId newStartBlock)
    (newStartBlock : (programBlocks p))

allContinuations :: Program -> [Continuation]
allContinuations p = getContinuation <$> programBlocks p

allJumps :: Program -> [UncondJump]
allJumps p = startJump : blockJumps
  where startJump = programStart p
        blockJumps = allContinuations p >>= contJumps

blockMap :: Program -> Map BlockId Block
blockMap p = Map.fromList $ map (\b -> (getBlockId b, b)) (programBlocks p)

-- Block

getBlockId :: Block -> BlockId
getBlockId (Block id _ _) = id

getContinuation :: Block -> Continuation
getContinuation (Block _ _ cont) = cont

mapBlockStmts :: ([Statement] -> [Statement]) -> Block -> Block
mapBlockStmts f (Block id stmts cont) = Block id (f stmts) cont

-- Block instances

instance Eq Block where
  b1 == b2 = getBlockId b1 == getBlockId b2

instance Ord Block where
  compare b1 b2 = compare (getBlockId b1) (getBlockId b2)

-- Continuation

contJumps :: Continuation -> [UncondJump]
contJumps (Branch _ succJmp failJmp) = [succJmp, failJmp]
contJumps (Uncond jmp) = [jmp]
contJumps (Final _) = []

-- UncondJump

uncondDest :: UncondJump -> BlockId
uncondDest (Goto id) = id
uncondDest (Inline id) = id
