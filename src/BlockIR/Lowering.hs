module BlockIR.Lowering where

import BlockIR.BlockIR

-- import BlockIR.BlockTree (BlockTree)
import qualified BlockIR.BlockTree as BlockTree

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
import Utils.Outcome (Outcome(..), success, failure)
import BlockIR.Construct (stmtBlock)

-- import Control.Monad.Trans.State as State (State, evalState)
-- import qualified Control.Monad.Trans.State as State

-- import Data.Bool (bool)
-- import Data.Foldable (find)
import Data.Function ((&))
-- import qualified Data.List as List
-- import Data.Map (Map, (!))
-- import qualified Data.Map as Map
-- import Data.Maybe (fromMaybe)
-- import Data.Semigroup (Semigroup, (<>))
-- import qualified Data.Set as Set

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
    oldStart = programStart basicProgram
    
    -- At the start of the program, check if there is more input
    --   If there is more input, start matching the basic program
    --   If there isn't, go to failure
    newStartOutcome = Outcome oldStart (Goto newFailureId)
    newStartBlock = Block
                      newStartId
                      [StartMatch]
                      (Branch
                        BlockTree.HasMoreInput
                        oldStart
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
  where initBlock = stmtBlock (nextFreeId p) (programStart p) [Init]
