module BlockIR.Construct where

import BlockIR.BlockIR

-- import BlockIR.BlockTree (BlockTree)
import qualified BlockIR.BlockTree as BlockTree

import RegexIR.RegexOpTree (RegexOpTree(..))
import qualified RegexIR.RegexOpTree as Regex

-- import qualified Utils.AbstractGraph as Abstract
-- import Utils.PrettyPrint (quoted, (%%), (+\\+), indent, properUnlines, concatWithSep)
-- import Utils.Dot (DotGraph)
-- import qualified Utils.Dot as Dot
-- import qualified Utils.CodeGen as C
-- import Utils.Prelude
-- import Utils.Map
-- import Utils.List
import Utils.Outcome (Outcome(..), success, failure)

import Control.Monad.Trans.State as State (State, evalState)
import qualified Control.Monad.Trans.State as State

-- import Data.Bool (bool)
-- import Data.Foldable (find)
-- import Data.Function ((&))
-- import qualified Data.List as List
-- import Data.Map (Map, (!))
-- import qualified Data.Map as Map
-- import Data.Maybe (fromMaybe)
-- import Data.Semigroup (Semigroup, (<>))
-- import qualified Data.Set as Set

-- 1. Extract BlockIR from RegexOpTree

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
  let finalOutcome = Outcome (Goto succId) (Goto failId)
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
