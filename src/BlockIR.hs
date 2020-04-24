module BlockIR where

import Data.Foldable (foldl')
import RegexOpTree
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Semigroup (Semigroup, (<>))
import Parser
import Utils

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
data Expr = StringEq String | FirstCharIn (Set Char)

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


-- Evaluating the simple tree-based IR

-- evalIRTree :: BlockTree -> String -> Maybe String
-- evalIRTree = partiallyParseString . treeBasedParser

-- treeBasedParser :: BlockTree -> Parser String
-- treeBasedParser (BlockNode exp success failure) = (treeExpParser exp) wireTrue success wireFalse failure
-- treeBasedParser FinalSuccess = Parser.alwaysSucceed ""
-- treeBasedParser FinalFailure = Parser.alwaysFail

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

(%%) :: (Show s) => String -> s -> String
pre %% o = pre ++ (show o)

quoted :: (Show s) => s -> String
quoted o = '"' : (show o) ++ "\""

commaSeparated :: (Foldable t, Show s) => t s -> String
commaSeparated = foldr commaSep []
  where
    commaSep s [] = (show s)
    commaSep prefix suffix = (show prefix) ++ ", " ++ suffix

showAll :: (Functor f, Show s) => f s -> f String
showAll = fmap show

indent :: String -> String
indent text = properUnlines $ map (\l -> "  " ++ l) (lines text)

properUnlines :: [String] -> String
properUnlines = concatWithSep "\n"

concatWithSep :: String -> [String] -> String
concatWithSep = mconcatWithSep

mconcatWithSep :: (Semigroup m, Foldable t) => m -> t m -> m
mconcatWithSep msep = foldr1 (\prefix -> \suffix -> prefix <> msep <> suffix)
