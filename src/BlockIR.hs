module BlockIR where

import Data.Foldable (foldl')
import RegexOpTree
import Data.Set (Set)


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



type CCode = String

printCTree :: BlockTree -> CCode
printCTree (BlockNode e sc fl) = c_if (printCExpr e) (printCTree sc) (printCTree fl)
printCTree FinalSuccess = "success!"
printCTree FinalFailure = "failure"

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


printC :: CCode -> CCode -> RegexOpTree -> CCode

printC contSuccess contFailure (RegexString s) =
  c_if (c_strncmp "curr_pos" s (length s))
    contSuccess
    contFailure

printC contSuccess contFailure (RegexCharClass charclass) = undefined

printC contSuccess contFailure (RegexSequence nodes) = foldr continueExec contSuccess nodes
  where continueExec regexNode successContinuation = printC successContinuation contFailure regexNode

printC contSuccess contFailure (RegexAlternative left right) = printC contSuccess tryRight left
  where tryRight = printC contSuccess contFailure right

c_if :: CCode -> CCode -> CCode -> CCode
c_if cond thenBranch elseBranch = unlines [
  "if (" ++ cond ++ ") {",
  thenBranch,
  "}",
  "else {",
  elseBranch,
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
