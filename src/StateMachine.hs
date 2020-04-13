module StateMachine where

import Data.Foldable (foldl')
import RegexOpTree

data StateMachine s i = StateMachine {
  startingState :: s,
  stateTransition :: s -> i -> s,
  acceptingState :: s -> Bool }


executeMachine :: StateMachine s i -> [i] -> s
executeMachine (StateMachine start transition _) = foldl' transition start

type CCode = String

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
