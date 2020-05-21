module ArgParse where

import Utils.PrettyPrint ((%%), quoted, enclosed)

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, fromMaybe)

type ParsedArgs = [(Flag, [String])]

parseFlags :: Map String Flag -> [String] -> ParsedArgs

parseFlags knownFlags (flagArg : nextArgs) =
  case Map.lookup flagArg knownFlags of
    Just(flag) ->
      let (flagParams, remArgs) = splitAccordingTo flag nextArgs
      in (flag, flagParams) : (parseFlags knownFlags remArgs)
    Nothing ->
      error $ "Unknown flag " ++ quoted flagArg

parseFlags _ [] = []

splitAccordingTo :: Flag -> [a] -> ([a], [a])
splitAccordingTo flag xs
  | (length xs < argsRequired) = error $
      "Not enough arguments to parse " ++ quoted (flagName flag)
      ++ ": needed " %% argsRequired ++ ", found " %% (length xs)
  | otherwise = splitAt argsRequired xs
  where argsRequired = flagArgCount flag

toConfig :: ParsedArgs -> Config
toConfig parsedArgs = Map.fromList [ (flagName flag , singleParam flag params) | (flag, params) <- parsedArgs, isOption flag ]
  where
    singleParam :: Flag -> [String] -> String
    singleParam flag argVals = case flagArgCount flag of
      0 | (null argVals) -> []
      1 | (length argVals == 1) -> head argVals
      _ -> error $
            "Invalid number of arguments for " ++
            quoted (flagName flag) ++ " -> " %% argVals

type PreparedAction = (Action, [String])

extractActions :: ParsedArgs -> [PreparedAction]
extractActions parsedArgs = mapMaybe tryExtractingAction parsedArgs
  where
    tryExtractingAction (flag, params) = case flagGoal flag of
                                           Do f -> Just (f, params)
                                           Option -> Nothing

execActions :: [PreparedAction] -> Config -> [IO ()]
execActions actions config = [ f params config | (f, params) <- actions ]


-- Flag API

data Flag = Flag String Int Goal
data Goal = Do Action | Option
type Action = [String] -> Config -> IO ()
type Config = Map String String

command name io = Flag name 0 (Do $ const io)

option name = Flag name 1 Option

toggle name = Flag name 0 Option

flagName :: Flag -> String
flagName (Flag name _ _) = name

flagArgCount :: Flag -> Int
flagArgCount (Flag _ argCount _) = argCount

isOption :: Flag -> Bool
isOption flag = case flagGoal flag of
                  Option -> True
                  _ -> False

flagGoal :: Flag -> Goal
flagGoal (Flag _ _ goal) = goal
