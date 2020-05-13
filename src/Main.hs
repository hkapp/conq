module Main where

import Utils

import Test (runAllTests)
import RegexOpTree (RegexOpTree(..))
import qualified BlockIR
import qualified BlockTreeIR as BlockTree
import qualified RegexParser
import Dot (DotGraph)
import qualified Dot
import PrettyPrint ((%%), quoted, enclosed)

import Control.Applicative
import Data.Foldable (traverse_)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, fromMaybe)
import System.Environment
import System.IO

main:: IO ()
main = getArgs >>= handleArgs

handleArgs commandLineArgs =
  let
    parsedArgs = parseFlags allFlags commandLineArgs
    config = toConfig parsedArgs
    actions = extractActions parsedArgs
    actionResults = execActions actions config
  in sequence_ actionResults

allFlags :: Map String Flag
allFlags = mapFromValues flagName [
  command "--test" (const runAllTests),
  command "--generate" generateCCode,
  option "--output-dir",
  command "--dot" printDot,
  option "--regex",
  option "--phase",
  toggle "--hardcoded"
  ]

-- Possible Actions

generateCCode :: Config -> IO ()
generateCCode config =
  let
    filename = generateFileName config ".c"
    ir = getBlockIR config
    code = BlockIR.generateCode ir
  in
    writeToFile filename code

writeToFile :: FilePath -> String -> IO ()
writeToFile filename text = do
  outFile <- openFile filename WriteMode
  hPutStr outFile text
  hClose outFile
  putStrLn $ "Wrote " %% (length $ lines text) ++ " lines to " ++ (enclosed "'" filename)

printDot :: Config -> IO ()
printDot config = writeToFile (generateFileName config ".dot") dotCode
  where
    dotCode = Dot.prettyPrint (getDotGraph config)

-- Config items

getOutputDir :: Config -> FilePath
getOutputDir = Map.findWithDefault "../gen" "--output-dir"

generateFileName :: Config -> String -> FilePath
generateFileName config extension = (getOutputDir config) ++ "/be" ++ extension

getRegex :: Config -> String
getRegex config = case Map.lookup "--regex" config of
                    Just r -> r
                    Nothing -> error $ "No regex was passed as argument. Use the \"--regex\" flag."

getRegexOpTree :: Config -> RegexOpTree
getRegexOpTree config =
  if Map.member "--hardcoded" config
    then hardcodedRegexTree
    else fromMaybe parsingError (RegexParser.parseRegex regex)
          where
            regex = getRegex config
            parsingError = error $ "Not a valid regex: " ++ quoted regex

hardcodedRegexTree :: RegexOpTree
hardcodedRegexTree =
  RegexSequence [
    (RegexString "a"),
    (RegexString "b"),
    (RegexString "c")
    ]

getBlockTree :: Config -> BlockTree.BlockTree
getBlockTree config = BlockTree.buildIRTree (getRegexOpTree config)

-- getBlockIR :: Config -> [BlockIR.Block]
-- getBlockIR config = BlockIR.toBlockList (getBlockTree config)

getBlockIRAtLevel :: Int -> Config -> BlockIR.Program
getBlockIRAtLevel 1 config = BlockIR.fromRegexOpTree (getRegexOpTree config)
getBlockIRAtLevel 2 config = BlockIR.lower (getBlockIRAtLevel 1 config)

defaultBlockIRLevel = 2

getBlockIR :: Config -> BlockIR.Program
getBlockIR = getBlockIRAtLevel defaultBlockIRLevel

data Phase = RegexOpTree | BlockTree | BlockIR Int

getPhase :: Config -> Phase
getPhase config = case Map.lookup "--phase" config of
  Just "RegexOpTree" -> RegexOpTree
  Just "BlockTree" -> BlockTree
  Just "BlockIR" -> defaultBlockIR
  Just "BlockIR1" -> BlockIR 1
  Just "BlockIR2" -> BlockIR 2
  Nothing -> defaultBlockIR
  Just phase -> error $ "Unknown phase " ++ quoted phase
  where defaultBlockIR = BlockIR defaultBlockIRLevel

getDotGraph :: Config -> DotGraph
getDotGraph config = case getPhase config of
  RegexOpTree -> error $ "Dot not support for RegexOpTree"
  BlockTree -> BlockTree.toDotGraph (getBlockTree config)
  BlockIR n -> BlockIR.toDotGraph (getBlockIRAtLevel n config)

-- Command Line Parser

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
