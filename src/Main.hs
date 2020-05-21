module Main where

import ArgParse

import Test.TestMain (runAllTests)

import qualified BlockIR.BlockIR as BlockIR
import qualified BlockIR.BlockTree as BlockTree
import qualified Parser.RegexParser as RegexParser
import RegexIR.RegexOpTree (RegexOpTree(..))

import Utils.Dot (DotGraph)
import qualified Utils.Dot as Dot
import Utils.PrettyPrint ((%%), quoted, enclosed)
import Utils.Map (mapFromValues)

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
getBlockIRAtLevel 3 config = BlockIR.optimize (getBlockIRAtLevel 2 config)

defaultBlockIRLevel = 3

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
  Just "BlockIR3" -> BlockIR 3
  Nothing -> defaultBlockIR
  Just phase -> error $ "Unknown phase " ++ quoted phase
  where defaultBlockIR = BlockIR defaultBlockIRLevel

getDotGraph :: Config -> DotGraph
getDotGraph config = case getPhase config of
  RegexOpTree -> error $ "Dot not support for RegexOpTree"
  BlockTree -> BlockTree.toDotGraph (getBlockTree config)
  BlockIR n -> BlockIR.toDotGraph (getBlockIRAtLevel n config)
