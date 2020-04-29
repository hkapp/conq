module Main where

import Utils

import Test (runAllTests)
import RegexOpTree (RegexOpTree(..))
import qualified BlockIR
import qualified RegexParser
import qualified Dot

import Control.Applicative
import Data.Foldable (traverse_)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import System.Environment
import System.IO

main:: IO ()
main = getArgs <&> (parseFlags allFlags) >>= (handleCommands allFlags)

handleCommands commands config = traverse_ execCommand commands
  where execCommand (Flag cmdName _ _ cmdExec) =
          if (Map.member cmdName config) then cmdExec config else return ()

allFlags :: Map String Flag
allFlags = fromListWithKey flagName [
  action0 "--test" runAllTests,
  actionWithConfig1 "--generate" generateFromConfig,
  option1 "--output-dir" `withDefault` "../gen",
  action0 "--print" printHardcoded
  ]

-- Possible Actions

generateFromConfig :: Map String String -> IO ()
generateFromConfig config =
  let
    filename = (config ! "--output-dir") ++ "/be.c"
    regexDef = (config ! "--generate")
    maybeIRTree = RegexParser.parseRegex regexDef <&> BlockIR.buildIRTree
  in
    foldMap (printTreeToFile filename) maybeIRTree

printTreeToFile filename tree = do
  outFile <- openFile filename WriteMode
  hPutStr outFile (BlockIR.printCTree tree)
  hClose outFile

printHardcoded :: IO ()
printHardcoded = putStrLn $ Dot.prettyPrint (BlockIR.toDotGraph (BlockIR.buildIRTree thisRegexTree))
  where thisRegexTree = RegexAlternative
                          (RegexAlternative
                            (RegexString "a")
                            (RegexString "b"))
                          (RegexString "c")

-- Command Line Parser

parseFlags :: Map String Flag -> [String] -> Map String String
parseFlags knownFlags args = recParse (defaults, args)
  where
    defaults = Map.mapMaybe flagDefault knownFlags
    recParse (parsedArgs, (fName : remArgs)) =
      recParse $ handleFlag (knownFlags ! fName) remArgs parsedArgs
    recParse (parsedArgs, []) = parsedArgs
    handleFlag (Flag flagName flagArgCount _ _) remArgs parsedArgs
      | (length remArgs >= flagArgCount) = (Map.insert flagName parsedVal parsedArgs, argsAfterParse)
      | otherwise = error $ "Not enough arguments left after flag \"" ++ flagName ++ "\""
      where (parsedVal, argsAfterParse) = parseArg flagArgCount remArgs
    parseArg 0 argsToParse = ("", argsToParse)
    parseArg 1 (val : argsLeft) = (val, argsLeft)
    parseArg 1 _ = error "Not enough arguments left after flag1"
    parseArg c _ = error $ "Unsupported flag count: " ++ (show c)

-- handleActions :: IO [Action] -> IO ()
-- runAllTests

-- getActions :: IO [Action]
-- getActions = buildActions getArgs
  -- where
    -- buildActions ("--test" : remArgs) = runAllTests
    -- buildActions ()

fromListWithKey :: (Ord k) => (a->k) -> [a] -> Map k a
fromListWithKey f = Map.fromList . map (\x -> (f x, x))

data Flag = Flag String Int (Maybe String) (Map String String -> IO ())

action0 name io = Flag name 0 Nothing (const io)

actionWithConfig1 name f = Flag name 1 Nothing f

option1 name = Flag name 1 Nothing (const $ pure ())

withDefault :: Flag -> String -> Flag
withDefault (Flag name nargs _ f) defaultVal =
  Flag name nargs (Just defaultVal) f

flagName :: Flag -> String
flagName (Flag name _ _ _) = name

flagDefault :: Flag -> Maybe String
flagDefault (Flag _ _ x _) = x
