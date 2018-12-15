module Main where

import Parser

main:: IO ()
main = do
    -- putStrLn "Regular expression:"
    -- line <- getLine                                     -- line :: String
    -- putStrLn ("Your regular expression: " ++ line)
    let str = "[aa]a"
    putStr (enclose str)
    putStrLn $ case parse str of
      Match [] -> " matches completely"
      Match x -> " partly matches, rem=" ++ enclose x
      Failure -> " does not match"
    -- if matches str
      -- then putStrLn " matches"
      -- else putStrLn " does not match"

enclose str = '"' : str ++ '"' : []
