module Main where

import Parser
import RegexParser

main:: IO ()
main = do
    -- putStrLn "Regular expression:"
    -- line <- getLine                                     -- line :: String
    -- putStrLn ("Your regular expression: " ++ line)
    let str = "[aa]a"
        parseResult = match str
    putStrLn ((enclose str) ++ (explain parseResult))


enclose str = '"' : str ++ '"' : []

explain :: ParseResult t -> String
explain (Success t [])  = " matches completely"
explain (Success t rem) = " partly matches, rem=" ++ (enclose rem)
explain Failure         = " does not match"
