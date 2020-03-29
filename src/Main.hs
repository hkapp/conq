module Main where

import Parser
import RegexParser
import ParserTest

main:: IO ()
main = runAllTests
    --do
    --let str = "a|a|a"
        --parseResult = match str
    --putStrLn ((enclose str) ++ (explain parseResult))


enclose str = '"' : str ++ '"' : []

explain :: ParseResult t -> String
explain (Success t [])  = " isValidRegex completely"
explain (Success t rem) = " partly isValidRegex, rem=" ++ (enclose rem)
explain Failure         = " does not match"
