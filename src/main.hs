module Main where

main:: IO ()
main = do
    -- putStrLn "Regular expression:"
    -- line <- getLine                                     -- line :: String
    -- putStrLn ("Your regular expression: " ++ line)
    let str = "[aa"
    putStr (enclose str)
    putStrLn $ case parse str of
      Match [] -> " matches completely"
      Match x -> " partly matches, rem=" ++ enclose x
      Failure -> " does not match"
    -- if matches str
      -- then putStrLn " matches"
      -- else putStrLn " does not match"

enclose str = '"' : str ++ '"' : []

-- Parser code here

data ParseResult = Match String | Failure
type Parser = String -> ParseResult

combine :: [Parser] -> Parser
combine (p : ps) input = case p input of
  Match s -> Match s
  Failure -> combine ps input
combine [] input = Failure

matches input = case parse input of
  Match [] -> True
  _ -> False

parse = combine [rep anyletter, parseopt]

parseopt input = 
  if length input < 2
  then Failure
  else
    case (head input, last input) of
      ('[', ']') -> parse (init (tail input))
      _ -> Failure


anyletter input = case input of
  c : rem | isletter c -> Match rem
  _ -> Failure
  
isletter c = c >= 'a' && c <= 'z'

rep subexp input =
  if null input
    then Match input
    else
      case subexp input of
        Match rem -> rep subexp rem
        Failure -> Failure -- Match input
