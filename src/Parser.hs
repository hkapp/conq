module Parser where

-- Types

type Parser = String -> ParseResult

data ParseResult = Match String | Failure

data TreeBuilder tree = TB {
  buildChar      :: Char -> tree,
  buildCharClass :: [Char] -> tree
}


-- Exported functions

parse :: Parser
parse = anymatch [rep anyletter, charclass]

matches :: String -> Bool
matches input = case parse input of
  Match [] -> True
  _ -> False


-- Parser combinators

anymatch :: [Parser] -> Parser

anymatch (p : ps) input = case p input of
  Match s -> Match s
  Failure -> anymatch ps input
  
anymatch [] input = Failure


sq :: [Parser] -> Parser

sq elems input =
  foldl
    (\res pars ->
      case res of
        Match rem -> pars rem
        Failure -> Failure)
    (Match input)
    elems


rep :: Parser -> Parser

rep subexp input =
  if null input
    then Match input
    else
      case subexp input of
        Match rem -> rep subexp rem
        Failure -> Failure -- Match input


allowChars :: (Char -> Bool) -> Parser

allowChars predicate input = case input of
  c : rem | predicate c -> Match rem
  _ -> Failure

char :: Char -> Parser

char c = allowChars (\z -> z == c)


-- Base elements

anyletter = allowChars isletter
  
isletter c = c >= 'a' && c <= 'z'


-- Complex elements

-- '[' charlist ']'
charclass = sq [char '[', rep anyletter, char ']']
