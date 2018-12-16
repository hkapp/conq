module Parser where

-- Types

type Parser t = TreeBuilder t -> String -> ParseResult t

data ParseResult t = Success t String | Failure

data TreeBuilder t = TreeBuilder {
  buildChar      :: Char -> t,
  buildCharClass :: [Char] -> t
}

noTree = TreeBuilder (\c -> ()) (\cs -> ())

-- Exported functions

parse :: Parser t
parse = anymatch [rep anyletter, charclass]

matches :: String -> Bool
matches input = case parse noTree input of
  Success () [] -> True
  _ -> False


-- Parser combinators

anymatch :: [Parser t] -> Parser t

anymatch (p : ps) input = case p input of
  Success t s -> Success t s
  Failure -> anymatch ps input
  
anymatch [] input = Failure


sq :: [Parser t] -> Parser t

sq elems input =
  foldl
    (\res pars ->
      case res of
        Success rem -> pars rem
        Failure -> Failure)
    (Success input)
    elems


rep :: Parser t -> ([t] -> t) -> Parser t

rep parseRepExp combineRes tb input =
  -- let subrep subin =
    -- if null subin
      -- then ([], subin)
      -- else
        -- case subexp subin of
          -- Success subtree rem ->
            -- case subrep rem of (ts, subrem) -> (subtree : ts, subrem)
          -- Failure -> ([], subin)
  -- in case subrep input of
    -- ([], rem) -> Failure
    -- (ts, rem) -> Success (combineRes ts) rem
  case recparse input of
    ([], rem) -> Failure
    (ts, rem) -> Success (combineRes ts) rem
    where --recparse :: String -> ([t], String)
          recparse []    = ([], [])
          recparse remin = checkExpParse (parseRepExp tb remin)
          --checkExpParse :: ParseResult t -> ([t], String)
          where checkExpParse Failure             = ([], remin)
                checkExpParse (Success subtree rem) = buildRes (recparse rem)
          --buildRes :: ([t], String) -> ([t], String)
                buildRes (ts, subrem) = (subtree : ts, subrem)
        

allowChars :: (Char -> Bool) -> Parser t

allowChars predicate t input = case input of
  c : rem | predicate c -> Success (buildChar t c) rem
  _ -> Failure

char :: Char -> Parser t

char c = allowChars (\z -> z == c)


-- Base elements

anyletter = allowChars isletter
  
isletter c = c >= 'a' && c <= 'z'


-- Complex elements

-- '[' charlist ']'
charclass = sq [char '[', rep anyletter, char ']']
