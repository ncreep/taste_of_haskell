module Parser where

import Control.Applicative
import Data.Char
import Data.List

{-
A simple applicative parser implementation. Using the combinators to construct a small grep-like language.

Inspired by:
http://dev.stephendiehl.com/fun/002_parsers.html
https://github.com/Gabriel439/Haskell-Turtle-Library/blob/45219bc99141fe956bc00f73f151460449f89376/src/Turtle/Pattern.hs
https://gist.github.com/willtim/660454
-}
 
-- A parser for a string of characters
newtype Parser a = Parser { parse :: String -> [(a, String)] }

-- Note: cannot put type signatures in typeclass instances since this creates a duplicate signature

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ \s -> 
    [(f a, s') | (a, s') <- p s]
    
instance Applicative Parser where
  -- pure :: a -> Parser a
  pure a = Parser $ \s -> [(a, s)]

  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  Parser pf <*> Parser pa = Parser $ \s ->
    [(f a, s'') | (f, s') <- pf s, (a, s'') <- pa s']
      

instance Alternative Parser where
  empty = Parser $ \s -> []

  Parser pa <|> Parser pb = Parser $ \s -> pa s ++ pb s

-- Checks whether the current character in the stream matches the given predicate
satisfy :: (Char -> Bool) -> Parser String
satisfy predicate = Parser $ \s ->
  case s of
    []     -> []
    (c:cs) -> 
      if predicate c
      then [([c], cs)]
      else []

-- Matches any character
dot :: Parser String
dot = satisfy $ const True
 
-- Matches the given character
char :: Char -> Parser String
char c = satisfy (c ==)

-- Matches any alphanumeric character
alphaNum :: Parser String
alphaNum = satisfy isAlphaNum

-- oneOf - matches any of the given character, noneOf - matches non of the given characters
oneOf, noneOf :: [Char] -> Parser String
oneOf cs = satisfy $ \c -> elem c cs
noneOf cs = satisfy $ \c -> notElem c cs

infixr 0 &
-- Runs both parsers and concatenates their results
(&) :: Parser String -> Parser String -> Parser String
(&) pa pb = (++) <$> pa <*> pb

-- Matches the given string
string :: String -> Parser String
string ""     = pure ""
string (c:cs) = char c & string cs

-- star - matches the given parser 0 or more times, plus - matches 1 or more times
star, plus :: Parser String -> Parser String
star p = concat <$> many p
plus p = concat <$> some p

-- Checks whether the given parser matches anywhere inside a string
-- The signature can be generalized to: has :: Parser a -> Parser a
has :: Parser String -> Parser String
has p = star dot *> p <* star dot

-- Tests whether the parser reached the end of the stream
eof :: Parser ()
eof = Parser $ \s ->
  case s of
    "" -> [((), "")]
    _  -> []
  
-- Tries to match the given parser till then end of the string, if successful, returning a list of results
match :: Eq a => Parser a -> String -> [a]
match p str = map fst noDupsRes
  where noDupsRes = nub parseRes
        parseRes = parse (p <* eof) str


-- A datatype describing a simplified version of grep
-- Not completely accurate (e.g. `Star` should take only single characters or `Dot`s), this can be improved using GADTs
data Grep = 
    Str String 
  | Dot
  | OneOf [Char] 
  | NoneOf [Char] 
  | Star Grep 
  | Plus Grep 
  | Or Grep Grep
  | And Grep Grep
  deriving (Eq, Show)
 
-- Converts a `Grep` value into a corresponding `Parser`
grepToParser grep = case grep of
  Str s     -> string s
  Dot       -> dot
  OneOf cs  -> oneOf cs
  NoneOf cs -> noneOf cs
  Star g    -> star $ grepToParser g
  Plus g    -> plus $ grepToParser g
  Or g1 g2  -> grepToParser g1 <|> grepToParser g2
  And g1 g2 -> grepToParser g1 & grepToParser g2

-- Using `And`s to join a list of Greps into a single Grep
andGreps :: [Grep] -> Grep
andGreps []       = Str ""
andGreps [g]      = g
andGreps (g : gs) = And g $ andGreps gs

-- Parsers for the various cases of Grep
grepChar   = Str <$> alphaNum
grepString = Str <$> plus alphaNum
grepDot    = Dot <$ char '.'
grepOneOf  = OneOf <$> (char '[' *> plus alphaNum <* char ']') -- syntax: [abc]
grepNoneOf = NoneOf <$> (string "[^" *> plus alphaNum <* char ']') -- syntax: [^abc]
grepStar   = Star <$> (grepDot <|> grepChar)  <* char '*' -- syntax: a*
grepPlus   = Plus <$> (grepDot <|> grepChar) <* char '+' -- syntax: a+
grepOr     = Or <$> notGrepOr <* char '|' <*> grepParser -- syntax: abc*|[abc]a|.*
-- since `Or` should have low precedence, first trying to match as many non-`Or` values as possible

-- A single parser for everything that is not an `Or` parser
notGrepOr = andGreps <$> some (grepString <|> grepDot <|> grepOneOf <|> grepNoneOf <|> grepStar <|> grepPlus)
-- The full parser for Grep expressions
grepParser = grepOr <|> notGrepOr

-- Converts a grep-like string into a `Parser`
toGrep :: String -> Parser String
toGrep grepStr = case match grepParser grepStr of
  []       -> empty
  grep : _ -> grepToParser grep -- taking the first result

-- Takes a grep-like string and a target string and return `True` if there is a match
matches :: String -> String -> Bool
matches grepStr target = not $ null matches
  where matches = match (has grep) target
        grep = toGrep grepStr