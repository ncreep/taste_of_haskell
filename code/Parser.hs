module Parser where

import Control.Applicative
import Data.Char
import Data.List

{-
A simple applicative parser implementation.

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

  -- Creates a new parser that runs both parsers in parallel and concatenates the
  -- alternative parsings, this provides backtracking when there are multiple alternatives
  Parser pa <|> Parser pb = Parser $ \s -> pa s ++ pb s

-- Checks whether the current character in the stream matches the given predicate
satisfy :: (Char -> Bool) -> Parser String
satisfy predicate = Parser $ \s ->
  case s of
    []   -> []
    c:cs -> 
      if predicate c
      then [([c], cs)]
      else []
      
-- Matches the given character
char :: Char -> Parser String
char c = satisfy (c ==)

-- Matches any character
dot :: Parser String
dot = satisfy $ const True
 
-- Matches any alphanumeric character
alphaNum :: Parser String
alphaNum = satisfy isAlphaNum

-- oneOf - matches any of the given character, noneOf - matches non of the given characters
oneOf, noneOf :: [Char] -> Parser String
oneOf cs = satisfy $ \c -> elem c cs
noneOf cs = satisfy $ \c -> notElem c cs

infixr 6 &
-- Runs both parsers sequentially and concatenates their outputs
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
match :: Parser a -> String -> [a]
match p str = map fst $ parse (p <* eof) str
