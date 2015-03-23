module Grep where

import Control.Applicative

import Parser

{-
Using the combinators from `Parser` to construct a small grep-like language.
-}

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

-- Using `And`s to join a list of `Grep`s into a single `Grep`
andGreps :: [Grep] -> Grep
andGreps []       = Str ""
andGreps [g]      = g
andGreps (g : gs) = And g $ andGreps gs

-- Parsers for the various cases of `Grep`
grepChar   = Str <$> alphaNum
grepString = Str <$> plus alphaNum
grepDot    = Dot <$ char '.'
grepOneOf  = OneOf <$> (char '[' *> plus alphaNum <* char ']') -- syntax: [abc]
grepNoneOf = NoneOf <$> (string "[^" *> plus alphaNum <* char ']') -- syntax: [^abc]
grepStar   = Star <$> (grepDot <|> grepChar) <* char '*' -- syntax: a*
grepPlus   = Plus <$> (grepDot <|> grepChar) <* char '+' -- syntax: a+
grepOr     = Or <$> notGrepOr <* char '|' <*> grepParser -- syntax: abc*|[abc]a|.*
-- since `Or` should have low precedence, first trying to match as many non-`Or` values as possible

-- A single parser for everything that is not an `Or` parser, parsing as many occurrences as possible 
notGrepOr = andGreps <$> some (grepString <|> grepDot <|> grepOneOf <|> grepNoneOf <|> grepStar <|> grepPlus)

-- The full parser for `Grep` expressions
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