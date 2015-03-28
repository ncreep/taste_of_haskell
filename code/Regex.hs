module Regex where

import Control.Applicative

import Parser

{-
Using the combinators from `Parser` to construct a small regex-like language.
-}

-- A datatype describing a simplified version of regular expressions
-- Not completely accurate (e.g. `Star` should take only single character patterns), this can be improved using GADTs
data Regex = 
    Str String 
  | Dot
  | OneOf [Char] 
  | NoneOf [Char] 
  | Star Regex 
  | Plus Regex 
  | Or Regex Regex
  | And Regex Regex
  deriving (Eq, Show)
 
-- Converts a `Regex` value into a corresponding `Parser`
regexToParser regex = case regex of
  Str s     -> string s
  Dot       -> dot
  OneOf cs  -> oneOf cs
  NoneOf cs -> noneOf cs
  Star g    -> star $ regexToParser g
  Plus g    -> plus $ regexToParser g
  Or g1 g2  -> regexToParser g1 <|> regexToParser g2
  And g1 g2 -> regexToParser g1 & regexToParser g2

-- Using `And`s to join a list of `Regex`s into a single `Regex`
andRegexes :: [Regex] -> Regex
andRegexes []       = Str ""
andRegexes [g]      = g
andRegexes (g : gs) = And g $ andRegexes gs

-- Parsers for the various cases of `Regex`
regexChar   = Str <$> alphaNum
regexString = Str <$> plus alphaNum
regexDot    = Dot <$ char '.'
regexOneOf  = OneOf <$> (char '[' *> plus alphaNum <* char ']') -- syntax: [abc]
regexNoneOf = NoneOf <$> (string "[^" *> plus alphaNum <* char ']') -- syntax: [^abc]
regexStar   = Star <$> (regexDot <|> regexChar <|> regexOneOf <|> regexNoneOf) <* char '*' -- syntax: a*
regexPlus   = Plus <$> (regexDot <|> regexChar <|> regexOneOf <|> regexNoneOf) <* char '+' -- syntax: a+
regexOr     = Or <$> notRegexOr <* char '|' <*> regexParser -- syntax: abc*|[abc]a|.*
-- since `Or` should have low precedence, first trying to match as many non-`Or` values as possible

-- A single parser for everything that is not an `Or` parser, parsing as many occurrences as possible 
notRegexOr = andRegexes <$> some (regexString <|> regexDot <|> regexOneOf <|> regexNoneOf <|> regexStar <|> regexPlus)

-- The full parser for `Regex` expressions
regexParser = regexOr <|> notRegexOr

-- Converts a regex-like string into a `Parser`
toRegex :: String -> Parser String
toRegex regexStr = case match regexParser regexStr of
  []        -> empty
  regex : _ -> regexToParser regex -- taking the first result

-- Takes a regex-like string and a target string and return `True` if there is a match
matches :: String -> String -> Bool
matches regexStr target = not $ null matches
  where matches = match (has regex) target
        regex = toRegex regexStr