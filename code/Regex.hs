module Regex where

import Control.Applicative
import Data.Maybe

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
evalRegex :: Regex -> Parser String
evalRegex regex = case regex of
  Str s     -> string s
  Dot       -> dot
  OneOf cs  -> oneOf cs
  NoneOf cs -> noneOf cs
  Star r    -> star $ evalRegex r
  Plus r    -> plus $ evalRegex r
  Or r1 r2  -> evalRegex r1 <|> evalRegex r2
  And r1 r2 -> evalRegex r1 & evalRegex r2

-- Parsers for the various cases of `Regex`
regexChar   = Str <$> alphaNum
regexString = Str <$> plus alphaNum
regexDot    = Dot <$ char '.'
regexOneOf  = OneOf <$> (char '[' *> plus alphaNum <* char ']') -- syntax: [abc]
regexNoneOf = NoneOf <$> (string "[^" *> plus alphaNum <* char ']') -- syntax: [^abc]
regexStar   = Star <$> singleCharRegex <* char '*' -- syntax: a*
regexPlus   = Plus <$> singleCharRegex <* char '+' -- syntax: a+
regexAnd    = And <$> simpleRegex <*> (simpleRegex <|> regexAnd) -- syntax: [a]b
regexOr     = Or <$> (simpleRegex <|> regexAnd) <* char '|' <*> regexParser -- syntax: a|b
-- since `Or` should have low precedence, first trying to match as many non-`Or` values as possible
-- since `And` should have high precedence, we are not letting it capture `Or` values

-- Parser that match single characters
singleCharRegex = regexChar <|> regexDot <|> regexOneOf <|> regexNoneOf
-- A parser for all the simple regex parsers, i.e. excluding `And` and `Or`
simpleRegex = regexString <|> regexDot <|> regexOneOf <|> regexNoneOf <|> regexStar <|> regexPlus

-- The full parser for `Regex` expressions
regexParser = simpleRegex <|> regexAnd <|> regexOr

safeHead = listToMaybe

-- Converts a string into a `Regex` datatype
strToRegex :: String -> Maybe Regex
strToRegex str = safeHead $ match regexParser str -- taking the first result

-- Takes a regex-like string and a target string and return `True` if there is a match anywhere inside the string
grep :: String -> String -> Bool
grep regexStr target = not $ null matches
  where matches = match (has regex) target
        maybeRegex = fmap evalRegex $ strToRegex regexStr
        regex = fromMaybe empty maybeRegex