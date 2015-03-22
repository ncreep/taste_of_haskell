{-# LANGUAGE GADTs #-}
module Parser where

import Control.Applicative
import Data.Char
import Data.List
 
newtype Parser a = Parser { parse :: String -> [(a, String)] }

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


-- satisfy :: (Char -> Bool) -> Parser String
satisfy predicate = Parser $ \s ->
  case s of
    [] -> []
    (c:cs) -> 
      if predicate c
      then [(c, cs)]
      else []
 
-- char :: Char -> Parser String
char c = satisfy (c ==)

infixr 0 &
(&) :: Parser String -> Parser String -> Parser String
(&) pa pb = (++) <$> pa <*> pb

asStr c = [c]

string :: String -> Parser String
string "" = pure ""
string (c:cs) = asStr <$> char c & string cs

-- dot :: Parser String
dot = satisfy $ const True

-- oneOf :: [Char] -> Parser String
oneOf cs = satisfy $ \c -> elem c cs

-- noneOf :: [Char] -> Parser String
noneOf cs = satisfy $ \c -> notElem c cs

opt :: Char -> Parser String
opt c = string [c] <|> pure ""

-- star :: Parser String -> Parser String
star = many
-- star p = concat <$> many p

-- plus :: Parser String -> Parser String
plus =  some
-- plus p =  concat <$> some p

-- has :: Parser a -> Parser a
has p = star dot *> p <* star dot

eof :: Parser ()
eof = Parser $ \s ->
  case s of
    "" -> [((), "")]
    _ -> []
  
-- match :: Parser String -> String -> [(String, String)]
match p = (map fst) . nub . (parse $ p <* eof)

runParser (Parser p) s = 
  case map fst $ filter hasResult results of
    [res] -> res
    [] -> error "No results"
    _ -> error "More than one parse result"
    where results = p s
          hasResult (a, []) = True
          hasResult (a, _) = False
          
runParser' (Parser p) s = map fst $ filter hasResult results
    where results = p s
          hasResult (a, []) = True
          hasResult (a, _) = False

alphaNum = satisfy isAlphaNum

data Grep a where
  Chr :: Char -> Grep Char
  Str :: String  -> Grep String
  Dot :: Grep Char
  OneOf :: [Char] -> Grep Char
  NoneOf :: [Char] -> Grep Char
  Star :: Grep Char -> Grep String
  Plus :: Grep Char -> Grep String
  Or :: Grep String -> Grep String -> Grep String
  And :: Grep String -> Grep String -> Grep String
  deriving (Eq, Show)
  
joinGreps [] = Str ""
joinGreps [g] = g
joinGreps (g : gs) = And g $ joinGreps gs
-- joinGreps = foldl (And) (Str "")
  
grepChar = Chr <$> alphaNum
grepString = Str <$> plus alphaNum
grepDot = Dot <$ char '.'
grepOneOf = OneOf <$> (char '[' *> plus alphaNum <* char ']')
grepNoneOf = NoneOf <$> (string "[^" *> plus alphaNum <* char ']')
grepStar = Star <$> (grepDot <|> grepChar)  <* char '*'
grepPlus = Plus <$> (grepDot <|> grepChar) <* char '+'
grepOr = Or <$> notGrepOr <* char '|' <*> grepParser

notGrepOr = joinGreps <$> some (grepString <|> grepDot <|> grepOneOf <|> grepNoneOf <|> grepStar <|> grepPlus)
grepParser = grepOr <|> notGrepOr

grepToParser :: Grep a -> Parser a
grepToParser grep = case grep of
  Chr c -> asStr <$> char c
  Str s -> string s
  Dot -> asStr <$> dot
  OneOf cs -> asStr <$> oneOf cs
  NoneOf cs -> asStr <$> noneOf cs
  Star g -> star $ grepToParser g
  Plus g -> plus $ grepToParser g
  Or g1 g2 -> grepToParser g1 <|> grepToParser g2
  And g1 g2 -> grepToParser g1 & grepToParser g2

toGrep s = case match grepParser s of
  [] -> empty
  p : _ -> grepToParser p

matchGrep = match . has . toGrep
matches pattern = not . null . (matchGrep pattern)
{-
grepString = string <$> plus alphaNum
grepDot = dot <$ char '.'
grepOneOf = oneOf <$> (char '[' *> plus alphaNum <* char ']')
grepNoneOf = noneOf <$> (string "[^" *> plus alphaNum <* char ']')
grepStar = star <$> (grepDot <|> grepChar)  <* char '*'
grepPlus = plus <$> (grepDot <|> grepChar) <* char '+'
grepOr = (<|>) <$> notGrepOr <* char '|' <*> grep
notGrepOr = grepString <|> grepDot <|> grepOneOf <|> grepNoneOf <|> grepStar <|> grepPlus

-- grep :: Parser (Parser String)
-- grep = star $ grepString <|> grepDot <|> grepNoneOf <|> grepStar <|> grepPlus <|> grepOneOf <|> grepOr
-- grep = joinParsers <$> (some $ grepString <|> grepDot <|> grepOneOf <|> grepNoneOf<|> grepStar <|> grepPlus )-- <|> grepOr)
grep =  joinParsers <$> (many $ notGrepOr <|> grepOr) -- )

joinParsers :: [Parser String] -> Parser String
joinParsers = foldl (&) (pure "")
-}

-- toGrep :: String -> Parser String
-- toGrep s = 
