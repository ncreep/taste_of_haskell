import Control.Applicative
import Data.List

data May a = J a | N
  deriving (Show, Eq)
  
instance Functor May where
  -- fmap f (J a) = J $ f a
  fmap _ N = N
  
instance Applicative May where
  pure x = J x
  J f <*> J x = J $ f x
  _   <*> _   = N
  
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

  Parser pa <|> Parser pb = Parser $ \s ->
    pa s ++ pb s
    -- case pa s of 
      -- []  -> pb s
      -- res -> res
  
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

(<>) :: Parser String -> Parser String -> Parser String
(<>) pa pb = (++) <$> pa <*> pb

string :: String -> Parser String
string "" = pure ""
-- string (c:cs) = char c <> string cs
string (c:cs) = ((\c' -> [c]) <$> char c) <> string cs

-- dot :: Parser String
-- dot = satisfy $ const True
dot = Parser $ \s ->
  case s of
    [] -> []
    (c:cs) -> [(c, cs)]

-- oneOf :: [Char] -> Parser String
oneOf cs = satisfy $ \c -> elem c cs

-- noneOf :: [Char] -> Parser String
noneOf cs = satisfy $ \c -> notElem c cs

opt :: Char -> Parser String
opt c = string [c] <|> pure ""

-- star :: Parser String -> Parser String
-- star p = concat <$> many p
star = many

-- plus :: Parser String -> Parser String
plus p =  concat <$> some p

has :: Parser a -> Parser a
-- has p = chars *> p <* chars
has p = star dot *> p <* star dot

chars :: Parser String
chars = Parser $ \s ->
  reverse (zip (Data.List.inits s) (Data.List.inits s)) 
  
eof :: Parser ()
eof = Parser $ \s ->
  case s of
    "" -> [((), "")]
    _ -> []
  
match p = parse $ p <* eof