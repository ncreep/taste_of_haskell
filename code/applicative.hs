import Control.Applicative

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

{-
lift :: (a -> b) -> ((a, String) -> (b, String))
lift f = \(a, s) -> (f a, s)
-}
 
instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ \s -> 
    [(f a, s') | (a, s') <- p s]
    
instance Applicative Parser where
  -- pure :: a -> Parser a
  pure x = Parser $ \s -> [(x, s)]

  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  Parser pf <*> Parser pa = Parser $ \s ->
    [(f a, s'') | (f, s') <- pf s, (a, s'') <- pa s']
      

instance Alternative Parser where
  empty = Parser $ \s -> []

  Parser pa <|> Parser pb = Parser $ \s ->
    case pa s of 
      []  -> pb s
      res -> res
      
satisfy :: (Char -> Bool) -> Parser String
satisfy p = Parser $ \s ->
  case s of
    [] -> []
    (c:cs) -> 
      if p c
      then [([c], cs)]
      else []
 
char :: Char -> Parser String
char c = satisfy (c ==)

(<>) :: Parser String -> Parser String -> Parser String
(<>) pa pb = (++) <$> pa <*> pb

string :: String -> Parser String
string "" = pure ""
string (c:cs) = char c <> string cs

dot :: Parser String
dot = satisfy $ const True

oneOf :: [Char] -> Parser String
oneOf cs = satisfy $ \c -> elem c cs

noneOf :: [Char] -> Parser String
noneOf cs = satisfy $ \c -> notElem c cs

opt :: Char -> Parser String
opt c = string [c] <|> pure ""

star :: Parser String -> Parser String
star p = concat <$> many p

plus :: Parser String -> Parser String
plus p =  concat <$> some p