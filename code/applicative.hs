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
  
newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

{-
lift :: (a -> b) -> ((a, String) -> (b, String))
lift f = \(a, s) -> (f a, s)
-}
 
instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ \s -> 
    case p s of
      Just (a, s') -> Just (f a, s')
      Nothing -> Nothing
    {-
      fmap f' $ p s
    where f' = lift f
    -} 
    
instance Applicative Parser where
  -- pure :: a -> Parser a
  pure x = Parser $ \s -> Just (x, s)
  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  Parser pf <*> Parser pa = Parser $ \s ->
      case pf s of
        Just (f, s') -> case pa s' of
          Just (a, s'') -> Just (f a, s'')
          Nothing -> Nothing
        Nothing -> Nothing
{-      
     let (s', mf)  = 
         (s'', ma) = parse pa s'
     in (s'', mf <*> ma)
-}

instance Alternative Parser where
  empty = Parser $ \s -> Nothing
  Parser pa <|> Parser pb = Parser $ \s ->
    case pa s of 
      Just (a, s') -> Just (a, s')
      Nothing -> pb s
 
 
item :: Parser Char
item = Parser $ \s ->
  case s of
    [] -> Nothing
    (c:cs) -> Just (c, cs)
  {-  
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> 
  case parse item s of
    Just (c, s') -> if p c then Just (c, s') else Nothing
    Nothing -> Nothing
  -}
satisfy :: (Char -> Bool) -> Parser String
satisfy p = Parser $ \s ->
  case s of
    [] -> Nothing
    (c:cs) -> 
      if p c
      then Just ([c], cs)
      else Nothing

 
char :: Char -> Parser String
char c = satisfy (c ==)

string :: String -> Parser String
string "" = pure ""
string s@(c:cs) = char c <> string cs

dot :: Parser String
dot = satisfy $ const True

(<>) :: Parser String -> Parser String -> Parser String
(<>) pa pb = (++) <$> pa <*> pb

oneOf :: [Char] -> Parser String
oneOf cs = satisfy $ \c -> elem c cs

noneOf :: [Char] -> Parser String
noneOf cs = satisfy $ \c -> notElem c cs

opt :: Char -> Parser String
opt c = string [c] <|> pure ""

star :: Parser String -> Parser String
star p =  concat <$> many p

plus :: Parser String -> Parser String
plus p =  concat <$> some p