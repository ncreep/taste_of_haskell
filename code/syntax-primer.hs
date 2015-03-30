import Prelude hiding (map, ($), Maybe, Just, Nothing)
import Data.Char
import Control.Applicative

main :: IO ()
main = putStrLn "Hello World!"

{- Basic syntax -}

x :: Integer
x = 3

y :: Integer
y = 3 + x

name :: [Char]
name = "Keyser Soze"

addLists :: [a] -> [a] -> [a]
addLists ls1 ls2 = ls1 ++ ls2

longList :: [Integer]
longList = addLists [1, 2, 3] [4, 5] -- [1, 2, 3, 4, 5]

map :: (a -> b) -> [a] -> [b]
map f ls = case ls of
  []  -> []
  h:t -> f h : map f t
 
map' :: (a -> b) -> [a] -> [b] 
map' f []    = []
map' f (h:t) = f h : map' f t

mapPlusTwo :: [Integer] -> [Integer]
mapPlusTwo = map (\x -> x + 2)

mapPlusTwo' = map (+ 2)

plusTwoed :: [Integer]
plusTwoed = mapPlusTwo [1, 2, 3] -- [3, 4, 5]

validLogin :: [a] -> [b] -> Bool
validLogin name pass = 
  let validName = length name > 2
      validPass = length pass > 8
  in validName && validPass

validLogin' :: [a] -> [b] -> Bool  
validLogin' name pass = validName && validPass
  where validName = length name > 2
        validPass = length pass > 8

infixr 0 $
($) :: (a -> b) -> a -> b 
f $ x = f x

mapFiltered = map (+ 2) $ filter (3 <) [1, 2, 3, 4, 5] -- [6, 7]
mapFiltered' = map (+ 2) (filter (3 <) [1, 2, 3, 4, 5])

{- Type/data declarations -}

type User = String

data TwitterAPI = Timeline User Integer 
                | StatusUpdate User String 
                | Search String 
                deriving (Eq, Show)

timeline = Timeline "foo" 15
update = StatusUpdate "foo" "like, whatever"
search = Search "I dunno"

                                   
newtype TweetId = TweetId { id :: Integer }

data Maybe a = Just a | Nothing deriving (Eq, Show)

{- Typeclasses -}

{-
class Functor f where
  fmap :: (a -> b) -> f a -> f b
-}

instance Functor Maybe where
  fmap f (Just x) = Just $ f x
  fmap _ Nothing = Nothing
 
addWorld :: Functor f => f String -> f String 
addWorld fs = fmap (++ " World!") fs

addedWorld = addWorld $ Just "Hello" -- Just "Hello World!"

{-  
class Functor f => Applicative f where
    pure :: a -> f a

    (<*>) :: f (a -> b) -> f a -> f b
-}
  
instance Applicative Maybe where
  pure x = Just x
  
  Just f <*> Just x = Just $ f x
  _      <*> _      = Nothing

maybeSearch = pure Search <*> Just "I dunno" -- Just (Search "I dunno")
maybeTimeline = pure Timeline <*> Just "Keyser" <*> Just 5 -- Just (Timeline "Keyser" 5)
maybeTimeline' = pure Timeline <*> Nothing <*> Just 5 -- Nothing

maybeSearch' = Search <$> Just "I dunno"
maybeTimeline'' = Timeline <$> Just "Keyser" <*> Just 5

dropFirst = Just 3 *> Just 5
dropSecond = Just 3 <* Just 5

dropFirst' = Nothing *> Just 5
dropSecond' = Just 3 <* Nothing