import Control.Applicative

hello :: IO ()
hello = putStrLn "Hello"

world :: IO ()
world = putStrLn "World!"

helloWorld :: IO ()
helloWorld = hello *> world

getName :: IO String
getName = getLine

addGreeting :: IO String
addGreeting = fmap ("Hello " ++) getName

getNameAndGreet :: IO ()
getNameAndGreet = getName >>= \name -> 
  putStrLn $ "Hello " ++ name
  
getNameAndGreet' :: IO ()
getNameAndGreet' = do
  name <- getName
  putStrLn $ "Hello " ++ name
