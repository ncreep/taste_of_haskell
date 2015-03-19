{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (readFile)
import Data.ByteString.Lazy (readFile)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text.Lazy as T
import Network.HTTP.Conduit
import Web.Authenticate.OAuth
import Data.Aeson
import GHC.Generics
import Control.Applicative
import Control.Monad.Trans (liftIO)
import Control.Monad (forM_)

import Web.Scotty hiding (text)
import Text.Blaze.Html5 hiding (text, map)
import Text.Blaze.Html5.Attributes
import qualified Web.Scotty as S
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text

import Parser

-- Insert here your own credentials

data Tweet = Tweet { text :: String
                   , id_str :: String
                   } deriving (Show, Generic)
        
instance FromJSON Tweet
instance ToJSON Tweet

        
data Config = Config { accessToken :: String
                     , accessTokenSecret :: String
                     , consumerKey :: String
                     , consumerSecret :: String
                     } deriving (Show, Generic)

instance FromJSON Config
instance ToJSON Config

credential config = 
  newCredential (C8.pack $ accessToken config) (C8.pack $ accessTokenSecret config)

oauth config = newOAuth { oauthServerName     = "api.twitter.com"
                        , oauthConsumerKey    = C8.pack $ consumerKey config
                        , oauthConsumerSecret = C8.pack $ consumerSecret config
                        }
          
grepTweets :: String -> [Tweet] -> [Tweet]          
grepTweets pattern = filter $ (matches pattern) . text 

feedUrl user limit = 
     "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=" 
  ++ user
  ++ "&count="
  ++ (show limit)

timeline :: Config -> String -> Int -> IO (Maybe [Tweet])
timeline config user limit = do
  req       <- parseUrl $ feedUrl user limit
  signedReq <- signOAuth (oauth config) (credential config) req;
  resp      <- withManager $ httpLbs signedReq
  let body   = responseBody resp
  return $ decode body

-- Read configuration, then display tweets of user
main :: IO ()
main = do
  maybeConf <- decode <$> readFile "config.json"
  case maybeConf of 
    Nothing     -> putStrLn "Failed to parse config.json"
    -- Just config -> displayWith config "Hackage"
    Just config -> startServer config

startServer config = scotty 3000 $ do
  get "/grep-tweets/:user" $ do
    user <- S.param "user"
    limit <- S.param "limit"
    pattern <- S.param "pattern"
    maybeTweets <- liftIO $ timeline config user limit
    
    let response = case maybeTweets of
          Nothing     -> h1 "Failed to fetch tweets"
          Just tweets -> htmlTemplate $ grepTweets pattern tweets
          
    (S.html . renderHtml) response
    
  get "/twitter.js" $ file "twitter.js"

htmlTemplate tweets = docTypeHtml $ do
     H.body $ do
         forM_ tweets tweetDiv
         script "" ! src "/twitter.js"

tweetDiv tweet = H.div "" ! class_ "tweet" ! A.id (toValue $ id_str tweet)
    
-- using configuration, display tweets for user to stdout
displayWith :: Config -> String -> IO ()
displayWith config user = do
  maybeTweets <- timeline config user 5
  case maybeTweets of
    Nothing     -> putStrLn "Failed to fetch tweets"
    Just tweets -> mapM_ print $ map text $ grepTweets ".*" tweets