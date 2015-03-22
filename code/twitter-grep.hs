{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (readFile)
import Data.ByteString.Lazy (readFile)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
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

{-
A server application that can do grep-like functionality to a Twitter timeline.
Upon start-up, the application assumes the presence of a 'config.json' file in the
same directory that contains the credentials for the Twitter API (see the Config record).

The application can be used as follows:
http://localhost:3000/grep-tweets/Hackage?limit=20&pattern=[Ss]ome|sim.le|pa[^abc]ttern
-}

-- The data that we'll be using for a single tweet
-- The names of the fields match the names of the field in the JSON result
data Tweet = Tweet { text :: String
                   , id_str :: String
                   } deriving (Show, Generic)
        
instance FromJSON Tweet
instance ToJSON Tweet

-- Configuration for the application: the credentials used to access the Twitter API
-- The fields should match the fields in a 'config.json' file to be provided to the application
data Config = Config { accessToken :: String
                     , accessTokenSecret :: String
                     , consumerKey :: String
                     , consumerSecret :: String
                     } deriving (Show, Generic)

instance FromJSON Config
instance ToJSON Config

-- Creating credentials from the configuration data
credential config = 
  newCredential (pack $ accessToken config) (pack $ accessTokenSecret config)

-- Creating an OAuth value from the configuration data
oauth config = newOAuth { oauthServerName     = "api.twitter.com"
                        , oauthConsumerKey    = pack $ consumerKey config
                        , oauthConsumerSecret = pack $ consumerSecret config
                        }
                        
{- Pure functions -}
                      
-- Does a grep on the text of a list of tweets, return a list with only the matched tweets
grepTweets :: String -> [Tweet] -> [Tweet]          
grepTweets pattern = filter $ (matches pattern) . text 

-- Generating the Twitter feed url for the given user and tweets limit
feedUrl user limit = 
     "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=" 
  ++ user
  ++ "&count="
  ++ (show limit)
  
-- An HTML template for a single tweet: embedding the tweet ID into a single 'div' tag
tweetDiv :: Tweet -> Html
tweetDiv tweet = H.div "" ! class_ "tweet" ! A.id (toValue $ id_str tweet)

-- An HTML template for a list of tweets, 'twitter.js' to render the tweets
htmlTemplate :: [Tweet] -> Html
htmlTemplate tweets = docTypeHtml $ do
     H.body $ do
         forM_ tweets tweetDiv
         script "" ! src "/twitter.js"
 
{- IO functions -}

-- Fetches the Twitter timeline for the given user, using the Twitter API
timeline :: Config -> String -> Int -> IO (Maybe [Tweet])
timeline config user limit = do
  req       <- parseUrl $ feedUrl user limit
  signedReq <- signOAuth (oauth config) (credential config) req
  resp      <- withManager $ httpLbs signedReq
  let body   = responseBody resp
  return $ decode body

-- Starting the server for the application.
startServer config = scotty 3000 $ do
  -- The main path for the application, applying grep to a user's timeline
  get "/grep-tweets/:user" $ do
    user <- S.param "user"
    limit <- S.param "limit"
    pattern <- S.param "pattern"
    maybeTweets <- liftIO $ timeline config user limit
    
    let response = case maybeTweets of
          Nothing     -> h1 "Failed to fetch tweets"
          Just tweets -> htmlTemplate $ grepTweets pattern tweets
          
    (S.html . renderHtml) response
    
  -- An auxiliary js file to render tweets
  get "/twitter.js" $ file "twitter.js"

-- Starting the application: first reading the configuration data from 'config.json',
-- then starting the server
main :: IO ()
main = do
  maybeConf <- decode <$> readFile "config.json"
  case maybeConf of 
    Nothing     -> putStrLn "Failed to parse config.json"
    Just config -> startServer config