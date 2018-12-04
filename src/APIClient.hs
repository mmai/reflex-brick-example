{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module APIClient ( APIClient(..)
               , apiSearchPackages
               , getClient
               ) where

import           Data.Text
import           Data.ByteString.Lazy
import           Data.Maybe
import           Control.Monad.State
import           Network.Wreq hiding (get)
import qualified Network.Wreq.Session as S
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types

import           Data 

data APIClient = APIClient 
  { server  :: String
  , session :: S.Session
  }

getClient :: IO APIClient
getClient = do
  sess   <- S.newAPISession
  return $ APIClient 
        { server    = "http://hackage.haskell.org/"
        , session   = sess
        }

callApiGet :: String -> StateT APIClient IO (Response ByteString)
callApiGet request = do
  APIClient {..} <- get
  let opts = defaults & header "Accept" .~ ["application/json"]
  liftIO $ S.getWith opts session (server <> request) 

getPackages :: String -> StateT APIClient IO [Package]
getPackages search = do
  resp <- callApiGet $ "packages/search?terms=" <> search 
  return . fromMaybe [] $ decode $ resp ^. responseBody 

apiSearchPackages :: String -> APIClient -> IO [Package]
apiSearchPackages search = evalStateT (getPackages search)

-- apiSearchPackages' :: APIClient -> IO (String -> [Package])
-- apiSearchPackages = evalStateT $ do
--   APIClient {..} <- get
--   let opts = defaults & header "Accept" .~ ["application/json"]
--   resp <- liftIO $ S.getWith opts session (server <> "packages/search?terms=" <> search) 
--   return . fromMaybe [] $ decode $ resp ^. responseBody 
