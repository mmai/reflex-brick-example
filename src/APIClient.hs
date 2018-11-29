{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module APIClient ( APIClient(..)
               , apiPackages
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
import           Data.Aeson.Lens (_String, key)
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

apiPackages :: String -> APIClient -> IO [Package]
apiPackages search = evalStateT (getPackages search)
