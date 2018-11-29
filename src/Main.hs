{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Control.Monad (void)
import           Control.Monad.Trans (liftIO)
import           Control.Monad.IO.Class
import           Control.Lens
import           Data.Text

import           Reflex
import           Reflex.Brick
import qualified Graphics.Vty as V

import           Data
import           APIClient
import           Template (appStateToBrickAppState)

main :: IO ()
main = do
  let initial = AppState []
  apiclient <- getClient
  runReflexBrickApp (pure ()) Nothing (appStateToBrickAppState initial :: ReflexBrickAppState ()) $ \es -> do
    let 
        eQuit = select es $ RBKey (V.KChar 'q')
        ePackages = select es $ RBKey (V.KChar 's') 

    eFetchedPackages <- mkFetchPackages apiclient ePackages
    dState <- foldDyn ($) initial .
              mergeWith (.) $ 
                [ eFetchedPackages
                ]

    let eNotQuit = difference (updated dState) eQuit
        eOut     = appStateToBrickAppState <$> eNotQuit

    pure $ ReflexBrickApp eOut never (void eQuit)

mkFetchPackages :: (Reflex t, MonadIO m) => APIClient -> Event t a -> m (Event t (AppState -> AppState))
mkFetchPackages apiclient e = do 
  fetchPackages <- liftIO $ updatePackagesIO apiclient
  pure $ fetchPackages <$ e

updatePackagesIO :: APIClient -> IO (AppState -> AppState)
updatePackagesIO api = (packages .~) <$> apiPackages "reflex" api 
