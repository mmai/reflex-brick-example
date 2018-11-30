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
    let eQuit   = select es $ RBKey (V.KChar 'q')
        eSearch = select es $ RBKey (V.KChar 's') 

    eSearchPackages <- mkSearchPackages apiclient eSearch
    dState <- foldDyn ($) initial . mergeWith (.) $ 
                [ eSearchPackages
                ]

    let eNotQuit = difference (updated dState) eQuit
        eOut     = appStateToBrickAppState <$> eNotQuit

    pure $ ReflexBrickApp eOut never (void eQuit)

mkSearchPackages :: (Reflex t, MonadIO m) => APIClient -> Event t a -> m (Event t (AppState -> AppState))
mkSearchPackages apiclient e = do 
  searchPackages <- liftIO $ searchPackagesIO apiclient
  pure $ searchPackages <$ e

searchPackagesIO :: APIClient -> IO (AppState -> AppState)
searchPackagesIO api = (packages .~) <$> apiSearchPackages "reflex" api 
