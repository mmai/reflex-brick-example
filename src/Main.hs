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
  let initial = AppState "reflex" []
  apiclient <- getClient
  runReflexBrickApp (pure ()) Nothing (appStateToBrickAppState initial :: ReflexBrickAppState ()) $ \es -> do
    let eQuit   = select es $ RBKey (V.KChar 'q')
        eSearch = const "reflex" <$> (select es $ RBKey (V.KChar 's') )

    eSearchPackages <- mkSearchPackages apiclient eSearch
    dState <- foldDyn ($) initial . mergeWith (.) $ 
                [ eSearchPackages
                ]

    let eNotQuit = difference (updated dState) eQuit
        eOut     = appStateToBrickAppState <$> eNotQuit

    pure $ ReflexBrickApp eOut never (void eQuit)

mkSearchPackages :: (Reflex t, MonadHold t m, MonadIO m) => APIClient -> Event t String -> m (Event t (AppState -> AppState))
mkSearchPackages apiclient eSearch = do 
  -- dSearch <- holdDyn "reflex" eSearch
  -- searchPackagesIni <- liftIO $ searchPackagesIO apiclient "reflex" 
  -- dSearch <- foldDynM (mkSearchPackages' apiclient) searchPackagesIni eSearch
  -- return $ updated dSearch
  -- XXX  with performEvent ?

  searchPackages <- liftIO $ searchPackagesIO apiclient "reflex" 
  pure $ searchPackages <$ eSearch

mkSearchPackages' :: (MonadIO m) => APIClient -> String -> (AppState -> AppState) -> m (AppState -> AppState)
mkSearchPackages' api search _ = liftIO $ searchPackagesIO api search

searchPackagesIO :: APIClient -> String -> IO (AppState -> AppState)
searchPackagesIO api search = (packages .~) <$> apiSearchPackages search api 

