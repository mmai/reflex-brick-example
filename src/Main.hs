{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Control.Monad (void)
import           Control.Monad.Trans (liftIO)
import           Control.Monad.IO.Class
import           Control.Concurrent (forkIO)
import           Control.Lens
import           Data.Text

import           Reflex
import           Reflex.Brick
import qualified Graphics.Vty as V

import           Data
import           APIClient
import           Template ( appStateToBrickAppState, Name(..), initialSearchEditor, getSearchContents)

main :: IO ()
main = do
  let initial = AppState 
        { _search  = "reflex" 
        , _searchE = initialSearchEditor
        , _packages = []
        }
  apiclient <- getClient
  runReflexBrickApp (pure ()) Nothing (appStateToBrickAppState initial :: ReflexBrickAppState Name) $ \es -> do
    let eQuit   = select es $ RBKey V.KEsc
        eSearch = const "brick" <$> select es ( RBKey V.KEnter ) -- XXX use behavior to get search value ?
        eWrite =  select es ( RBKey (V.KChar 'f') ) -- select all other keypresses ?


-- myEventHandler :: MyState -> BrickEvent n e -> EventM Name (Next MyState)
-- myEventHandler s (VtyEvent e) = continue =<< handleEventLensed s theEdit handleEditorEvent e

                -- VtyEvent (V.EvKey V.KEnter [])
                --     | focusGetCurrent (formFocus s) /= Just AddressField -> halt s
                -- _ -> do
                --     s' <- handleFormEvent ev s

-- TODO : if focus on search, handle keypress events and update search field

    -------------- trying to construct searchPackages Event

    -- -- simple : NOK, doesn't compile
    -- let eSearchPackages = liftIO . searchPackagesIO apiclient <$> eSearch

    -- -- performEvent : NOK events are triggered at the next "real" event 
    -- eSearchPackages <- performEvent $ liftIO . searchPackagesIO apiclient <$> eSearch

    -- -- performAsync : NOK  events are triggered at the next "real" event 
    -- eSearchPackages <- performAsync eSearch (searchPackagesIO apiclient)

    -- search string parameter fixed : OK, but we need a real variable parameter
    searchPackages <- liftIO $ searchPackagesIO apiclient "wreq" 
    let eSearchPackages = searchPackages  <$ eSearch
    -- let eSearchPackages = searchPackages . getSearchContents <$ eSearch

    -------------- end trying to construct searchPackages Event

    dState <- foldDyn ($) initial eSearchPackages

    let eNotQuit = difference (updated dState) eQuit
        eOut     = appStateToBrickAppState <$> eNotQuit

    pure $ ReflexBrickApp eOut never (void eQuit)

-- handleForm :: Event t a -> AppState -> AppState
-- handleForm = handleBrickEvents

searchPackagesIO :: APIClient -> String -> IO (AppState -> AppState)
searchPackagesIO api search = (packages .~) <$> apiSearchPackages search api 

-- Wrapper for performEventAsync where the function is called in another thread
forkEventAsync :: (MonadIO (Performable m), TriggerEvent t m, PerformEvent t m) => Event t a -> (a -> (b -> IO ()) -> IO ()) -> m (Event t b)
forkEventAsync e f = performEventAsync $ ffor e $ \a cb -> liftIO ( void $ forkIO $ f a cb )

performAsync ::  (MonadIO (Performable m), TriggerEvent t m, PerformEvent t m) => Event t a -> (a -> IO b) -> m (Event t b)
performAsync e f = forkEventAsync e (\a cb -> f a >>= cb) 
