{-# LANGUAGE OverloadedStrings #-}

module Template (
    appStateToBrickAppState
  ) where

import           Control.Lens

import           Brick
import           Reflex.Brick
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Center       as C
import qualified Graphics.Vty               as V

import Data 

appStateToBrickAppState :: AppState -> ReflexBrickAppState n
appStateToBrickAppState s = ReflexBrickAppState [window] (const Nothing) (attrMap V.defAttr []) 
  where window  =  B.borderWithLabel (str "Search on Hackage (press 'q' to quit)") $ content
        content = vBox [ searchWidget
                       , C.hCenter $ hBox [ packagesListWidget
                                          ]
                       ]
        searchWidget = C.hCenter . padAll 1 $ txt "Search"
        packagesListWidget = padAll 1 $ vBox [ C.hCenter $ vBox (fmap packageWidget (s ^. packages))]
        packageWidget p = hBox [ txt $ p ^. name ]
