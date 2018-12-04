{-# LANGUAGE OverloadedStrings #-}

module Template (
    appStateToBrickAppState
  , Name (..)
                , initialSearchEditor
                , getSearchContents
  ) where

import           Control.Lens
import           Data.Text

import qualified Graphics.Vty          as V
import           Reflex.Brick
import           Brick
import qualified Brick.Widgets.Border  as B
import qualified Brick.Widgets.Center  as C
import qualified Brick.Widgets.Edit    as WE
import Brick.Forms
  ( Form
  , newForm
  , formState
  , formFocus
  , renderForm
  , handleFormEvent
  , focusedFormInputAttr
  , invalidFormInputAttr
  , editTextField
  , (@@=)
  )
import Brick.Focus
  ( focusGetCurrent
  , focusRingCursor
  )

import Data 

getSearchContents = WE.getEditContents . _searchE

initialSearchEditor :: WE.Editor Text Name
initialSearchEditor = WE.editorText SearchField (Just 1) "??"

appStateToBrickAppState :: AppState -> ReflexBrickAppState Name
appStateToBrickAppState s = ReflexBrickAppState [window] (const Nothing) stylesMap 
-- appStateToBrickAppState s = ReflexBrickAppState [window] (focusRingCursor formFocus searchForm) stylesMap 
    where 
        window  =  B.borderWithLabel (str "Search on Hackage") $ 
                      hBox [ usageWidget
                           , C.hCenter $ padAll 1 $ vBox 
                                  [ searchForm
                                  , hBox [ packagesListWidget ]
                                  ]
                           ] 
        packageWidget p    = hBox [ txt $ p ^. name ]
        packagesListWidget = vBox [ C.hCenter $ vBox (fmap packageWidget (s ^. packages))]
        -- searchForm = renderForm  $ newForm [ editTextField search SearchField (Just 1) ] s
        searchForm = WE.renderEditor (txt . intercalate "\n") True ( s ^. searchE )
        usageWidget        = B.borderWithLabel (str "Usage") $ 
          vBox [ txt "<ENTER> : search"
               , txt "<ESC>   : quit"
               ]

-- handleBrickEvents :: Event -> AppState -> AppState

stylesMap :: AttrMap
stylesMap = attrMap V.defAttr
  [ (WE.editAttr, V.white `on` V.black)
  , (WE.editFocusedAttr, V.black `on` V.yellow)
  , (invalidFormInputAttr, V.white `on` V.red)
  , (focusedFormInputAttr, V.black `on` V.yellow)
  ]
