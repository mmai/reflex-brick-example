{-# LANGUAGE OverloadedStrings #-}

module Template ( appStateToBrickAppState
                , Name (..)
                , initialForm
                , handleSearchForm
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

initialForm :: Form FormState e Name
initialForm = newForm [ editTextField fsf SearchField (Just 1) ] (FormState "reflex")

handleSearchForm :: BrickEvent Name BrickAppEvent -> AppState -> EventM Name AppState
handleSearchForm e s = do
  newState <- handleFormEvent e (s ^. searchFormState )
  return $ s & searchFormState .~ newState

appStateToBrickAppState :: AppState -> ReflexBrickAppState Name
appStateToBrickAppState s = ReflexBrickAppState [window] (focusRingCursor formFocus searchForm) stylesMap 
    where 
        window  =  B.borderWithLabel (str "Search on Hackage") $ 
                      hBox [ usageWidget
                           , C.hCenter $ padAll 1 $ vBox 
                                  [ renderForm searchForm
                                  , hBox [ packagesListWidget ]
                                  ]
                           ] 
        packageWidget p    = hBox [ txt $ p ^. name ]
        packagesListWidget = vBox [ C.hCenter $ vBox (fmap packageWidget (s ^. packages))]
        searchForm = s ^. searchFormState
        usageWidget        = B.borderWithLabel (str "Usage") $ 
          vBox [ txt "<ENTER> : search"
               , txt "<ESC>   : quit"
               ]

stylesMap :: AttrMap
stylesMap = attrMap V.defAttr
  [ (WE.editAttr, V.white `on` V.black)
  , (WE.editFocusedAttr, V.black `on` V.yellow)
  , (invalidFormInputAttr, V.white `on` V.red)
  , (focusedFormInputAttr, V.black `on` V.yellow)
  ]
