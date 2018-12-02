{-# LANGUAGE OverloadedStrings #-}

module Template (
    appStateToBrickAppState
  , Name (..)
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
  , setFieldValid
  , renderForm
  , handleFormEvent
  , invalidFields
  , allFieldsValid
  , focusedFormInputAttr
  , invalidFormInputAttr
  , radioField
  , editShowableField
  , editTextField
  , (@@=)
  )
import Brick.Focus
  ( focusGetCurrent
  , focusRingCursor
  )

import Data 

data Name = SearchField deriving (Eq, Ord, Show)

appStateToBrickAppState :: AppState -> ReflexBrickAppState Name
appStateToBrickAppState s = ReflexBrickAppState [window] (focusRingCursor formFocus s) (attrMap V.defAttr []) 
-- appStateToBrickAppState s = ReflexBrickAppState [window] (const Nothing) (attrMap V.defAttr []) 
  -- where window  =  B.borderWithLabel (str "Search on Hackage") $ content
    where 
        window  =  B.borderWithLabel (str "Search on Hackage") $ hBox [ usageWidget
                      , content
                      ] 
        content = vBox [ C.hCenter searchForm
                       , C.hCenter $ hBox [ packagesListWidget
                                          ]
                       ]
        packageWidget p    = hBox [ txt $ p ^. name ]
        packagesListWidget = padAll 1 $ vBox [ C.hCenter $ vBox (fmap packageWidget (s ^. packages))]
        searchForm = renderForm . newForm [ editTextField search SearchField (Just 1) ] $ s
        usageWidget        = B.borderWithLabel (str "Usage") $ 
          vBox [ txt "'s' : search"
               , txt "'q' : quit"
               ]

