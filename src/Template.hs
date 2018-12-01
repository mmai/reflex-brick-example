{-# LANGUAGE OverloadedStrings #-}

module Template (
    appStateToBrickAppState
  ) where

import           Control.Lens
import           Data.Text

import qualified Graphics.Vty          as V
import           Reflex.Brick
import           Brick
import qualified Brick.Widgets.Border  as B
import qualified Brick.Widgets.Center  as C
import qualified Brick.Widgets.Edit    as WE
-- import Brick.Forms
--   ( Form
--   , newForm
--   , formState
--   , formFocus
--   , setFieldValid
--   , renderForm
--   , handleFormEvent
--   , invalidFields
--   , allFieldsValid
--   , focusedFormInputAttr
--   , invalidFormInputAttr
--   , radioField
--   , editShowableField
--   , editTextField
--   , (@@=)
--   )

import Data 

data Name = NameSearch deriving (Eq, Ord)

appStateToBrickAppState :: AppState -> ReflexBrickAppState n
appStateToBrickAppState s = ReflexBrickAppState [window] (const Nothing) (attrMap V.defAttr []) 
  -- where window  =  B.borderWithLabel (str "Search on Hackage") $ content
    where 
        window  =  B.borderWithLabel (str "Search on Hackage") $ hBox [ usageWidget
                      , content
                      ] 
        content = vBox [ searchWidget
                       , C.hCenter $ hBox [ packagesListWidget
                                          ]
                       ]
        packageWidget p    = hBox [ txt $ p ^. name ]
        packagesListWidget = padAll 1 $ vBox [ C.hCenter $ vBox (fmap packageWidget (s ^. packages))]
        usageWidget        = B.borderWithLabel (str "Usage") $ 
          vBox [ txt "'s' : search"
               , txt "'q' : quit"
               ]

searchEditor = WE.editorText NameSearch (Just 1) "reflex"
searchWidget = WE.renderEditor (txt . Data.Text.concat) True searchEditor


-- data Search = FormState { _search    :: Text }
--
-- mkForm :: Text -> Form Text e Name
-- mkForm =
--     let label s w = padBottom (Pad 1) $
--                     (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
--     in newForm [ label "Search" @@=
--                    editTextField name NameField (Just 1)
--                ]
