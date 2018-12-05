{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Data where

import           Data.Text
import           Data.Aeson
import           Control.Lens.TH

import           GHC.Generics
import           Data.ByteString.Lazy.Internal
import qualified Brick.Widgets.Edit    as WE
import           Brick.Forms (Form)

data AppState = AppState
  { _searchFormState  :: Form FormState BrickAppEvent Name
  , _packages :: [Package]
  }


-- Brick types
data Name = SearchField deriving (Eq, Ord, Show)
data BrickAppEvent = BrickAppEvent
data FormState = FormState { _fsf :: Text }

newtype Package = Package { _name  :: Text }

instance ToJSON Package where
  toJSON p = object [ "name" .= _name p ]

instance FromJSON Package where
  parseJSON = withObject "package" $ \o ->
    Package <$> o .: "name"

makeLenses ''Package
makeLenses ''AppState
makeLenses ''FormState
