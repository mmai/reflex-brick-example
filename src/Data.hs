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

data AppState = AppState
  { _search   :: Text
  , _searchE  :: WE.Editor Text Name
  , _packages :: [Package]
  }

data Name = SearchField deriving (Eq, Ord, Show)

newtype Package = Package { _name  :: Text }

instance ToJSON Package where
  toJSON p = object [ "name" .= _name p ]

instance FromJSON Package where
  parseJSON = withObject "package" $ \o ->
    Package <$> o .: "name"

makeLenses ''Package
makeLenses ''AppState
