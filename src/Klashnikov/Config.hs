{-# LANGUAGE OverloadedStrings #-}
module Klashnikov.Config where

import qualified Data.Yaml as Y
import Data.Yaml ((.:))
import Control.Applicative ((<$>), empty)
import Data.Text (Text)
import Data.String (String)
import Lib.Prelude

data KlashnikovConfig = KC { etcd :: Text } deriving (Eq, Show)

instance Y.FromJSON KlashnikovConfig where
  parseJSON (Y.Object v) = KC <$> v .: "etcd"
  parseJSON _ = empty

loadConfigFromFile :: String -> IO (Either Y.ParseException KlashnikovConfig)
loadConfigFromFile = Y.decodeFileEither
