{-# LANGUAGE OverloadedStrings #-}
module Klashnikov.Config where

import qualified Data.Yaml as Y
import Data.Yaml ((.:))
import Control.Applicative ((<$>), empty)
import Data.String (String)
import Lib.Prelude

data KlashnikovConfig = KC { etcd :: String
                           , backends :: String } deriving (Eq, Show)

instance Y.FromJSON KlashnikovConfig where
  parseJSON (Y.Object v) = KC <$> v .: "etcd" <*> v .: "backends"
  parseJSON _ = empty

loadConfigFromFile :: String -> IO (Either Y.ParseException KlashnikovConfig)
loadConfigFromFile = Y.decodeFileEither
