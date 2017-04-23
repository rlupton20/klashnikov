{-# LANGUAGE OverloadedStrings #-}
module Klashnikov.Config
(   KlashnikovConfig(..)
  , loadConfigFromFile
  , Configuration
  , runConfiguration
  , KlashnikovError(..) )where

import qualified Data.Yaml as Y
import           Data.Yaml ((.:))
import           Control.Applicative ((<$>), empty)
import           Data.String (String)
import           Lib.Prelude

import           Control.Monad.Except (ExceptT(..), runExceptT, withExceptT)


data KlashnikovConfig = KC { etcd :: String
                           , backends :: String
                           , port :: Int } deriving (Eq, Show)

instance Y.FromJSON KlashnikovConfig where
  parseJSON (Y.Object v) = KC <$> v .: "etcd"
                              <*> v .: "backends"
                              <*> v .: "port"
  parseJSON _ = empty


loadConfigFromFile :: String -> Configuration KlashnikovConfig
loadConfigFromFile = mapError BadConfigFile . fromEither . Y.decodeFileEither


--------------------------------------------------------------------------------
-- Monad types for configuration process
--------------------------------------------------------------------------------

type Configuration = ExceptT KlashnikovError IO

runConfiguration :: Configuration a -> IO (Either KlashnikovError a)
runConfiguration = runExceptT

mapError :: (e -> KlashnikovError) -> ExceptT e IO a -> Configuration a
mapError = withExceptT

fromEither :: m ( Either e a ) -> ExceptT e m a
fromEither = ExceptT


--------------------------------------------------------------------------------
-- Error types for bootstrapping the system
--------------------------------------------------------------------------------

data KlashnikovError = BadConfigFile Y.ParseException |
                       NoKeyspace String deriving (Show)
