{-# LANGUAGE OverloadedStrings #-}
module Klashnikov.Config
(   KlashnikovConfig(..)
  , getConfigFilename
  , loadConfigFromFile
  , Configuration
  , runConfiguration
  , KlashnikovError(..) )where

import           Control.Applicative ((<$>), empty)
import           Data.String (String)
import           Data.Yaml ((.:))
import qualified Data.Yaml as Y
import           Lib.Prelude
import           System.Environment (getArgs)

import           Control.Monad.Except (ExceptT(..), runExceptT, withExceptT)


data KlashnikovConfig = KC { etcd :: String
                           , backends :: String
                           , port :: Int } deriving (Eq, Show)

instance Y.FromJSON KlashnikovConfig where
  parseJSON (Y.Object v) = KC <$> v .: "etcd"
                              <*> v .: "backends"
                              <*> v .: "port"
  parseJSON _ = empty


--------------------------------------------------------------------------------
-- Monadic computations for initial configuration
--------------------------------------------------------------------------------

getConfigFilename :: Configuration String
getConfigFilename = do
  as <- liftIO $ getArgs
  case as of
    [] -> throwError NoConfigFileSpecified
    [file] -> return file
    _ -> throwError TooManyCommandLineParameters


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

data KlashnikovError = NoConfigFileSpecified |
                       TooManyCommandLineParameters |
                       BadConfigFile Y.ParseException |
                       NoKeyspace String deriving (Show)
