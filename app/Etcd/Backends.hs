{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Etcd.Backends
( listOfBackendsOnEvent
, getListOfBackends ) where


import Core (maybeListToList)
import Protolude
import Types

import Database.Etcd ( EtcdM, watchDirectoryFromIndex, getDirectory )
import Data.Tyro (type( >%> ), List, Extract, unwrap)
import Data.String (String)


--------------------------------------------------------------------------------
-- Obtaining list of backends
--------------------------------------------------------------------------------

type ExtractNodeValues a = "node" >%> "nodes" >%> List ("value" >%> Extract a)


unwrapExtractNodeValues :: ExtractNodeValues a -> [a]
unwrapExtractNodeValues = fmap unwrap . unwrap


listOfBackendsOnEvent :: String -> Integer -> EtcdM [Backend]
listOfBackendsOnEvent path watchIndex = do
  _ <- watchDirectoryFromIndex path watchIndex :: EtcdM (Maybe ())
  getListOfBackends path


getListOfBackends :: String -> EtcdM [Backend]
getListOfBackends path = do
  be <- getDirectory path :: EtcdM (Maybe (ExtractNodeValues Text))
  let parsed = maybeListToList $ fmap unwrapExtractNodeValues be
      structure = maybeListToList . sequence . filter isJust . fmap parseBackend
  return $ structure parsed
