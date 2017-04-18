{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Etcd.Indices (
  getWorkingIndex ) where

import Core (maybeListToList)
import Protolude

import Database.Etcd ( EtcdM, getDirectory )
import           Database.Etcd.JSON (ModifiedIndex(..))
import Data.Tyro (type( >%> ), List, Extract, unwrap)
import Data.String (String)


--------------------------------------------------------------------------------
-- Obtaining current modified indices
--------------------------------------------------------------------------------


type ExtractNodeModifiedIndex =
  "node" >%> "nodes" >%> List ("modifiedIndex" >%> Extract Integer)

getCurrentDirectoryIndex :: String -> EtcdM (Maybe Integer)
getCurrentDirectoryIndex path = do
  ix <- getDirectory path :: EtcdM (Maybe ModifiedIndex)
  return $ fmap index ix
  where
    index :: ModifiedIndex -> Integer
    index (ModifiedIndex ix) = ix

getNodesIndices :: String -> EtcdM [Integer]
getNodesIndices path = do
  ixs <- getDirectory path :: EtcdM (Maybe ExtractNodeModifiedIndex)
  let simplified = maybeListToList $ fmap unwrapModifiedIndices ixs
  return simplified
  where
    unwrapModifiedIndices :: ExtractNodeModifiedIndex -> [Integer]
    unwrapModifiedIndices = fmap unwrap . unwrap

getWorkingIndex :: String -> EtcdM (Maybe Integer)
getWorkingIndex path = do
  ixs <- getNodesIndices path
  case ixs of
    [] -> getCurrentDirectoryIndex path
    xs -> return . Just $ maximum xs
