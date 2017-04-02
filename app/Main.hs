{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Protolude
import Lib

import Network.Wai (Request, responseLBS)
import Network.Wai.Handler.Warp (run)

import Network.HTTP.ReverseProxy (ProxyDest(..), WaiProxyResponse(..)
                                 , waiProxyTo, defaultOnExc)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Types (serviceUnavailable503)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Control.Concurrent.STM.TVar (TVar, newTVarIO, writeTVar, readTVar)
import Data.Text (Text)
import qualified Data.Text.Lazy.Encoding as EL
import qualified Data.Text.Encoding as E
import qualified Data.Vector as V
import Data.Vector ((!?))
import Data.Text.Lazy (fromStrict)
import Data.String (String)
import qualified Data.Aeson as A
import Data.Aeson ((.:))
import Data.Tyro (type( >%> ), List, Extract, unwrap)

import Control.Monad.State (StateT, evalStateT)
import Control.Concurrent.Stack (Stack, register, runStack)
import Database.Etcd (Etcd, EtcdM, etcd, runEtcd, watchDirectoryFromIndex, getDirectory)



--------------------------------------------------------------------------------
-- Core types
--------------------------------------------------------------------------------

type Manager = ReaderT LoadBalancing (StateT Integer IO)

runManager :: LoadBalancing -> Integer -> Manager a -> IO a
runManager l i m = evalStateT (runReaderT m l) i


--------------------------------------------------------------------------------
-- Core application
--------------------------------------------------------------------------------

main :: IO ()
main = runStack core

core :: Stack ()
core = do
  env <- liftIO $ newEnvironment
  register $ runManager env 1 (watch $ etcd "http://localhost:2379/")

  manager <- liftIO $ newManager defaultManagerSettings
  register $
    run 8080 $ waiProxyTo (balancer env) defaultOnExc manager

  where
    backends :: String
    backends = "backends/"

    watch :: Etcd -> Manager ()
    watch configStore = forever $ do
      env <- ask
      i <- get
      liftIO $ do
        rbe <- runEtcd configStore $ listOfBackendsOnEvent backends i
        let !struct = buildBalancerStructure rbe
        atomically $ writeTVar (proxies env) struct
        putStrLn $ "Saw event: " ++ show i ++ " " ++ show rbe
      put (i+1)


balancer :: LoadBalancing -> (Request -> IO WaiProxyResponse)
balancer env = \_ -> do
  v <- atomically $ readTVar (proxies env)
  let l = V.length v
  n <- readIORef (current env)
  let i = if n >= l then 0 else n
      proxy = v !? i
  putStrLn $ "Using backend" ++ show i
  modifyIORef' (current env) (\_ -> if i + 1 == l then 0 else i + 1 )
  return $ maybe (WPRResponse $ responseLBS serviceUnavailable503 [] "") WPRProxyDest proxy


--------------------------------------------------------------------------------
-- Obtaining list of backends
--------------------------------------------------------------------------------

listOfBackendsOnEvent :: String -> Integer -> EtcdM [Backend]
listOfBackendsOnEvent path watchIndex = do
  _ <- watchDirectoryFromIndex path watchIndex :: EtcdM (Maybe ())
  be <- getDirectory path :: EtcdM (Maybe ("node" >%> "nodes" >%> List ("value" >%> Extract Text)))
  let maybeListToList = maybe [] identity
      parsed = maybeListToList ((fmap (fmap unwrap)) $ (fmap unwrap be))
      structure = maybeListToList . sequence . filter isJust . fmap parseBackend
  return $ structure parsed

buildBalancerStructure :: [Backend] -> V.Vector ProxyDest
buildBalancerStructure = let !encodeAddress = E.encodeUtf8 . address
                             !toDest = ProxyDest <$> encodeAddress <*> port in
  V.fromList . map toDest


--------------------------------------------------------------------------------
-- Backend parsing
--------------------------------------------------------------------------------

data Backend = Backend { name :: Text
                       , address :: Text
                       , port :: Int } deriving (Show, Eq)

instance A.FromJSON Backend where
  parseJSON (A.Object v) = Backend <$> v .: "name" <*> v .: "address" <*> v .: "port"
  parseJSON _ = empty

parseBackend :: Text -> Maybe Backend
parseBackend = A.decode . EL.encodeUtf8 . fromStrict


--------------------------------------------------------------------------------
-- Load balancing environment structure
--------------------------------------------------------------------------------

data LoadBalancing = LoadBalancing { current :: IORef Int
                                   , proxies :: TVar (V.Vector ProxyDest) }

newEnvironment :: IO LoadBalancing
newEnvironment = do
  c <- newIORef 0
  p <- newTVarIO V.empty
  return $ LoadBalancing c p
