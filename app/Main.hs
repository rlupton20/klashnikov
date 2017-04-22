{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import           Protolude
import           Core

import           Network.Wai (Request, responseLBS)
import           Network.Wai.Handler.Warp (run)

import           Control.Concurrent.STM.TVar ( TVar, newTVarIO
                                             , writeTVar, readTVar )
import           Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import           Data.String (String)
import           Data.Vector ((!?))
import qualified Data.Vector as V
import           Network.HTTP.Client (newManager, defaultManagerSettings)
import           Network.HTTP.ReverseProxy (ProxyDest(..), WaiProxyResponse(..)
                                           , waiProxyTo, defaultOnExc)
import           Network.HTTP.Types (serviceUnavailable503)

import           Control.Concurrent.Stack (Stack, register, runStack)
import           Database.Etcd ( Etcd, etcd, runEtcd )
import           Etcd.Backends
import           Etcd.Indices
import qualified Klashnikov.Config as Config



--------------------------------------------------------------------------------
-- Core types
--------------------------------------------------------------------------------

type Manager = ReaderT BalancerEnvironment (StateT Integer IO)

runManager :: BalancerEnvironment -> Integer -> Manager a -> IO a
runManager l i m = evalStateT (runReaderT m l) i


--------------------------------------------------------------------------------
-- Load balancing environment structure
--------------------------------------------------------------------------------

data BalancerEnvironment = BE { current :: IORef Int
                              , proxies :: TVar (V.Vector ProxyDest) }

newEnvironment :: IO BalancerEnvironment
newEnvironment = do
  c <- newIORef 0
  p <- newTVarIO V.empty
  return $ BE c p


--------------------------------------------------------------------------------
-- Core application
--------------------------------------------------------------------------------

main :: IO ()
main = do
  cfg <- Config.loadConfigFromFile "klashnikov.yaml"
  case cfg of
    Left e -> putStrLn $ "Bad configuration file: " ++ show e
    Right config -> do
      let configStore = etcd $ Config.etcd config
          backends = Config.backends config
      index <- runEtcd configStore $ getWorkingIndex backends
      case index of
        Just ix -> runStack $ core configStore backends ix
        Nothing -> putStrLn $ "No keyspace configured at: " ++ backends


core :: Etcd -> String -> Integer -> Stack ()
core configStore backends index = do
  env <- liftIO $ newEnvironment
  register $ runManager env index (watch configStore)

  manager <- liftIO $ newManager defaultManagerSettings
  register $
    run 8080 $ waiProxyTo (balancer env) defaultOnExc manager

  where
    watch :: Etcd -> Manager ()
    watch cs = forever $ do
      env <- ask
      i <- get
      liftIO $ do
        rbe <- runEtcd cs $ listOfBackendsOnEvent backends i
        let !struct = buildBalancerStructure rbe
        atomically $ writeTVar (proxies env) struct
        putStrLn $ "Saw event: " ++ show i ++ " " ++ show rbe
      put (i+1)


balancer :: BalancerEnvironment -> (Request -> IO WaiProxyResponse)
balancer env = \_ -> do
  v <- atomically $ readTVar (proxies env)
  let l = V.length v
  n <- readIORef (current env)
  let i = if n >= l then 0 else n
      proxy = v !? i
  modifyIORef' (current env) (\_ -> if i + 1 == l then 0 else i + 1 )
  let respond503 = (WPRResponse $ responseLBS serviceUnavailable503 [] "")
  return $ maybe respond503 WPRProxyDest proxy
