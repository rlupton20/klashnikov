{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import           Core
import           Protolude

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
import           Klashnikov.Config ( Configuration, runConfiguration
                                   , getConfigFilename, loadConfigFromFile
                                   , KlashnikovConfig, KlashnikovError(..))
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
  setup <- runConfiguration init
  case setup of
    Left e -> putStrLn $ ( show e :: String )
    Right core -> runStack core


init :: Configuration (Stack ())
init = do
  file <- getConfigFilename
  config <- loadConfigFromFile file
  buildCore config


buildCore :: KlashnikovConfig -> Configuration (Stack ())
buildCore config = do
  index <- liftIO . runEtcd configStore $ getWorkingIndex backends
  case index of
    Nothing -> throwError $ NoKeyspace backends
    Just ix -> return $ do
      env <- liftIO $ newEnvironment
      register $ runManager env ix (watch backends configStore)

      manager <- liftIO $ newManager defaultManagerSettings
      register $
        run port $ waiProxyTo (balancer env) defaultOnExc manager

  where
    configStore :: Etcd
    configStore = etcd $ Config.etcd config

    backends :: String
    backends = Config.backends config

    port :: Int
    port = Config.port config

    watch :: String -> Etcd -> Manager ()
    watch path cs = forever $ do
      env <- ask
      i <- get
      liftIO $ do
        rbe <- runEtcd cs $ listOfBackendsOnEvent path i
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
