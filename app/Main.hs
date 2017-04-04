{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import           Protolude

import           Network.Wai (Request, responseLBS)
import           Network.Wai.Handler.Warp (run)

import           Control.Concurrent.STM.TVar ( TVar, newTVarIO
                                             , writeTVar, readTVar )
import           Data.Aeson ((.:))
import qualified Data.Aeson as A
import           Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import           Data.String (String)
import           Data.Text (Text)
import qualified Data.Text.Encoding as E
import           Data.Text.Lazy (fromStrict)
import qualified Data.Text.Lazy.Encoding as EL
import           Data.Tyro (type( >%> ), List, Extract, unwrap)
import           Data.Vector ((!?))
import qualified Data.Vector as V
import           Network.HTTP.Client (newManager, defaultManagerSettings)
import           Network.HTTP.ReverseProxy (ProxyDest(..), WaiProxyResponse(..)
                                           , waiProxyTo, defaultOnExc)
import           Network.HTTP.Types (serviceUnavailable503)

import           Control.Concurrent.Stack (Stack, register, runStack)
import           Database.Etcd ( Etcd, EtcdM, etcd, runEtcd
                               , watchDirectoryFromIndex, getDirectory )



--------------------------------------------------------------------------------
-- Core types
--------------------------------------------------------------------------------

type Manager = ReaderT BalancerEnvironment (StateT Integer IO)

runManager :: BalancerEnvironment -> Integer -> Manager a -> IO a
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


--------------------------------------------------------------------------------
-- Obtaining list of backends
--------------------------------------------------------------------------------

type ExtractNodeValues a = "node" >%> "nodes" >%> List ("value" >%> Extract a)

listOfBackendsOnEvent :: String -> Integer -> EtcdM [Backend]
listOfBackendsOnEvent path watchIndex = do
  _ <- watchDirectoryFromIndex path watchIndex :: EtcdM (Maybe ())
  be <- getDirectory path :: EtcdM (Maybe (ExtractNodeValues Text))
  let maybeListToList = maybe [] identity
      parsed = maybeListToList ((fmap (fmap unwrap)) $ (fmap unwrap be))
      structure = maybeListToList . sequence . filter isJust . fmap parseBackend
  return $ structure parsed

buildBalancerStructure :: [Backend] -> V.Vector ProxyDest
buildBalancerStructure = let encodeAddress = E.encodeUtf8 . address
                             toDest = ProxyDest <$> encodeAddress <*> port in
  V.fromList . map toDest


--------------------------------------------------------------------------------
-- Backend parsing
--------------------------------------------------------------------------------

data Backend = Backend { name :: Text
                       , address :: Text
                       , port :: Int } deriving (Show, Eq)

instance A.FromJSON Backend where
  parseJSON (A.Object v) = Backend <$>
    v .: "name" <*> v .: "address" <*> v .: "port"
  parseJSON _ = empty

parseBackend :: Text -> Maybe Backend
parseBackend = A.decode . EL.encodeUtf8 . fromStrict


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
