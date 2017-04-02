{-# LANGUAGE OverloadedStrings #-}
module Main where

import Protolude
import Lib

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)

import Network.HTTP.ReverseProxy (ProxyDest(..), WaiProxyResponse(..)
                                 , waiProxyTo, defaultOnExc)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.Text (Text)
import Data.String (String)
import qualified Data.Aeson as A
import Data.Aeson ((.:))

import Control.Monad.State (StateT, evalStateT)
import Control.Concurrent.Stack (Stack, register, runStack)
import Database.Etcd (Etcd, etcd, runEtcd, watchDirectoryFromIndex)

main :: IO ()
main = runStack core

core :: Stack ()
core = do
  manager <- liftIO $ newManager defaultManagerSettings
  register $
    run 8080 $ waiProxyTo (\_ -> return $ WPRProxyDest proxy) defaultOnExc manager

  register $ evalStateT watch 1

  where
    cm :: Etcd
    cm = etcd "http://localhost:2379/"

    backends :: String
    backends = "backends/"

    watch :: StateT Integer IO ()
    watch = forever $ do
      i <- get
      liftIO $ do
        runEtcd cm $ watchDirectoryFromIndex "" i :: IO (Maybe ())
        putStrLn $ "Saw event: " ++ show i
      put (i+1)

proxy :: ProxyDest
proxy = ProxyDest "localhost" 8000

data Backend = Backend { name :: Text
                       , address :: String
                       , port :: Int } deriving (Show, Eq)

instance A.FromJSON Backend where
  parseJSON (A.Object v) = Backend <$> v .: "key" <*> v .: "address" <*> v .: "port"
  parseJSON _ = empty
