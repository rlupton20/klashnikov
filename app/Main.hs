{-# LANGUAGE OverloadedStrings #-}
module Main where

import Protolude
import Lib

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)

import Network.HTTP.ReverseProxy (ProxyDest(..), WaiProxyResponse(..)
                                 , waiProxyTo, defaultOnExc)
import Network.HTTP.Client (newManager, defaultManagerSettings)

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings
  run 8080 $ waiProxyTo (\_ -> return $ WPRProxyDest proxy) defaultOnExc manager


proxy :: ProxyDest
proxy = ProxyDest "localhost" 8000
