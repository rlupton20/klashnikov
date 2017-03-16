{-# LANGUAGE OverloadedStrings #-}
module Main where

import Protolude
import Lib

import Network.Wai (Application, responseLBS)
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = print "Klashnikov" >> run 8080 balancer

balancer :: Application
balancer _ respond = respond $ responseLBS status200
                     [("Content-Type", "text/plain")]
                     "Test"
