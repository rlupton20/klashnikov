module Core
( buildBalancerStructure ) where

import Protolude
import Types

import qualified Data.Vector as V
import           Network.HTTP.ReverseProxy (ProxyDest(..))
import qualified Data.Text.Encoding as E


buildBalancerStructure :: [Backend] -> V.Vector ProxyDest
buildBalancerStructure = let encodeAddress = E.encodeUtf8 . address
                             toDest = ProxyDest <$> encodeAddress <*> port in
  V.fromList . map toDest
