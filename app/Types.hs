module Types (
  Backend(..)
, parseBackend ) where

import           Protolude
import qualified Data.Aeson as A
import           Data.Aeson ((.:))
import qualified Data.Text.Lazy.Encoding as EL


data Backend = Backend { name :: Text
                       , address :: Text
                       , port :: Int } deriving (Show, Eq)


--------------------------------------------------------------------------------
-- Backend parsing
--------------------------------------------------------------------------------

instance A.FromJSON Backend where
  parseJSON (A.Object v) = Backend <$>
    v .: "name" <*> v .: "address" <*> v .: "port"
  parseJSON _ = empty


parseBackend :: Text -> Maybe Backend
parseBackend = A.decode . EL.encodeUtf8 . fromStrict
