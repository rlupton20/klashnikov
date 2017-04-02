{-# LANGUAGE OverloadedStrings #-}
import Test.Framework as TF
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit as HU
import Test.HUnit ((~:),(@=?))

import Protolude
import Klashnikov.Config

import qualified Data.Yaml as Y

main :: IO ()
main = defaultMain tests

tests :: [ TF.Test ]
tests = [ configTests ]

configTests :: TF.Test
configTests = testGroup "Config tests" . hUnitTestToTests $
  HU.TestList [ canParseConfigurationFile
              , canDetectBadConfigurationFile ]

canParseConfigurationFile :: HU.Test
canParseConfigurationFile = "Check can parse configuration file" ~:
  test
  where
    test = let yaml = "etcd: http://127.0.0.1:2379"
               expected = Just $ KC "http://127.0.0.1:2379"
               parsed = Y.decode yaml in
             expected @=? parsed

canDetectBadConfigurationFile :: HU.Test
canDetectBadConfigurationFile = "Check can detect bad configuration file" ~:
  test
  where
    test = let yaml = "etcdb: http://127.0.0.1:2379"
               expected = Nothing :: Maybe KlashnikovConfig
               parsed = Y.decode yaml in
             expected @=? parsed
