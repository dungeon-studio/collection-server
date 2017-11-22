{-|
Module      : External.Servant.API.ContentTypes.CollectionJSONSpec
Description : Tests for External.Servant.API.ContentTypes.CollectionJSON
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Tests for "External.Servant.API.ContentTypes.CollectionJSON".
-}
module External.Servant.API.ContentTypes.CollectionJSONSpec (main, spec) where

import Data.CollectionJSON (Collection)
import Servant.API (mimeRender, mimeUnrender)
import Servant (Proxy (Proxy))
import Test.Hspec (describe, hspec, Spec)
import Test.Hspec.QuickCheck (modifyMaxSize, prop)
import Test.Invariant ((<=>))

import External.Data.CollectionJSON.Arbitrary ()
import External.Servant.API.ContentTypes.CollectionJSON
import Internal.Data.CollectionJSON ()

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "properties" $ modifyMaxSize (const 25) $
    prop "mimeUnrender (Proxy :: Proxy CollectionJSON) . mimeRender (Proxy :: Proxy CollectionJSON) == Right" 
      (mimeUnrender (Proxy :: Proxy CollectionJSON) . mimeRender (Proxy :: Proxy CollectionJSON) <=> Right :: Collection -> Bool)
