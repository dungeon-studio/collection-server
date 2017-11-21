{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : TypesSpec
Description : Tests for Types
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Tests for "Types".
-}
module TypesSpec (main, spec) where

import Data.CollectionJSON (Collection (..), Datum (..), Item (..))
import Data.Maybe (fromJust)
import Data.Text (Text)
import Network.URI (parseRelativeReference, URI)
import Test.Hspec (describe, hspec, it, Selector, shouldThrow, shouldReturn, Spec)

import Types

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  do describe "Item Constructors" $
       do describe "iDirectory" $
            do it "should throw DoesNotExist" $
                 iDirectory (toURI "/invalid/non-existent") "resources/invalid/non-existent" `shouldThrow` doesNotExist "resources/invalid/non-existent"

               it "should return a linked item" $
                 iDirectory (toURI "/mixed/subcollection") "resources/mixed/subcollection" `shouldReturn` item (toURI "/mixed/subcollection") (Just "subcollection")

          describe "iFile" $
            do it "should throw DoesNotExist" $
                 iFile (toURI "/invalid/non-existent") "resources/invalid/non-existent.yaml" `shouldThrow` doesNotExist "resources/invalid/non-existent.yaml"

               it "should throw DoesNotParse" $
                 iFile (toURI "/invalid/item") "resources/invalid/item.yaml" `shouldThrow` doesNotParse "resources/invalid/item.yaml"

               it "should return a valid item" $
                 iFile (toURI "/mixed/item") "resources/mixed/item.yaml" `shouldReturn` item (toURI "/mixed/item") Nothing

          describe "fromPath'" $
            do it "should throw DoesNotExist" $
                 fromPath' (toURI "/invalid/non-existent") "resources/invalid/non-existent" `shouldThrow` doesNotExist "resources/invalid/non-existent"

               it "should throw DoesNotParse" $
                 fromPath' (toURI "/invalid/item") "resources/invalid/item.yaml" `shouldThrow` doesNotParse "resources/invalid/item.yaml"

               it "should parse a valid file" $
                 do fromPath' (toURI "/mixed/item") "resources/mixed/item.yaml" `shouldReturn` item (toURI "/mixed/item") Nothing
                    fromPath' (toURI "/items/item") "resources/items/item.yaml" `shouldReturn` item (toURI "/items/item") Nothing

               it "should parse a valid directory" $
                 fromPath' (toURI "/mixed/subcollection") "resources/mixed/subcollection" `shouldReturn` item (toURI "/mixed/subcollection") (Just "subcollection")

     describe "Collection Constructors" $
       do describe "cDirectory" $
            do it "should throw DoesNotExist" $
                 cDirectory "resources/invalid/non-existent" (toURI "/invalid/non-existent") `shouldThrow` doesNotExist "resources/invalid/non-existent"

               it "should throw DoesNotParse" $
                 cDirectory "resources/invalid" (toURI "/invalid") `shouldThrow` doesNotParse "resources/invalid/item.yaml"

               it "should parse a valid directory" $
                 cDirectory "resources/items" (toURI "/items") `shouldReturn` collection (toURI "/items") [item (toURI "/items/item") Nothing]

               it "should parse a mixed directory" $
                 cDirectory "resources/mixed" (toURI "/mixed") `shouldReturn` collection (toURI "/mixed") [item (toURI "/mixed/subcollection") (Just "subcollection"), item (toURI "/mixed/item") Nothing]

          describe "cFile" $
            do it "should throw DoesNotExist" $
                 cFile "resources/invalid/non-existent.yaml" (toURI "/invalid/non-existent") `shouldThrow` doesNotExist "resources/invalid/non-existent.yaml"

               it "should throw DoesNotParse" $
                 cFile "resources/invalid/item.yaml" (toURI "/invalid/item") `shouldThrow` doesNotParse "resources/invalid/item.yaml"

               it "should parse a valid file" $
                 cFile "resources/items/item.yaml" (toURI "/items/item") `shouldReturn` collection (toURI "/items") [item (toURI "/items/item") Nothing]

          describe "fromPath" $
            do it "should throw DoesNotExist" $
                 fromPath "resources/invalid/non-existent" (toURI "/invalid/non-existent") `shouldThrow` doesNotExist "resources/invalid/non-existent"

               it "should throw DoesNotParse" $
                 fromPath "resources/invalid" (toURI "/invalid") `shouldThrow` doesNotParse "resources/invalid/item.yaml"

               it "should parse a valid file" $
                 fromPath "resources/items/item.yaml" (toURI "/items/item") `shouldReturn` collection (toURI "/items") [item (toURI "/items/item") Nothing]

               it "should parse a valid directory" $
                 fromPath "resources/items" (toURI "/items") `shouldReturn` collection (toURI "/items") [item (toURI "/items/item") Nothing]

               it "should parse a mixed directory" $
                 fromPath "resources/mixed" (toURI "/mixed") `shouldReturn` collection (toURI "/mixed") [item (toURI "/mixed/subcollection") (Just "subcollection"), item (toURI "/mixed/item") Nothing]

collection :: URI -> [Item] -> Collection
collection u xs = Collection
  { cVersion  = "1.0"
  , cHref     = u
  , cLinks    = []
  , cItems    = xs
  , cQueries  = []
  , cTemplate = Nothing
  , cError    = Nothing
  }

item :: URI -> Maybe Text -> Item
item u n = Item
    { iHref  = u
    , iData  = n'
    , iLinks = []
    }
  where n' = maybe [] (\ x -> [ Datum "name" (Just x) (Just x) ]) n

doesNotExist :: FilePath -> Selector DirectoryCollectionException
doesNotExist p (DoesNotExist p') = p == p'
doesNotExist _ _                 = False

doesNotParse :: FilePath -> Selector DirectoryCollectionException -- TODO Match message?
doesNotParse p (DoesNotParse p' _) = p == p'
doesNotParse _ _                   = False

toURI :: String -> URI
toURI = fromJust . parseRelativeReference
