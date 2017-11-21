{-|
Module      : Internal.System.FilePathSpec
Description : Tests for Internal.System.FilePath
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Tests for "Internal.System.FilePath".
-}
module Internal.System.FilePathSpec (main, spec) where

import System.FilePath (splitDirectories, takeDirectory)
import Test.Hspec (describe, hspec, Spec)
import Test.Hspec.QuickCheck (prop)
import Test.Invariant ((<=>), deflating)

import Internal.System.FilePath

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  do describe "collapse" $
       describe "properties" $
         do prop "notElem \"..\" . splitDirectories . collapse"  $ notElem ".." . splitDirectories . collapse

            prop "notElem \".\" . splitDirectories . collapse"   $ notElem "." . splitDirectories . collapse

            prop "deflating collapse" $ deflating collapse

     describe "takeDirectoryIf" $
       describe "properties" $
         do prop "takeDirectoryIf (const True) <=> takeDirectory" $ takeDirectoryIf (const True) <=> takeDirectory
            prop "takeDirectoryIf (const False) <=> id"           $ takeDirectoryIf (const False) <=> id
