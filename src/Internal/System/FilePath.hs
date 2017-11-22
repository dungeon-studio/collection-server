{-|
Module      : Internal.System.FilePath
Description : Extra Functions for Working with FilePath
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Extra funcitons for working with 'FilePath's.
-}
module Internal.System.FilePath
  ( collapse
  , takeDirectoryIf
  ) where

import System.FilePath (joinPath, splitDirectories, takeDirectory)

collapse :: FilePath -> FilePath
collapse = joinPath . reverse . dropDots . reverse . splitDirectories
  where dropDots ("..":ss) = tail $ dropDots ss
        dropDots (".":ss)  = dropDots ss
        dropDots (s:ss)    = s : dropDots ss
        dropDots []        = []

takeDirectoryIf :: (FilePath -> Bool) -> FilePath -> FilePath
takeDirectoryIf f p = if f p
                         then takeDirectory p
                         else p
