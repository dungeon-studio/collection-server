{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module      : Internal.Data.CollectionJSON
Description : FromCollection and ToCollection Instances for Collection
Copyright   : (c) Alex Brandt, 2017
License     : MIT

FromCollection and ToCollection instances for 'Collection'.
-}
module Internal.Data.CollectionJSON where

import Data.CollectionJSON (Collection, FromCollection (..), ToCollection (..))

instance FromCollection Collection where
  fromCollection = id

instance ToCollection Collection where
  toCollection = id
