{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : External.Servant.API.ContentTypes.CollectionJSON
Description : Servant ContentType for @application/vnd.collection+json@
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Servant ContentType for @application/vnd.collection+json@---a content type
created by M. Amundsen and documented at
<http://amundsen.com/media-types/collection/>.
-}
module External.Servant.API.ContentTypes.CollectionJSON where

import Data.Aeson (eitherDecode, encode)
import Data.CollectionJSON (FromCollection (fromCollection), ToCollection (toCollection))
import Network.HTTP.Media ((//))
import Servant.API (Accept (..), MimeRender (..), MimeUnrender (..))

-- | Content type suitable for use with "Servant".
data CollectionJSON

-- | @application/vnd.collection+json@
instance Accept CollectionJSON where
  contentType _ = "application" // "vnd.collection+json" -- TODO profile parameter

-- | Convert a 'ToCollection' instance to a 'ByteString'.
instance ToCollection a => MimeRender CollectionJSON a where
  mimeRender _ = encode . toCollection

-- | Convert a 'ByteString' to a 'FromCollection' instance.
instance FromCollection a => MimeUnrender CollectionJSON a where
  mimeUnrender _ = fmap fromCollection . eitherDecode
