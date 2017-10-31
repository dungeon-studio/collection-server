{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Network.URI.Missing
Description : Missing Instances and Functions from Network.URI
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Missing instances and functions from "Network.URI"
-}
module Network.URI.Missing where

import Data.Either.Utils (maybeToEither)
import Data.Text (unpack)
import Network.URI (parseURIReference, URI, uriPath)
import Web.HttpApiData (FromHttpApiData (parseUrlPiece))

instance FromHttpApiData URI where
  parseUrlPiece = maybeToEither "invalid URI" . parseURIReference . unpack

-- | Add 'String' to 'URI''s 'uriPath'.
append :: URI -> String -> URI
append b c = b { uriPath = uriPath b ++ "/" ++ c }
