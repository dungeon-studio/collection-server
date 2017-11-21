{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : API
Description : HTTP API for collection-server
Copyright   : (c) Alex Brandt, 2017
License     : MIT

API for collection-server.
-}
module API
  ( API
  , server
  ) where

import Control.Monad.Catch (catch)
import Control.Monad (unless)
import Data.CollectionJSON (Collection)
import Data.List (isPrefixOf)
import Data.Maybe (fromJust)
import Network.URI (parseRelativeReference, URI)
import Servant ((:>), CaptureAll, Get, Handler, Server, throwError)
import System.FilePath ((</>), joinPath, takeBaseName)

import Errors
import External.Servant.API.ContentTypes.CollectionJSON (CollectionJSON)
import Internal.Data.CollectionJSON ()
import Internal.System.FilePath (collapse, takeDirectoryIf)
import Types

-- | "Servant" API for statis @application/vnd.collection+json@ resources.
type API = CaptureAll "path" FilePath :> Get '[CollectionJSON] Collection

-- | "Servant" 'Server' for static @application/vnd.collection+json@ resources.
server :: FilePath -> Server API
server = handler

handler :: FilePath -> [FilePath] -> Handler Collection
handler r ss =
  do unless (r `isPrefixOf` p') $ throwError $ collection404 p u -- Check for directory escapes.

     fromPath p' u `catch` throwServantErr u

  where p  = joinPath ss
        p' = takeDirectoryIf ((== "index") . takeBaseName) $ collapse $ r </> p

        u  = fromJust $ parseRelativeReference p

throwServantErr :: URI -> DirectoryCollectionException -> Handler Collection
throwServantErr u (DoesNotExist p)   = throwError $ collection404 p u
throwServantErr u (DoesNotParse _ m) = throwError $ collection500 m u
