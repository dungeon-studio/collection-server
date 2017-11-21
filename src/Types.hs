{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Types
Description : Types for collection-server
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Types for collection-server.
-}
module Types
  ( DirectoryCollectionException
    ( DoesNotParse
    , DoesNotExist
    )
  , fromPath
  , cFile
  , cDirectory
  , fromPath'
  , iFile
  , iDirectory
  , parse
  ) where

import Control.Monad.Catch (catch, Exception, MonadCatch, MonadThrow, onException, throwM)
import Control.Monad.Extra (unlessM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.CollectionJSON
import Data.List (isPrefixOf, intercalate)
import Data.Maybe (fromJust)
import Data.Text (pack)
import Data.Typeable (Typeable)
import Data.Yaml (decodeFileEither, FromJSON)
import Network.URI (parseRelativeReference, pathSegments, relativeTo, URI, uriPath)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>), (<.>), takeBaseName)

-- * Exceptions

data DirectoryCollectionException = DoesNotParse FilePath String
                                  | DoesNotExist FilePath
  deriving (Typeable)

instance Show DirectoryCollectionException where
  show (DoesNotParse p m) = "could not parse " ++ p ++ ": " ++ m
  show (DoesNotExist p)   = p ++ " does not exist"

instance Exception DirectoryCollectionException

-- * FilePath Based Collection Constructors

fromPath :: (MonadCatch m, MonadIO m) => FilePath -> URI -> m Collection
fromPath p u = cDirectory p u `catch` (\ (e :: DirectoryCollectionException) -> cFile p u `onException` throwM e)

cFile :: (MonadIO m, MonadThrow m) => FilePath -> URI -> m Collection
cFile p u =
  do i <- iFile u p
     return Collection
       { cVersion  = "1.0"
       , cHref     = u { uriPath = '/' : intercalate "/" (init $ pathSegments u) }
       , cLinks    = []
       , cItems    = [i]
       , cQueries  = []
       , cTemplate = Nothing
       , cError    = Nothing
       }

cDirectory :: (MonadCatch m, MonadIO m) => FilePath -> URI -> m Collection
cDirectory p u =
  do unlessM (liftIO $ doesDirectoryExist p) $ throwM $ DoesNotExist p

     xs <- mapM (fromPath' u) =<< liftIO (ls p)

     return Collection
       { cVersion  = "1.0"
       , cHref     = u
       , cLinks    = []
       , cItems    = xs
       , cQueries  = []      -- TODO Generic queries implementation.
       , cTemplate = Nothing -- TODO Allow collection items to be added?
       , cError    = Nothing
       }
  where ls = fmap (map (p </>) . filter (not . isPrefixOf ".")) . listDirectory

-- FilePath Based Item Constructors

fromPath' :: (MonadCatch m, MonadIO m) => URI -> FilePath -> m Item
fromPath' u p = iFile u p `catch` h
  where h (DoesNotExist _) = iDirectory u p
        h e                = throwM e

iFile :: (MonadIO m, MonadThrow m) => URI -> FilePath -> m Item
iFile u p =
  do unlessM (liftIO $ doesFileExist p') $ throwM $ DoesNotExist p'
     i <- parse p'
     return $ i { iHref = u }
  where p' = p <.> "yaml"

iDirectory :: (MonadIO m, MonadThrow m) => URI -> FilePath -> m Item
iDirectory u p =
  do unlessM (liftIO $ doesDirectoryExist p) $ throwM $ DoesNotExist p
     return Item
       { iHref  = fromJust (parseRelativeReference n) `relativeTo` u
       , iData  = [ Datum "name" (Just $ pack n) (Just $ pack n)
                  ]
       , iLinks = []
       }
  where n = takeBaseName p

-- * Other Functions

parse :: (FromJSON a, MonadIO m, MonadThrow m) => FilePath -> m a
parse p = either (throwM . DoesNotParse p . show) return =<< liftIO (decodeFileEither p)
