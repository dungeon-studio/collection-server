{-|
Module      : Main
Description : Main Module for collection-server
Copyright   : (c) Alex Brandt, 2017
License     : MIT

"Main" Module for collection-server.
-}
module Main (main) where

import Control.Monad.Extra (notM, whenM)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Servant (Application, Proxy (Proxy), serve)
import System.Directory (doesDirectoryExist)
import System.Envy (decodeEnv)

import API
import Environment
import External.Network.URI.HttpApiData ()

-- | Set up and run a "Network.Wai.Handler.Warp" server with our "Servant"
--   'API'.
main :: IO ()
main =
  do e <- either fail return =<< decodeEnv
     print e

     let p = resourcePath e
     whenM (notM $ doesDirectoryExist p) $ fail $ "path, " ++ p ++ ", does not exist"

     run (port e) $ application' p

  where application' = simpleCors . logStdout . application

application :: FilePath -> Application
application = serve (Proxy :: Proxy API) . server
