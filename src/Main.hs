{-|
Module      : Main
Description : Main Module for collection-server
Copyright   : (c) Alex Brandt, 2017
License     : MIT

"Main" Module for collection-server.
-}
module Main (main) where

import Control.Monad.Extra (notM, whenM)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setLogger, setPort)
import Network.Wai.Logger (withStdoutLogger)
import Servant (Application, Proxy (Proxy), serve)
import System.Directory (doesDirectoryExist)
import System.Envy (decodeEnv)

import API
import Environment
import External.Network.URI.HttpApiData ()

-- | Set up and run a "Network.Wai.Handler.Warp" server with our "Servant"
--   'API'.
main :: IO ()
main = withStdoutLogger $ \ l ->
  do e <- either fail return =<< decodeEnv
     print e

     let p = resourcePath e
     whenM (notM $ doesDirectoryExist p) $ fail $ "path, " ++ p ++ ", does not exist"

     let w = setPort (port e) $
             setLogger l
             defaultSettings

     runSettings w $ application p

application :: FilePath -> Application
application = serve (Proxy :: Proxy API) . server
