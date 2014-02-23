module Main where

import Network
import System.Environment (getArgs)

import HSync.Client.SyncActions

import System.Log.Logger

--------------------------------------------------------------------------------

main :: IO ()
main = withSocketsDo $ do
    (configPath:rest) <- getArgs
    updateGlobalLogger "HSync" (setLevel DEBUG)
    syncMain configPath

  -- (mode:configPath:rest) <- getArgs
  -- case mode of
  --   "listen"    -> listenMain   configPath
  --   "put"       -> putMain      configPath
  --   "download"  -> downloadMain configPath
  --   "upload"    -> uploadMain   configPath
  --   "showstate" -> showState    configPath
