module Main where

import Prelude hiding (FilePath)

import Network

import Filesystem.Path.CurrentOS(FilePath, decodeString)

import HSync.Client.SyncActions

import System.Log.Logger


import System.Environment(getArgs)


--------------------------------------------------------------------------------

main :: IO ()
main = withSocketsDo $ do
    (configPath:rest) <- getArgs
    updateGlobalLogger "HSync" (setLevel DEBUG)
    syncMain (decodeString configPath)

  -- (mode:configPath:rest) <- getArgs
  -- case mode of
  --   "listen"    -> listenMain   configPath
  --   "put"       -> putMain      configPath
  --   "download"  -> downloadMain configPath
  --   "upload"    -> uploadMain   configPath
  --   "showstate" -> showState    configPath