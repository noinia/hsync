module Main where

import Network
import System.Environment (getArgs)

import HSync.Client.SyncActions

--------------------------------------------------------------------------------

main :: IO ()
main = withSocketsDo $ do
  (mode:configPath:rest) <- getArgs
  case mode of
    "listen"   -> listenMain   configPath
    "put"      -> putMain      configPath
    "download" -> downloadMain configPath
