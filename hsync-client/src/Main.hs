module Main where

import Network
import System.Environment (getArgs)

import HSync.Client.SyncActions

--------------------------------------------------------------------------------

main :: IO ()
main = withSocketsDo $ getArgs >>= mainWith

mainWith                     :: [String] -> IO ()
mainWith (mode:configPath:_) = case mode of
                                 "listen" -> listenMain configPath
                                 "put"    -> putMain    configPath

  -- syncMain configPath
