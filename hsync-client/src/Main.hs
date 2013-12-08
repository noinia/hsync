module Main where

import Network
import System.Environment (getArgs)

import HSync.Client.SyncActions

--------------------------------------------------------------------------------

main :: IO ()
main = withSocketsDo $ getArgs >>= mainWith

mainWith                :: [String] -> IO ()
mainWith (configPath:_) = syncMain configPath
