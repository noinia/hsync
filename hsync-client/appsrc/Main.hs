{-# Language LambdaCase #-}
module Main where

import Network
import CLIOptions

import System.Console.CmdArgs

import qualified Filesystem.Path.CurrentOS as FP
import qualified HSync.Client.HSyncClient  as H



--------------------------------------------------------------------------------

main :: IO ()
main = withSocketsDo $ cmdArgs cliArgsMain >>= mainMode

mainMode                :: HSyncCLIOptions -> IO ()
mainMode (MainMode cfg) = H.readHSync (FP.decodeString cfg) >>= \case
    Left err -> print err >> putStrLn "\n"
    Right hs -> H.main hs
