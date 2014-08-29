{-# Language DeriveDataTypeable #-}
module CLIOptions where

import System.Console.CmdArgs

--------------------------------------------------------------------------------




data HSyncCLIOptions = MainMode { configFile :: FilePath
                                }
                     deriving (Show,Read,Eq,Data,Typeable)


cliArgsMain :: HSyncCLIOptions
cliArgsMain = MainMode { configFile = "config/hsync.yaml"
                           &= help "Path to the Configuration file"
                           &= opt  "config/hsync.yaml"
                       }
