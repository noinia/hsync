module CLIOptions where

import Prelude hiding (FilePath)

import Data.Data(Data,Typeable)

import System.Console.CmdArgs

import Filesystem.Path.CurrentOS


--------------------------------------------------------------------------------




data HSyncCLIOptions = MainMode { configFile :: FilePath
                                }
                     deriving (Show,Read,Eq,Data,Typeable)



mainMode = MainMode { configFile = def &= help "Path to the Configuration file"
                                       &= opt  "config/hsync.yaml"
                    }
