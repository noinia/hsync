module Main where

import           HSync.FileTree
import           HSync.Diff
-- import           Control.Lens
import           System.OsPath

--------------------------------------------------------------------------------

main :: IO ()
main = print =<< readFileTreeWithLastModified =<< encodeFS "/home/frank/tmp/3dhull_dana/"


--------------------------------------------------------------------------------
