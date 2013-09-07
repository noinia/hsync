{-# Language TemplateHaskell #-}
module HSync.Common.FSTree( readFSTree
                          ) where


import Storage.Hashed.Plain(readPlainTree)
import Storage.Hashed.Darcs(darcsTreeHash, darcsAddMissingHashes)
import Storage.Hashed.Hash
import Storage.Hashed.AnchoredPath
import Storage.Hashed.Tree( Tree )


import System.Directory

import Storage.Hashed.Index

import qualified Storage.Hashed.Tree as HT
import qualified Storage.Hashed.Index as I

import Data.Aeson.TH


$(deriveJSON id ''Name)
$(deriveJSON id ''AnchoredPath)
$(deriveJSON id ''Hash)


readFSTree   :: FilePath -> IO (Tree IO)
readFSTree p = darcsAddMissingHashes =<< readPlainTree p





printTree = print . map (\(a,b) -> (a, HT.itemHash b)) . HT.list


test = do
  -- print =<< getCurrentDirectory
  tree <- readPlainTree "/Users/frank/tmp/synced"
  print "plain tree"
  printTree tree
  idx <- updateIndexFrom "/Users/frank/tmp/index" darcsTreeHash tree
  -- idx  <- readIndex "/Users/frank/tmp/index" darcsTreeHash
  idxTree <- updateIndex idx
  print "unexpanded tree?"
  printTree idxTree
  fTree <- updateIndex $ I.filter (\a b -> True) idx
  print "FTree"
  printTree fTree
  tree' <- HT.expand idxTree
  print "expanded indexed tree"
  printTree tree'
