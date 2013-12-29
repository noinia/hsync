module HSync.Common.FSTree( module HSync.Common.FSTree
                          ) where


import Control.Applicative((<$>))

import Data.Maybe(fromJust)

import HSync.Common.FSTree.Basic as HSync.Common.FSTree

import HSync.Common.FSTree.Zipper( fsTreeZipperAt, tree, goToRoot
                                 , update, updateAndPropagate, replace
                                 , FSTreeZipper
                                 )
import HSync.Common.Types


-- | Given a function and a path p, update the subtree rooted at p. If the function
--    returns Nothing, the subtree is deleted.
updateSubTree       :: (FSTree fl dl -> Maybe (FSTree fl dl))
                       -> SubPath -> FSTree fl dl -> Maybe (FSTree fl dl)
updateSubTree f p t = atSubTree p t (update f)


-- | updates a given subtree, and propagates any label updates to all ancestor labels
updateAndPropagateUp              :: Either fl dl -> -- ^ default label
                                     (Either fl dl -> dl -> dl) -> -- ^ label update F
                                     (FSTree fl dl -> Maybe (FSTree fl dl)) ->
                                     SubPath ->
                                     FSTree fl dl ->
                                     Maybe (FSTree fl dl)
updateAndPropagateUp dL lF tF p t = atSubTree p t (updateAndPropagate dL lF tF)


-- | Replace a subtree and propagates label updates to all ancestor labels
replaceAndPropagateUp         :: (Either fl dl -> dl -> dl) -> -- ^ label function
                                 FSTree fl dl ->               -- ^ the new subtree
                                 SubPath      ->               -- ^ path where to replace
                                 FSTree fl dl ->               -- ^ the full tree.
                                 FSTree fl dl
replaceAndPropagateUp lF st p = fromJust .
                              updateAndPropagateUp undefined lF (const $ Just st) p
                              -- since fTree _ = Just st, we can safely leave
                              -- the default element unspecified.


-- | Replace a subtree. If the path does not exist, we return the unmodified tree.
replaceSubTree        :: SubPath ->      -- ^ the path at which to replace
                         FSTree fl dl -> -- ^ the new subtree
                         FSTree fl dl -> -- ^ the full tree
                         FSTree fl dl
replaceSubTree p st t = fromJust $ atSubTree p t (return . replace st)



-- | Run a function at a subtree, and return the full tree. If the path does
-- not exist, returns the original tree.
atSubTree       :: SubPath -> FSTree fl dl ->
                   (FSTreeZipper fl dl () -> Maybe (FSTreeZipper fl dl ())) ->
                   Maybe (FSTree fl dl)
atSubTree p t f = case fsTreeZipperAt t () p of
                     Nothing -> Just t
                     Just z  -> tree . goToRoot <$> f z
