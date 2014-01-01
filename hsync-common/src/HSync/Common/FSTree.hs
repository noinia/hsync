module HSync.Common.FSTree( module HSync.Common.FSTree
                          ) where


import Control.Applicative((<$>))

import Data.Maybe(fromJust)

import HSync.Common.FSTree.Basic as HSync.Common.FSTree

import HSync.Common.FSTree.Zipper( fsTreeZipperAt, tree, goToRoot
                                 , updateAndPropagate
                                 , FSTreeZipper
                                 )
import HSync.Common.Types


--------------------------------------------------------------------------------


-- | A function to propagate labels upwards
type PropFunc fl dl = Either fl dl -> dl -> dl


-- | updates a given subtree, and propagates any label updates to all ancestor labels
propagateUp              :: PropFunc fl dl  -- ^ label update F
                         -> Either fl dl    -- ^ default label
                         -> SubPath
                         -> (FSTree fl dl -> Maybe (FSTree fl dl))
                         -> FSTree fl dl
                         -> Maybe (FSTree fl dl)
propagateUp lF dL p tF t = atSubTree p t (updateAndPropagate dL lF tF)


-- | Variant of the above that just adjusts the tree, i.e. no deletes.
propagateUp'         :: PropFunc fl dl -> -- ^ label update F
                        SubPath ->
                        (FSTree fl dl -> FSTree fl dl) ->
                        FSTree fl dl ->
                        FSTree fl dl
propagateUp' lF p tF = fromJust . propagateUp lF undefined p (Just . tF)
                       -- since fTree _ = Just st, we can safely leave
                       -- the default element unspecified.

update :: PropFunc fl dl
       -> Either fl dl
       -> SubPath
       -> (FSTree fl dl -> Maybe (FSTree fl dl))
       -> FSTree fl dl
       -> Maybe (FSTree fl dl)
update = propagateUp


adjust :: PropFunc fl dl
       -> SubPath
       -> (FSTree fl dl -> FSTree fl dl)
       -> FSTree fl dl
       -> FSTree fl dl
adjust = propagateUp'

replace         :: PropFunc fl dl ->
                   SubPath ->
                   FSTree fl dl -> -- ^ New subtree
                   FSTree fl dl -> FSTree fl dl
replace lF p st = adjust lF p (const st)


delete         :: PropFunc fl dl
               -> Either fl dl   -- ^ Value to start the label propagation with
               -> SubPath
               -> FSTree fl dl
               -> Maybe (FSTree fl dl)
delete lF dL p = propagateUp lF dL p (const Nothing)





addFileAt        :: PropFunc fl dl -> SubPath -> File fl -> FSTree fl dl -> FSTree fl dl
addFileAt lF p f = adjust lF (init p) (addFile f)


addDirAt        :: PropFunc fl dl -> SubPath -> Directory fl dl ->
                   FSTree fl dl -> FSTree fl dl
addDirAt lF p d = adjust lF (init p) (addSubDir d)






--------------------------------------------------------------------------------


-- -- | Given a function and a path p, update the subtree rooted at p. If the function
-- --    returns Nothing, the subtree is deleted.
-- updateSubTree       :: (FSTree fl dl -> Maybe (FSTree fl dl))
--                        -> SubPath -> FSTree fl dl -> Maybe (FSTree fl dl)
-- updateSubTree f p t = atSubTree p t (update f)


-- -- | Replace a subtree. If the path does not exist, we return the unmodified tree.
-- replaceSubTree        :: SubPath ->      -- ^ the path at which to replace
--                          FSTree fl dl -> -- ^ the new subtree
--                          FSTree fl dl -> -- ^ the full tree
--                          FSTree fl dl
-- replaceSubTree p st t = fromJust $ atSubTree p t (return . replace st)



-- | Run a function at a subtree, and return the full tree. If the path does
-- not exist, returns the original tree.
atSubTree       :: SubPath -> FSTree fl dl ->
                   (FSTreeZipper fl dl () -> Maybe (FSTreeZipper fl dl ())) ->
                   Maybe (FSTree fl dl)
atSubTree p t f = case fsTreeZipperAt t () p of
                     Nothing -> Just t
                     Just z  -> tree . goToRoot <$> f z
