module HSync.Common.FSTree( module HSync.Common.FSTree
                          ) where


import HSync.Common.FSTree.Basic as HSync.Common.FSTree


import HSync.Common.Types

import HSync.Common.FSTree.Zipper( fsTreeZipperAt, tree, goToRoot
                                 , update, updateAndPropagate
                                 )


-- | Given a function and a path p, update the subtree rooted at p. If the function
--    returns Nothing, the subtree is deleted.
updateSubTree       :: (FSTree fl dl -> Maybe (FSTree fl dl))
                       -> SubPath -> FSTree fl dl -> Maybe (FSTree fl dl)
updateSubTree f p t = fsTreeZipperAt t () p >>=
                      update f >>= return . tree . goToRoot


-- | updates a given subtree, and propagates any label updates to all ancestor labels
updateAndPropagateUp              :: Either fl dl -> (Either fl dl -> dl -> dl) ->
                                     (FSTree fl dl -> Maybe (FSTree fl dl)) ->
                                     SubPath ->
                                     FSTree fl dl -> Maybe (FSTree fl dl)
updateAndPropagateUp dL lF tF p t =     fsTreeZipperAt t () p
                                    >>= updateAndPropagate dL lF tF
                                    >>= return . tree . goToRoot
