module HSync.Common.FSTree( module HSync.Common.FSTree
                          ) where


import Control.Applicative((<$>))

import Data.Maybe(fromJust)

import HSync.Common.FSTree.Basic as HSync.Common.FSTree

import HSync.Common.FSTree.Zipper( fsTreeZipperAt, fsTreeZipper , tree, goToRoot
                                 , updateAndPropagate , goToDirOrFile
                                 , FSTreeZipper , insertSubTree
                                 )
import HSync.Common.Types


--------------------------------------------------------------------------------

-- | A function to propagate labels upwards
type PropFunc fl dl = Either fl dl -> dl -> dl






--------------------------------------------------------------------------------
-- | Operations on an element of the tree that also allow updating information
--    in the ancestors of the given element.


-- | Insert/Adjust/Delete the element indicated by the subpath. See propagateUp
-- for more info.
update :: PropFunc fl dl
       -> Either fl dl
       -> SubPath
       -> (FSTree fl dl -> Maybe (FSTree fl dl))
       -> FSTree fl dl
       -> Maybe (FSTree fl dl)
update = propagateUp



-- | Variant of update that only allows adjusting already existing
-- elements. I.e. no deletes or inserts.
adjust :: PropFunc fl dl
       -> SubPath
       -> (FSTree fl dl -> FSTree fl dl)
       -> FSTree fl dl
       -> FSTree fl dl
adjust = propagateUp'

-- | Replace an existing tree by a new one.
replace         :: PropFunc fl dl
                -> SubPath
                -> FSTree fl dl -- ^ New subtree
                -> FSTree fl dl -> FSTree fl dl
replace lF p st = adjust lF p (const st)


-- | Delete a subtree.
delete         :: PropFunc fl dl
               -> Either fl dl   -- ^ Value to start the label propagation with
               -> SubPath
               -> FSTree fl dl
               -> Maybe (FSTree fl dl)
delete lF dL p = propagateUp lF dL p (const Nothing)


-- TOD: These ops should be implemented using update, not adjust
-- -- | Insert a new file
-- addFileAt        :: PropFunc fl dl -> SubPath -> File fl -> FSTree fl dl -> FSTree fl dl
-- addFileAt lF p f = adjust lF (init p) (addFile f)


-- -- | Insert a new directory.
-- addDirAt        :: PropFunc fl dl -> SubPath -> Directory fl dl ->
--                    FSTree fl dl -> FSTree fl dl
-- addDirAt lF p d = adjust lF (init p) (addSubDir d)



-- | Insert or adjust the label of the element at the given subpath. If the
-- element does not exist yet (but its parent does), we add the appropriate
-- label.
insertOrAdjustLabel              :: PropFunc fl dl
                                 -> SubPath
                                 -> Either (fl -> fl) (dl -> dl)
                                 -> FSTree fl dl
                                 -> FSTree fl dl
insertOrAdjustLabel pF sp labelF = fromJust . update pF eL sp treeF
  where
    -- The functions to use to update the label. In case labelF is a fileLabel
    -- we wont use dirLabelF, thus we simply leave it undefined. The case that
    -- labelF operates on dir labels is symmetric.
    fileLabelF = either id (const undefined) labelF
    dirLabelF  = either (const undefined) id labelF
    -- The tree function is simply updateLabel with the above update functions:
    treeF      = Just . updateLabel fileLabelF dirLabelF
    -- The default element is used to determine if we should create a file or a
    -- directory in case the node does not exist yet. If this is the case the
    -- actual function to update the label (i.e. the function in labelF) should
    -- be an insert, and thus be of the form: const l, for some l. So we can safely
    -- use undefined again.
    eL         = either (Left . ($ undefined)) (Right . ($ undefined)) labelF

--------------------------------------------------------------------------------
-- | The propagateUp function that does all the actual work.


-- | updates a given subtree, and propagates any label updates to all ancestor
-- labels. This function allows expressing inserts, updates, and deletes.
-- The semantics are
--
-- if p exists:
--     update the node at that path, and propagate the changes accordingly.
-- if the parent of p exists, (but p itself not), and f creates/inserts a tree:
--     create the new tree using tF, insert it into the tree in the propper place,
--     and *then* run the path propagation (starting at the newly inserted node).
-- otherwise[1]:
--     return an error
--
-- Note that if the operation is a delete. The label propagation function starts
-- at the parent node of the (just deleted) node. The default label is used to
-- initialize the propgatation.
--
-- [1]: Basically this means p points to something that does not exist, and
--      tF is not an insert either.
propagateUp              :: PropFunc fl dl  -- ^ label update F
                         -> Either fl dl    -- ^ default label
                         -> SubPath
                         -> (FSTree fl dl -> Maybe (FSTree fl dl)) -- ^ The
                                                                   -- tree
                                                                   -- update
                                                                   -- function
                         -> FSTree fl dl
                         -> Maybe (FSTree fl dl)
propagateUp lF dL p tF t = tree . goToRoot <$> atSubTree' zipperF
  where
    --             :: Function to manipulate the zipper with
    zipperF       = updateAndPropagate dL lF tF
    atSubTree' zF = case andParent p t of
                      (Nothing, _,_)        -> error "propagateUp: Path does not exist!"
                                               -- even the parent does not exist
                      (_,       Just z, _)  -> zF z
                                            -- The path exists, even the node there does
                                            -- so simply run the zipper function.
                      (Just pz, Nothing, n) -> let temp = new n in
                          case tF temp of
                            Nothing -> error "propagateUp: not an insert."
                            Just t' -> addAndSelect t' pz >>= zF
                            -- The node that we are interested in does not
                            -- exist yet. so try to create it first. If this
                            -- works we select it and run the zipper function
                            -- if not (so the path really does not exist),
                            -- throw an error.

    -- | Insert subtree t' into zipper z  and select it.
    addAndSelect t' z = let n = rootFileName t' in
                        goToDirOrFile n $ insertSubTree t' z
    -- Create a new file or directory, depending on the type of defaultL
    new n = case dL of
              Left _  -> F $ File n undefined
              Right _ -> D $ Directory n undefined [] []


-- | Variant of the above that just adjusts the tree, i.e. no inserts or deletes.
propagateUp'         :: PropFunc fl dl -- ^ label update F
                     -> SubPath
                     -> (FSTree fl dl -> FSTree fl dl)
                     -> FSTree fl dl
                     -> FSTree fl dl
propagateUp' lF p tF = fromJust . propagateUp lF undefined p (Just . tF)
                       -- since fTree _ = Just st, we can safely leave
                       -- the default element unspecified.

--------------------------------------------------------------------------------
-- | Helper functions


type Operation fl dl = FSTreeZipper fl dl () -> Maybe (FSTreeZipper fl dl ())


andLast        :: [a] -> ([a],a)
andLast []     = error "andLast: empty list."
andLast [y]    = ([],y)
andLast (x:xs) = let (xs',y) = andLast xs in (x:xs',y)


-- | Given a subpath and a tree, produces two zippers (zp,zc) zp centered at
-- the parent of the node indicated by subPath, and zc, the zipper at that node
-- itself. This also works if zc does not exist (yet).
andParent     :: SubPath -> FSTree fl dl -> ((Maybe (FSTreeZipper fl dl ()))
                                            ,(Maybe (FSTreeZipper fl dl ()))
                                            , FileName
                                            )
andParent p t = let (p',n) = andLast p
                    pz     = fsTreeZipperAt t () p'
                in (pz, pz >>= goToDirOrFile n, n)


-- | Run a function at a subtree, and return the full tree. If the path does
-- not exist, returns the original tree.
atSubTree       :: SubPath -> FSTree fl dl
                -> Operation fl dl
                -> Maybe (FSTree fl dl)
atSubTree p t f = case fsTreeZipperAt t () p of
                     Nothing -> Just t
                     Just z  -> tree . goToRoot <$> f z



-- -- | updates a given subtree, and propagates any label updates to all ancestor labels
-- propagateUp              :: PropFunc fl dl  -- ^ label update F
--                          -> Either fl dl    -- ^ default label
--                          -> SubPath
--                          -> (FSTree fl dl -> Maybe (FSTree fl dl))
--                          -> FSTree fl dl
--                          -> Maybe (FSTree fl dl)
-- propagateUp lF dL p tF t = atSubTree' p t (updateAndPropagate dL lF tF)
