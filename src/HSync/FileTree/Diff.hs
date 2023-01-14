{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module HSync.FileTree.Diff
  ( FileTreeChanged(..)
  , DirDelta(..)
  , DirAttrsDiff(..)
  ) where

import           Data.These
import           Flat hiding (from,to)
import           HSync.Diff
import           HSync.FileTree

--------------------------------------------------------------------------------

data DirAttrsDiff cache d = CacheOutdated     !(Delta cache)
                          | LocalAttrsChanged !(Delta d)
                          | BothChanged       !(Delta cache) !(Delta d)

deriving instance (Show cache, Show d, Show (Delta cache), Show (Delta d)
                  ) => Show (DirAttrsDiff cache d)
deriving instance (Eq cache, Eq d, Eq (Delta cache), Eq (Delta d)
                  ) => Eq (DirAttrsDiff cache d)

instance (HasDiff cache, HasDiff d) => HasDiff (DirAttrs cache d) where
  type Delta (DirAttrs cache d) = DirAttrsDiff cache d
  diff (DirAttrs oldCache oldD) (DirAttrs newCache newD) =
    these CacheOutdated LocalAttrsChanged BothChanged <$> diff (oldCache,oldD) (newCache,newD)

data FileTreeChanged dirDelta d f =
    FileChanged (Delta f)
  | DirectoryChanged dirDelta
    -- ^ The directory has changed, dirDelta represents the possible changes.
  | FileBecameDirectory f (d, DirectoryContent d f)
  | DirectoryBecameFile (d, DirectoryContent d f) f

deriving stock instance (Show d, Show f, Show dirDelta, Show (Delta f)
                        ) => Show (FileTreeChanged dirDelta d f)
deriving stock instance (Eq d, Eq f, Eq dirDelta, Eq (Delta f)
                        ) => Eq (FileTreeChanged dirDelta d f)
deriving stock instance (Generic d, Generic f, Generic dirDelta, Generic (Delta f)
                        ) => Generic (FileTreeChanged dirDelta d f)

data DirDelta d f = OnlyLocal      (Delta d)
                  | ContentChanged (Delta d) (Delta (DirectoryContent d f))

deriving stock instance ( Show f, Show d, Show cache
                        , Show (Delta f), Show (Delta d), Show (Delta cache)
                        ) => Show (DirDelta (DirAttrs cache d) f)
deriving stock instance ( Eq f, Eq d, Eq cache
                        , Eq (Delta f), Eq (Delta d), Eq (Delta cache)
                        ) => Eq (DirDelta (DirAttrs cache d) f)

instance (HasDiff f, HasDiff d, HasDiff cache
         ) => HasDiff (FileTree (DirAttrs cache d) f) where
  -- ^ note that this instance requires that the cache is somehow kept up to date, i.e.
  -- we will not attempt to test the children unless the local cache is outdated as well.

  type Delta (FileTree (DirAttrs cache d) f) =
    FileTreeChanged (DirDelta (DirAttrs cache d) f) (DirAttrs cache d) f

  -- diff :: FileTree cache f -> FileTree cache f -> Delta (FileTree cache f)
  diff (File l)            (File r)             = FileChanged <$> diff l r
  diff (File l)            (Directory d cntsR)  = Difference $ FileBecameDirectory l (d,cntsR)
  diff (Directory d cntsL) (File r)             = Difference $ DirectoryBecameFile (d,cntsL) r
  diff (Directory l cntsL) (Directory r cntsR) = case diff l r of
      NoDifference          -> NoDifference
        -- if there is no difference between the dirattrs, that in particular means
        -- the cached subtree values are the same. Therefore, there is no need to explicitly
        -- test the content
      Difference deltaAttrs -> Difference . DirectoryChanged $ case deltaAttrs of
        LocalAttrsChanged _               -> OnlyLocal deltaAttrs
          -- Same here, only the local attributes changed, but the cache is still the same.
          -- therefore, there is still no need to explicitly test the content.
        CacheOutdated _                   -> testContent
        BothChanged _ _                   -> testContent
        where
          -- explicitly test changes with the children/content
          testContent = case diff cntsL cntsR of
            NoDifference        -> OnlyLocal deltaAttrs -- we really only have local changes
            Difference deltaCnt -> ContentChanged deltaAttrs deltaCnt -- children also have changes





--------------------------------------------------------------------------------

-- data DirWithAttrChange d = DirAttrChange (Delta d)



-- data DirWithAttrChange =


-- instance (HasDiff f, HasDiff d
--          ) => HasDiff (FileTree d f (DirWithAttrChange)

--                                            ) where
--   type Delta (FileTree (Identity d) f) = FileTreeChanged (Identity d)
--                                                          f
--                                          (These (Diff d)
--                                                          (Diff (DirectoryContent (Identity d) f))
--                                                   )

--   diff (File l)            (File r)             = FileChanged <$> diff l r
--   diff (File l)            (Directory d cntsR)  = Difference $ FileBecameDirectory l (d,cntsR)
--   diff (Directory d cntsL) (File r)             = Difference $ DirectoryBecameFile (d,cntsL) r
--   diff (Directory d cntsL) (Directory d' cntsR) = case (diff d d', diff cntsL cntsR) of
--     (NoDifference, NoDifference)             -> NoDifference
--     (Difference deltaD, NoDifference)        -> Difference . DirectoryChanged $ This deltaD
--     (NoDifference, Difference deltaCnt)      -> Difference . DirectoryChanged $ That deltaCnt
--     (Difference deltaD, Difference deltaCnt) -> Difference . DirectoryChanged $ These deltaD deltaCnt
