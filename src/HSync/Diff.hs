{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
module HSync.Diff
  ( Diff(..)
  , HasDiff(..)

  , Changed(..), from, to

  , EitherChanged(..)

  , Updated(..)
  , Updates(Updates), added, deleted, updated


  , DirAttrs(..)
  , DirAttrsDiff(..)

    -- maybe I should move the ones below to a specific modules

  , FileTreeChanged(..)
  , DirDelta(..)
  , Cache(..)
  ) where

import           HSync.FileTree

import           Control.Lens hiding (from,to)
import qualified Data.Map as Map
import           Data.These
import           Data.Time.Clock
import           Flat hiding (from,to)

--------------------------------------------------------------------------------


-- | Data type modelling differences.
data Diff delta = NoDifference
                | Difference delta
            deriving stock (Show,Eq,Functor,Foldable,Traversable,Generic)

-- deriving stock instance (Show (Delta t)) => Show (Diff t)
-- deriving stock instance (Eq (Delta t))   => Eq (Diff t)


-- | A data type capturing that 'a' has changed from and old version
-- to a new version.
data Changed a = Changed {_from :: !a , _to :: !a }
  deriving stock (Show,Eq,Generic)

instance Flat a => Flat (Changed a)

from :: Lens' (Changed a) a
from = lens _from (\(Changed _ t) f -> Changed f t)

to :: Lens' (Changed a) a
to = lens _to   (\(Changed f _) t -> Changed f t)

-- | default implementation of diff
defaultDiff                        :: (Eq a, Delta a ~ Changed a)
                                   => a -> a -> Diff (Delta a)
defaultDiff old new  | old == new  = NoDifference
                     | otherwise   = Difference $ Changed old new

class HasDiff t where
  type Delta t
  type Delta t = Changed t

  diff :: t -> t -> Diff (Delta t)
  default diff :: (Eq t, Delta t ~ Changed t) => t -> t -> Diff (Delta t)
  diff = defaultDiff

instance HasDiff () where
  type Delta () = ()
  diff _ _ = NoDifference

instance HasDiff Int where
  type Delta Int = Changed Int

instance HasDiff UTCTime where
  type Delta UTCTime = Changed UTCTime

instance HasDiff FileName


-- data PairChanged a b = FirstChanged (Diff a)
--                      | SecondChanged (Diff b)
--                      | BothChanged (Diff a) (Diff b)

instance (HasDiff a, HasDiff b) => HasDiff (a,b) where
  type Delta (a,b) = These (Delta a) (Delta b)
  diff (oldA,oldB) (newA,newB) = case (diff oldA newA, diff oldB newB) of
    (NoDifference, NoDifference)           -> NoDifference
    (NoDifference, Difference deltaB)      -> Difference (That $ deltaB)
    (Difference deltaA, NoDifference)      -> Difference (This $ deltaA)
    (Difference deltaA, Difference deltaB) -> Difference (These deltaA deltaB)



data EitherChanged a b = LeftChanged (Delta a)
                       | RightChanged (Delta b)
                       | LeftToRight a b
                       | RightToLeft b a

deriving stock instance (Show a, Show b, Show (Delta a), Show (Delta b)) => Show (EitherChanged a b)
deriving stock instance (Eq a, Eq b, Eq  (Delta a), Eq (Delta b))    => Eq (EitherChanged a b)

instance (HasDiff a, HasDiff b) => HasDiff (Either a b) where
  type Delta (Either a b) = EitherChanged a b

  diff (Left l) (Left r)   = LeftChanged <$> diff l r
  diff (Left l) (Right r)  = Difference $ LeftToRight l r
  diff (Right l) (Left r)  = Difference $ RightToLeft l r
  diff (Right l) (Right r) = RightChanged <$> diff l r




data Updated a = Added !a
               | Deleted !a
               | Updated !(Delta a)

deriving stock instance (Show a, Show (Delta a))       => Show (Updated a)
deriving stock instance (Eq a, Eq (Delta a))           => Eq (Updated a)
deriving stock instance (Generic a, Generic (Delta a)) => Generic (Updated a)

instance (Generic a, Generic (Delta a), Flat a, Flat (Delta a)) => Flat (Updated a)

data Updates delta a = Updates { _added   :: [a]
                               , _deleted :: [a]
                               , _updated :: [delta]
                               }
                       deriving (Show,Eq,Generic)

instance (Flat delta, Flat a) => Flat (Updates delta a)

added   :: Lens' (Updates delta a) [a]
added   = lens _added   (\(Updates _ ds us) as -> Updates as ds us)

deleted   :: Lens' (Updates delta a) [a]
deleted = lens _deleted (\(Updates as _ us) ds -> Updates as ds us)

updated   :: Lens' (Updates delta a) [delta]
updated = lens _updated (\(Updates as ds _) us -> Updates as ds us)


instance Semigroup (Updates delta a) where
  (Updates as ds us) <> (Updates as' ds' us') = Updates (as <> as') (ds <> ds') (us <> us')

instance Monoid (Updates delta a) where
  mempty = Updates mempty mempty mempty


-- | Comptues the diff between two ordered lists
diffOrderedLists                               :: (Ord k, HasDiff v) => [(k,v)] -> [(k,v)]
                                               -> Updates (k,Delta v) (k,v)
diffOrderedLists []     []                     = mempty
diffOrderedLists ls     []                     = Updates mempty ls mempty
diffOrderedLists []     rs                     = Updates rs mempty mempty
diffOrderedLists (l@(lk,lv):ls) (r@(rk,rv):rs) = case lk `compare` rk of
    LT -> let us = diffOrderedLists ls (r:rs)
          in us&deleted %~ (l:)
    EQ -> let us = diffOrderedLists ls rs
          in case diff lv rv of
               NoDifference     -> us
               Difference delta -> us&updated %~ ((lk,delta):)
    GT -> let us = diffOrderedLists (l:ls) rs
          in us&added %~ (r:)


instance (Ord k, HasDiff v) => HasDiff (Map.Map k v) where
  type Delta (Map.Map k v) = Updates (k,Delta v) (k,v)

  diff old new = case diffOrderedLists (Map.toAscList old) (Map.toAscList new) of
                   Updates [] [] [] -> NoDifference
                   us               -> Difference us

--------------------------------------------------------------------------------

-- | directory attributes that supports local attributes as well as a cache.
data DirAttrs cache d = DirAttrs !cache -- ^ cached attributes about the content
                                 !d  -- ^ "local" attributes about this directory itself
                      deriving stock (Show,Eq,Ord,Generic)

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

-- deriving stock instance (Show f, Show d, Show (Delta d), Show (Delta (FileTree d f))
--                         ) => Show (DirDelta d f)
-- -- -- TODO: this sounds dangerous...

-- deriving stock instance (Eq (Delta d), Eq f, Eq d, Eq (Delta (FileTree d f))
--                         ) => Eq (DirDelta d f)


testDelta :: DirDelta Int Int
testDelta = OnlyLocal (Changed 6 8)


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

instance HasDiff d => HasDiff (Identity d) where
  type Delta (Identity d) = Delta d
  diff (Identity l) (Identity r) = diff l r


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



newtype Cache d = Cache d deriving stock (Show,Eq,Ord,Generic)

instance Flat d => Flat (Cache d)


instance HasDiff d => HasDiff (Cache d) where
  type Delta (Cache d) = Delta d
  diff (Cache l) (Cache r) = diff l r
