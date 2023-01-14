{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
module HSync.Diff
  ( Diff(..)
  , HasDiff(..)

  , Changed(..), from, to

  , EitherChanged(..)

  , Updated(..)
  , Updates(Updates), added, deleted, updated

    -- maybe I should move the ones below to a specific modules
  , Cache(..)
  ) where

import           Control.Lens hiding (from,to)
import qualified Data.Map as Map
import           Data.These
import           Data.Time.Clock
import           Flat hiding (from,to)
import           HSync.FileTree

--------------------------------------------------------------------------------


-- | Data type modelling differences.
data Diff delta = NoDifference
                | Difference delta
            deriving stock (Show,Eq,Functor,Foldable,Traversable,Generic)

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


--------------------------------------------------------------------------------

instance HasDiff d => HasDiff (Identity d) where
  type Delta (Identity d) = Delta d
  diff (Identity l) (Identity r) = diff l r




newtype Cache d = Cache d deriving stock (Show,Eq,Ord,Generic)

instance Flat d => Flat (Cache d)


instance HasDiff d => HasDiff (Cache d) where
  type Delta (Cache d) = Delta d
  diff (Cache l) (Cache r) = diff l r
