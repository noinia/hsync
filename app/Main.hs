{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
module Main where

import qualified MyLib (someFunc)

import           Control.Lens
import qualified Data.Map as Map
import           Data.These
import           System.OsPath

--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc


type FileName = OsString


data FileTreeRoot d a = Root { _rootPath :: !OsPath
                             , _tree     :: !(FileTree d a)
                             }
                      deriving (Show,Eq)

-- | the parent already stores the name of this file
data FileTree d f = File      !f
                  | Directory !d (DirectoryContent d f)
                  deriving (Show,Eq)

type DirectoryContent d f = Map.Map FileName (FileTree d f)

--------------------------------------------------------------------------------

data Diff delta = NoDifference
                | Difference delta
            deriving stock (Show,Eq,Functor,Foldable,Traversable)

-- deriving stock instance (Show (Delta t)) => Show (Diff t)
-- deriving stock instance (Eq (Delta t))   => Eq (Diff t)


data Changed a = Changed {_from :: !a , _to :: !a }
  deriving (Show,Eq)


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


instance HasDiff FileName -- use the default


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

deriving stock instance (Show a, Show (Delta a)) => Show (Updated a)
deriving stock instance (Eq a, Eq (Delta a))     => Eq (Updated a)

data Updates delta a = Updates { _added   :: [a]
                               , _deleted :: [a]
                               , _updated :: [delta]
                               }
                       deriving (Show,Eq)

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

data FileTreeChanged d f = FileChanged (Delta f)
                         | DirectoryAttributesChanged (Delta d)
                         | DirectoryContentChanged (Delta (DirectoryContent d f))
                         | DirectoryChanged (Delta d) (Delta (DirectoryContent d f))
                         -- ^ both the local attributes changed, as well as the content
                         | FileBecameDirectory f (d, DirectoryContent d f)
                         | DirectoryBecameFile (d, DirectoryContent d f) f

deriving stock instance (Show d, Show f, Show (Delta d), Show (Delta f)) => Show (FileTreeChanged d f)
deriving stock instance (Eq d, Eq f, Eq (Delta d), Eq (Delta f)) => Eq (FileTreeChanged d f)


instance (HasDiff f, HasDiff d) => HasDiff (FileTree d f) where
  type Delta (FileTree d f) = FileTreeChanged d f

  diff (File l)            (File r)             = FileChanged <$> diff l r
  diff (File l)            (Directory d cntsR)  = Difference $ FileBecameDirectory l (d,cntsR)
  diff (Directory d cntsL) (File r)             = Difference $ DirectoryBecameFile (d,cntsL) r
  diff (Directory d cntsL) (Directory d' cntsR) = case (diff d d', diff cntsL cntsR) of
    (NoDifference, NoDifference)             -> NoDifference
    (Difference deltaD, NoDifference)        -> Difference $ DirectoryAttributesChanged deltaD
    (NoDifference, Difference deltaCnt)      -> Difference $ DirectoryContentChanged deltaCnt
    (Difference deltaD, Difference deltaCnt) -> Difference $ DirectoryChanged deltaD deltaCnt


data DirDelta cache = OutdatedCache (Delta cache)
                    | ContentChanged (Delta cache) (Diff (DirectoryContent cache f))


-- data DirDelta d f = OutdatedAttribute (Delta d)
--                   | ContentChange (Delta (DirectoryContent d f))
--                   | BothChanged (Delta )






data FileTreeWithCacheChanged cache f dirDelta =
    FileChanged (Delta f)
  -- | DirectoryAttributesChanged (Delta d)
  -- | DirectoryContentChanged (Delta (DirectoryContent d f))
  | DirectoryChanged dirDelta
    -- ^ both the local attributes changed, as well as the content
  | FileBecameDirectory f (d, DirectoryContent d f)
  | DirectoryBecameFile (d, DirectoryContent d f) f




diffCached :: FileTree cache f -> FileTree cache f -> Delta (FileTree cache f)
diffCached (File l)            (File r)             = FileChanged <$> diff l r
diffCached (File l)            (Directory d cntsR)  = Difference $ FileBecameDirectory l (d,cntsR)
diffCached (Directory d cntsL) (File r)             = Difference $ DirectoryBecameFile (d,cntsL) r
diffCached (Directory cacheL cntsL) (Directory cacheR cntsR) = case diff cacheL cacheR of
  NoDifference          -> NoDifference
  Difference deltaCache -> case diff cntsL cntsR of
      NoDifference      -> Difference $ DirectoryAttributesChanged deltaCache
                           -- the cache is outdated
      Difference deltaCnt -> Difference $ DirectoryChanged deltaCache deltaCnt
-- note: in particular that we are never producing a "directoryContentChanged" event in this case.
