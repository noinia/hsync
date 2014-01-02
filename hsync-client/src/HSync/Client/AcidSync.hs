{-# Language TemplateHaskell #-}
{-# Language TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DeriveDataTypeable #-}
module HSync.Client.AcidSync where

import Control.Applicative

import Control.Monad.Reader.Class
import Control.Monad.State.Class(get, modify)


import Data.Acid(AcidState, Update, Query,
                 makeAcidic)
import Data.Acid.Advanced(query',update')
import Data.Data(Data, Typeable)
import Data.Default
import Data.SafeCopy(base, deriveSafeCopy)

import HSync.Common.MTimeTree

import HSync.Common.DateTime(DateTime)
import HSync.Common.Types

--------------------------------------------------------------------------------

-- | A single type that collects everything that we acidize
data AcidSync = AcidSync { remoteTreeAcid :: AcidState MTimeTreeState
                         }

--------------------------------------------------------------------------------

newtype MTimeTreeState = MTimeTreeState { unMTTS :: Maybe MTimeFSTree }
                         deriving (Eq,Show,Data,Typeable)

$(deriveSafeCopy 0 'base ''MTimeTreeState)

instance Default MTimeTreeState where
  def = MTimeTreeState Nothing

-- | Run a function on the tree we are storing
onMTimeTree   :: (MTimeFSTree -> Maybe MTimeFSTree) -> MTimeTreeState -> MTimeTreeState
onMTimeTree f = MTimeTreeState . (>>= f) . unMTTS


-- | Get the thing we are actually storing
queryMTimeTree :: Query MTimeTreeState (Maybe MTimeFSTree)
queryMTimeTree = unMTTS <$> ask


updateReplaceFull    :: Maybe MTimeFSTree -> Update MTimeTreeState ()
updateReplaceFull mt = modify (onMTimeTree $ const mt)


-- -- | Given a subpath and a tree. Replace that part of the subtree. with the new tree,
-- -- and propagate any updates to its ancestors.
-- updateReplaceMTimeTree       :: SubPath -> MTimeFSTree -> Update MTimeTreeState ()
-- updateReplaceMTimeTree p st = modify (onMTimeTree $


--                               )

--                               (\s ->
--                           MTimeTreeState . Just $ case (unMTTS s, p) of
--     (Nothing, _:_) -> error "replaceMTimeTree: No tree yet!"
--     (Nothing, [])  -> st -- full tree
--     (Just t, _)    -> replaceSubTree p st t
--     ) -- maybe we should Seq the execution of the case statement






$(makeAcidic ''MTimeTreeState [ 'queryMTimeTree
                              , 'updateReplaceFull
                              ])

--------------------------------------------------------------------------------
