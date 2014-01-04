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

newtype MTimeTreeState = MTimeTreeState { unMTTS :: Maybe MTimeTree }
                         deriving (Eq,Show,Data,Typeable)

$(deriveSafeCopy 0 'base ''MTimeTreeState)

instance Default MTimeTreeState where
  def = MTimeTreeState Nothing

-- | Run a function on the tree we are storing
onMTimeTree   :: (MTimeTree -> Maybe MTimeTree) -> MTimeTreeState -> MTimeTreeState
onMTimeTree f = MTimeTreeState . (>>= f) . unMTTS


-- | Get the thing we are actually storing
queryMTimeTree :: Query MTimeTreeState (Maybe MTimeTree)
queryMTimeTree = unMTTS <$> ask


updateReplaceFull    :: Maybe MTimeTree -> Update MTimeTreeState ()
updateReplaceFull mt = modify (MTimeTreeState . const mt . unMTTS)

$(makeAcidic ''MTimeTreeState [ 'queryMTimeTree
                              , 'updateReplaceFull
                              ])

--------------------------------------------------------------------------------
