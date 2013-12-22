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

import Data.SafeCopy(base, deriveSafeCopy)

import HSync.Common.MTimeTree
import HSync.Common.DateTime(DateTime)
import HSync.Common.Types

--------------------------------------------------------------------------------

newtype MTimeTreeState = MTimeTreeState { unMTTS :: Maybe MTimeFSTree }
                         deriving (Eq,Show,Data,Typeable)


$(deriveSafeCopy 0 'base ''MTimeTreeState)


peekMTimeTree :: Query MTimeTreeState (Maybe MTimeFSTree)
peekMTimeTree = unMTTS <$> ask

updateMTimeTree      :: SubPath -> DateTime -> Update MTimeTreeState (Maybe MTimeFSTree)
updateMTimeTree p dt = modify (MTimeTreeState . updateMTime p dt . unMTTS)
                       >> get >>= return . unMTTS

$(makeAcidic ''MTimeTreeState ['peekMTimeTree, 'updateMTimeTree])






--------------------------------------------------------------------------------
