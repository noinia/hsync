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
import HSync.Common.FSTree
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


queryMTimeTree :: Query MTimeTreeState (Maybe MTimeFSTree)
queryMTimeTree = unMTTS <$> ask

updateMTimeTree      :: SubPath -> DateTime -> Update MTimeTreeState (Maybe MTimeFSTree)
updateMTimeTree p dt = modify (MTimeTreeState . updateMTime p dt . unMTTS)
                       >> get >>= return . unMTTS

updateReplaceMTimeTree       :: SubPath -> MTimeFSTree -> Update MTimeTreeState ()
updateReplaceMTimeTree p st = modify (\s ->
                          MTimeTreeState . Just $ case (unMTTS s, p) of
    (Nothing, _:_) -> error "replaceMTimeTree: No tree yet!"
    (Nothing, [])  -> st -- full tree
    (Just t, _)    -> replaceSubTree t p st
    ) -- maybe we should Seq the execution of the case statement


replaceSubTree         :: MTimeFSTree -> SubPath -> MTimeFSTree -> MTimeFSTree
replaceSubTree t sp st = case fsTreeZipper t () >>= goTo sp
                                                >>= return . goToRoot . replace st of
                           Nothing       -> error "replaceSubTree: path does not exist."
                           Just (t',_,_) -> t'
                          -- TODO: it feels kind of weird that we replace the maybe
                          -- by an error.


$(makeAcidic ''MTimeTreeState [ 'queryMTimeTree
                              , 'updateMTimeTree
                              , 'updateReplaceMTimeTree
                              ])

--------------------------------------------------------------------------------
