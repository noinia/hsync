{-# Language TemplateHaskell #-}
{-# Language TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DeriveDataTypeable #-}
module HSync.Client.AcidSync( AcidSync(..)
                            , MTimeTreeState

                            , QueryMTimeTree(..)
                            , ReplaceFull(..)
                            , UpdateFileIdent(..)
                            , SetFileIdentOf(..)
                            ) where

import Control.Applicative

import Control.Monad.Reader.Class
import Control.Monad.State.Class(get, modify)


import Data.Acid(AcidState, Update, Query,
                 makeAcidic)
import Data.Acid.Advanced(query',update')
import Data.Data(Data, Typeable)
import Data.Default
import Data.SafeCopy(base, deriveSafeCopy)

import HSync.Common.MTimeTree(MTimeTree)

import HSync.Common.FileIdent(FileIdent)
import HSync.Common.DateTime(DateTime)
import HSync.Common.Types

import qualified HSync.Common.MTimeTree as MT


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

replaceFull    :: Maybe MTimeTree -> Update MTimeTreeState ()
replaceFull mt = modify (const $ MTimeTreeState mt)

-- | Acidized version of MTimeTree.updateFileIdent.
updateFileIdent          :: DateTime -> SubPath -> FileIdent -> Update MTimeTreeState ()
updateFileIdent dt sp fi = modify (onMTimeTree $ MT.updateFileIdent dt sp fi)

-- | Specialized version of updateFileIdent that does not allow deletes
setFileIdentOf :: SubPath -> FileIdent -> Update MTimeTreeState ()
setFileIdentOf = updateFileIdent undefined
                  -- Note: We specifically acidize this one to prevent having to
                  -- serialized the 'undefined' value we pass to updateFileIdent.

$(makeAcidic ''MTimeTreeState [ 'queryMTimeTree
                              , 'replaceFull
                              , 'updateFileIdent
                              , 'setFileIdentOf
                              ])

--------------------------------------------------------------------------------
