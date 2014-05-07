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
                            , AddByFileIdent(..)
                            , DeleteByFileIdent(..)
                            , UpdateByFileIdent(..)
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

import HSync.Common.TimedFSTree(MTimeTree)

import HSync.Common.FileIdent(FileIdent)
import HSync.Common.DateTime(DateTime)
import HSync.Common.Types

import qualified HSync.Common.TimedFSTree as FST


--------------------------------------------------------------------------------

-- | A single type that collects everything that we acidize
data AcidSync = AcidSync { remoteTreeAcid :: AcidState MTimeTreeState
                         }

--------------------------------------------------------------------------------

type MTimeTreeState = Maybe MTimeTree


-- | Get the thing we are actually storing
queryMTimeTree :: Query MTimeTreeState (Maybe MTimeTree)
queryMTimeTree = ask

replaceFull    :: Maybe MTimeTree -> Update MTimeTreeState ()
replaceFull mt = modify (const mt)

modify' f = modify $ fmap f


addByFileIdent       :: SubPath -> FileIdent -> Update MTimeTreeState ()
addByFileIdent sp fi = modify' $ FST.addByFileIdent sp fi


deleteByFileIdent          :: SubPath -> DateTime -> FileIdent -> Update MTimeTreeState ()
deleteByFileIdent sp dt fi = modify' $ FST.deleteByFileIdent sp dt fi


updateByFileIdent       :: SubPath -> FileIdent -> Update MTimeTreeState ()
updateByFileIdent sp fi = modify' $ FST.updateByFileIdent sp fi

$(makeAcidic ''MTimeTreeState [ 'queryMTimeTree
                              , 'replaceFull
                              , 'addByFileIdent
                              , 'deleteByFileIdent
                              , 'updateByFileIdent
                              ])

--------------------------------------------------------------------------------
