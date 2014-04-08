{-# Language TemplateHaskell #-}
{-# Language FlexibleInstances #-}
module HSync.Server.AcidSync where

import Prelude

import Control.Applicative((<$>))
import Control.Exception.Base(bracket)
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.IO.Class(MonadIO(..))

import Data.Default(def)
import Data.Acid(AcidState, Update, Query,
                 makeAcidic, openLocalState)
import Data.Acid.Advanced(update')
import Data.Acid.Local(createCheckpointAndClose)

import HSync.Common.Types(UserIdent, ErrorMessage)
import HSync.Common.Notification(Notification)
--import HSync.Common.TimedFSTree(File(..),Directory(..))

import HSync.Server.User(User(..),UserIndex(..))
import HSync.Server.FileSystemState
import HSync.Server.Settings(Extra(..))

-- import qualified HSync.Common.TimedFSTree as T
import qualified HSync.Server.User as U


--------------------------------------------------------------------------------

-- | A single type that collects everything that we acidize
data AcidSync = AcidSync { fsState :: AcidState FSState
                         , users   :: AcidState UserIndex
                         }

--------------------------------------------------------------------------------
-- | The acidic operations

---------------------------------------- FSState

-- | Get the thing we are actually storing
queryFSState :: Query FSState FSState
queryFSState = ask

replaceFull   :: FSState -> Update FSState ()
replaceFull t = modify (const t)

newNotification   :: Notification -> Update FSState ()
newNotification n = modify (updateNotification n)

$(makeAcidic ''FSState [ 'queryFSState
                       , 'replaceFull
                       , 'newNotification
                       ])

---------------------------------------- Users

queryUserIndex :: Query UserIndex UserIndex
queryUserIndex = ask

lookupUser    :: UserIdent -> Query UserIndex (Maybe User)
lookupUser ui = U.lookupUser ui <$> ask

insertUser   :: User -> Update UserIndex (Maybe ErrorMessage)
insertUser u = do
                 eix <- U.insertUser u <$> get
                 case eix of
                   Left err -> return $ Just err
                   Right ix -> put ix >> return Nothing


$(makeAcidic ''UserIndex [ 'queryUserIndex
                         , 'lookupUser
                         , 'insertUser
                         ])

--------------------------------------------------------------------------------

withAcidSync        :: Extra
                    -> (AcidSync -> IO a)
                    -> IO a
withAcidSync conf f = do
                        let baseDir = extraFilesDir conf
                        -- Get a blank copy of the of the FSTree. This is only used if
                        -- there is no acidious state yet.
                        blankFSS <- newFSState baseDir
                        -- Start/open Acid state
                        withState blankFSS $ \fsState' ->
                          withState def $ \users' ->
                            f $ AcidSync fsState' users'
  where
    withState init' g = bracket (openLocalState init')
                               (createCheckpointAndClose)
                               g

--------------------------------------------------------------------------------

notificationUpdate            :: MonadIO m => AcidSync -> Notification -> m ()
notificationUpdate acidSync n = let fsStateAcid = fsState acidSync in
                                update' fsStateAcid (NewNotification n)
