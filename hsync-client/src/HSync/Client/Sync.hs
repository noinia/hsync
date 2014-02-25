{-# Language  OverloadedStrings
            , TypeFamilies
  #-}
module HSync.Client.Sync where

import Control.Monad(mzero)

import Data.Default
import Data.Maybe(fromMaybe)
import Data.List(intercalate)
import Data.Set(Set)
import Data.Yaml

import HSync.Client.Import

import HSync.Common.DateTime(DateTime)

import Network.HTTP.Conduit(Manager)

import System.FilePath.GlobPattern(GlobPattern)

import qualified Data.Set  as S
import qualified Data.Text as T
import qualified Data.List


--------------------------------------------------------------------------------

-- | Extention to use for partial files
partialFileExtension :: String
partialFileExtension = ".hsyncpart"


type ServerAddress = Text

type IgnoredPatterns = [GlobPattern]


-- | Files that should be temporarily ignored (in order to avoid reuploading
--   incoming files)
type TemporaryIgnoreFiles = Set FilePath


-- | Each Synchronized Tree has its own manager/settings etc
data Sync = Sync { httpManager     :: Manager
                 , syncConfig      :: SyncConfig
                 , ignore          :: IgnoredPatterns
                 }

data SyncConfig = SyncConfig { localBaseDir'    :: FilePath -- without trailing /
                             , serverAddress'   :: ServerAddress
                             , user'            :: UserIdent
                             , hashedPassword'  :: HashedPassword
                             , remoteBaseDir'   :: SubPath
                             , clientIdent'     :: ClientIdent
                             , ignorePath'      :: FilePath
                             , statePath'       :: Maybe FilePath
                             }
                  deriving (Show,Eq)


-- Shortcuts for some of the things we need often from a sync

localBaseDir   = localBaseDir' . syncConfig
serverAddress  = serverAddress' . syncConfig
user           = user' . syncConfig
hashedPassword = hashedPassword' . syncConfig
remoteBaseDir  = remoteBaseDir' . syncConfig
clientIdent    = clientIdent' . syncConfig


instance Default SyncConfig where
    def = SyncConfig { localBaseDir'    = "/Users/frank/tmp/synced/"
                     , serverAddress'   = "http://localhost:3000"
                     , user'            = "nobody"
                     , hashedPassword'  = "hashed-secret"
                     , remoteBaseDir'   = []
                     , clientIdent'     = "client-ident"
                     , ignorePath'      = "config/ignore"
                     , statePath'       = Nothing
                     }


statePath s = let p = "state/" ++ (T.unpack $ clientIdent s)
              in fromMaybe p . statePath' . syncConfig $ s

toLocalPath               :: Sync -> Path -> FilePath
toLocalPath s (Path _ ps) = toLocalPath' s ps

toLocalPath'   :: Sync -> SubPath -> FilePath
toLocalPath' s = intercalate "/" . (localBaseDir s :) . map T.unpack

-- |TODO Switch to the FilePath package
-- | given a local file path, create a (remote) Path corresponding to it
--
-- Precondition: localBaseDir cli is a basedir of the given file path.
-- this is not checked.
toRemotePath        :: Sync -> FilePath -> Path
toRemotePath sync fp = let lbd = localBaseDir sync
                           rbd = remoteBaseDir sync
                           Just lp = Data.List.stripPrefix lbd fp
                           fp' = T.pack . Data.List.dropWhile (=='/') $ lp -- drop init /
                           p   = rbd <> T.split (== '/') fp'
                       in toRemotePath' sync p

toRemotePath'   :: Sync -> SubPath -> Path
toRemotePath' s = Path (user s)





instance FromJSON SyncConfig where
  parseJSON (Object v) = fromConfig <$> v .:  "localBaseDir"
                                    <*> v .:  "server"
                                    <*> v .:  "username"
                                    <*> v .:  "hashedPassword"
                                    <*> v .:  "remoteBaseDir"
                                    <*> v .:  "clientIdent"
                                    <*> v .:  "ignore"
                                    <*> v .:? "statePath"
    where
      fromConfig lbd s u hp rbd ci iPath mStatePath =
        def { localBaseDir'   = lbd
            , serverAddress'  = s
            , user'           = u
            , hashedPassword' = hp
            , remoteBaseDir'  = if T.null rbd
                                then []
                                else T.split (== '/') rbd
            , clientIdent'    = ci
            , ignorePath'     = T.unpack iPath
            , statePath'      = T.unpack <$> mStatePath
            }

  parseJSON _          = mzero


type ErrorMessage = String


readIgnore      :: Sync -> IO Sync
readIgnore sync = (fmap lines . readFile . ignorePath' . syncConfig $ sync) >>= \ps ->
                    return $ sync { ignore = ps }

readConfig    :: FilePath -> IO (Either ErrorMessage Sync)
readConfig fp = decodeFileEither fp >>= \es -> case es of
                  Left parseError -> return . Left . showError $ parseError
                  Right sc        -> let s = Sync undefined sc []
                                     in Right <$> readIgnore s
  where
    showError pe = "Error parsing sync config file " ++ show fp ++ ":\n" ++ show pe
