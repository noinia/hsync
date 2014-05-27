{-# LANGUAGE  OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module HSync.Client.Sync where

import Prelude hiding (FilePath)

import Control.Monad(mzero)

import Data.Default
import Data.Maybe(fromMaybe)
import Data.List(intercalate)
import Data.Set(Set)
import Data.Yaml

import Filesystem.Path.CurrentOS


import HSync.Client.Import
import HSync.Common.DateTime(DateTime)

import Network.HTTP.Conduit(Manager)

import System.FilePath.GlobPattern(GlobPattern)

import qualified Data.Text as T
import qualified Data.List

import qualified Filesystem.Path.CurrentOS as FP

--------------------------------------------------------------------------------

-- | The FilePaths of system-filepath include a trailing / for directories.
-- We sometimes get patsh that do not have such a trailing /
newtype FilePathIgnoringTrailingSlash = IgnoreSlash { unIgnoreSlash :: FilePath }
                                        deriving Show

instance Eq FilePathIgnoringTrailingSlash where
  (IgnoreSlash fp) == (IgnoreSlash fp') = dropTrailingSlash fp == dropTrailingSlash fp'

dropTrailingSlash fp
        | FP.null $ filename fp = parent fp </> dirname fp
        | otherwise             = fp

instance Ord FilePathIgnoringTrailingSlash where
  x@(IgnoreSlash fp) `compare` y@(IgnoreSlash fp')
    | x == y    = EQ
    | otherwise = fp `compare` fp'

--------------------------------------------------------------------------------

-- | Extention to use for partial files
partialFileExtension :: Text
partialFileExtension = "hsyncpart"

isPartialFile :: FilePath -> Bool
isPartialFile = flip hasExtension partialFileExtension

-- | Files that should be temporarily ignored (in order to avoid reuploading
--   incoming files)
type TemporaryIgnoreFiles = Set FilePathIgnoringTrailingSlash

type ServerAddress = Text

type IgnoredPatterns = [GlobPattern]


-- | Each Synchronized Tree has its own manager/settings etc
data Sync = Sync { httpManager     :: Manager
                 , syncConfig      :: SyncConfig
                 , ignore          :: IgnoredPatterns
                 }

data SyncConfig = SyncConfig { localBaseDir'   :: FilePath -- without trailing /
                             , serverAddress'  :: ServerAddress
                             , user'           :: UserIdent
                             , password'       :: HashedPassword
                             , remoteBaseDir'  :: SubPath
                             , clientIdent'    :: ClientIdent
                             , ignorePath'     :: FilePath
                             , statePath'      :: Maybe FilePath
                             }
                  deriving (Show,Eq)


-- Shortcuts for some of the things we need often from a sync

localBaseDir  = localBaseDir' . syncConfig
serverAddress = serverAddress' . syncConfig
user          = user' . syncConfig
password      = password' . syncConfig
remoteBaseDir = remoteBaseDir' . syncConfig
clientIdent   = clientIdent' . syncConfig


instance Default SyncConfig where
    def = SyncConfig { localBaseDir'    = fromText "/Users/frank/tmp/synced/"
                     , serverAddress'   = "http://localhost:3000"
                     , user'            = let Right u = userIdent "nobody" in u
                     , password'        = HashedPassword "hashed-secret"
                     , remoteBaseDir'   = []
                     , clientIdent'     = ClientIdent "client-ident"
                     , ignorePath'      = fromText "config/ignore"
                     , statePath'       = Nothing
                     }



statePath   :: Sync -> FilePath
statePath s = let p = fromText $ "state/" <> (unCI $ clientIdent s)
              in fromMaybe p . statePath' . syncConfig $ s

toLocalPath               :: Sync -> Path -> FilePath
toLocalPath s (Path _ ps) = toLocalPath' s ps

toLocalPath'      :: Sync -> SubPath -> FilePath
toLocalPath' s sp =  localBaseDir s </> FP.concat (map fromText sp)

-- |TODO Switch to the FilePath package
-- | given a local file path, create a (remote) Path corresponding to it
--
-- Precondition: localBaseDir cli is a basedir of the given file path.
-- this is not checked.
toRemotePath        :: Sync -> FilePath -> Path
toRemotePath sync fp = toRemotePath' sync sp
  where
    lbd = localBaseDir sync
    rbd = remoteBaseDir sync
    lp  = fromMaybe (error err) $ stripPrefix lbd fp
    sp  = map FP.encode . splitDirectories $ lp
    err = mconcat ["toRemotePath: Local basedir '"
                  , show lbd
                  , "' is not a prefix of '"
                  , show fp
                  , "'"
                  ]

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
        def { localBaseDir'  = FP.decode $ lbd <> "/"
            , serverAddress' = s
            , user'          = u
            , password'      = hp
            , remoteBaseDir' = if T.null rbd
                               then []
                               else T.split (== '/') rbd
            , clientIdent'   = ci
            , ignorePath'    = FP.decode iPath
            , statePath'     = (\p -> FP.decode $ p <> "/") <$> mStatePath
            }

  parseJSON _          = mzero


--type ErrorMessage = String


readIgnore      :: Sync -> IO Sync
readIgnore sync = (readLines . ignorePath' . syncConfig $ sync) >>= \ps ->
                    return $ sync { ignore = ps }
  where
    -- readLines :: FilePath -> IO IgnoredPattern
    readLines = fmap lines . readFile . FP.encodeString

readConfig    :: FilePath -> IO (Either ErrorMessage Sync)
readConfig fp = decodeFileEither fp' >>= \es -> case es of
                  Left parseError -> return . Left . showError $ parseError
                  Right sc        -> let s = Sync undefined sc []
                                     in Right <$> readIgnore s
  where
    fp'          = FP.encodeString fp
    showError pe = mconcat [ "Error parsing sync config file "
                           , showT fp
                           , ":\n"
                           , showT pe
                           ]
