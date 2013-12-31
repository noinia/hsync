{-# Language  OverloadedStrings
            , TypeFamilies
  #-}
module HSync.Client.Sync where

import Control.Monad(mzero)

import Data.Default
import Data.List(intercalate)
import Data.Yaml

import HSync.Client.Import

import HSync.Common.DateTime(DateTime)

import Network.HTTP.Conduit(Manager)

import System.FilePath.GlobPattern(GlobPattern, (/~))

import qualified Data.Text as T
import qualified Data.List


--------------------------------------------------------------------------------

type ServerAddress = Text
type PartialPath = String

type IgnoredPatterns = [GlobPattern]


-- | Each Synchronized Tree has its own manager/settings etc
data Sync = Sync { httpManager     :: Manager
                 , localBaseDir    :: FilePath -- without trailing /
                 , serverAddress   :: ServerAddress
                 , user            :: UserIdent
                 , hashedPassword  :: HashedPassword
                 , remoteBaseDir   :: PartialPath
                 , clientIdent     :: ClientIdent
                 , ignore          :: IgnoredPatterns
                 , ignorePath      :: FilePath
                 -- Database
                 }


instance Default Sync where
    def = Sync { httpManager     = undefined
               , localBaseDir    = "/Users/frank/tmp/synced"
               , serverAddress   = "http://localhost:3000"
               , user            = "nobody"
               , hashedPassword  = "hashed-secret"
               , remoteBaseDir   = ""
               , clientIdent     = "client-ident"
               , ignore          = []
               , ignorePath      = "config/ignore"
               -- Database
               }

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
                           fp' = Data.List.dropWhile (=='/') lp -- drop init /
                           p   = T.split (== '/') . T.pack $ (rbd ++ "/" ++ fp')
                       in toRemotePath' sync p


toRemotePath'   :: Sync -> SubPath -> Path
toRemotePath' s = Path (user s)




fromConfig lbd s u hp rbd ci iPath = def { localBaseDir   = lbd
                                         , serverAddress  = s
                                         , user           = u
                                         , hashedPassword = hp
                                         , remoteBaseDir  = rbd
                                         , clientIdent    = ci
                                         , ignorePath     = T.unpack iPath
                                         }

instance FromJSON Sync where
  parseJSON (Object v) = fromConfig <$> v .: "localBaseDir"
                                    <*> v .: "server"
                                    <*> v .: "username"
                                    <*> v .: "hashedPassword"
                                    <*> v .: "remoteBaseDir"
                                    <*> v .: "clientIdent"
                                    <*> v .: "ignore"
  parseJSON _          = mzero


type ErrorMessage = String


readIgnore      :: Sync -> IO Sync
readIgnore sync = (fmap lines . readFile . ignorePath $ sync) >>= \ps ->
                    return $ sync { ignore = ps }

readConfig    :: FilePath -> IO (Either ErrorMessage Sync)
readConfig fp = decodeFileEither fp >>= \es -> case es of
                  Left parseError -> return . Left . showError $ parseError
                  Right s         -> Right <$> readIgnore s
  where
    showError pe = "Error parsing sync config file " ++ show fp ++ ":\n" ++ show pe
