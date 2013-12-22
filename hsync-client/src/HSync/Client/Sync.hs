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

import qualified Data.Text as T
import qualified Data.List


--------------------------------------------------------------------------------

type ServerAddress = Text
type PartialPath = String


-- | Each Synchronized Tree has its own manager/settings etc
data Sync = Sync { httpManager     :: Manager
                 , localBaseDir    :: FilePath -- without trailing /
                 , serverAddress   :: ServerAddress
                 , user            :: UserIdent
                 , hashedPassword  :: HashedPassword
                 , remoteBaseDir   :: PartialPath
                 , clientIdent     :: ClientIdent
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




fromConfig lbd s u hp rbd ci = def { localBaseDir   = lbd
                                   , serverAddress  = s
                                   , user           = u
                                   , hashedPassword = hp
                                   , remoteBaseDir  = rbd
                                   , clientIdent    = ci
                                   }

instance FromJSON Sync where
  parseJSON (Object v) = fromConfig <$> v .: "localBaseDir"
                                    <*> v .: "server"
                                    <*> v .: "username"
                                    <*> v .: "hashedPassword"
                                    <*> v .: "remoteBaseDir"
                                    <*> v .: "clientIdent"
  parseJSON _          = mzero






type ErrorMessage = String

readConfig    :: FilePath -> IO (Either ErrorMessage Sync)
readConfig fp = decodeFileEither fp >>= \es -> return $ case es of
                  Left parseError -> Left . showError $ parseError
                  Right s         -> Right s
  where
    showError pe = "Error parsing sync config file " ++ show fp ++ ":\n" ++ show pe
