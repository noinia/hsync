{-# Language  FlexibleContexts #-}
module HSync.Client.Actions where


import Control.Failure

import Control.Monad(when)
import Control.Monad.IO.Class (liftIO)


import Data.Aeson(encode,Value)
import Data.ByteString(ByteString)
import Data.Conduit
import Data.Conduit.Binary


import HSync.Client.Sync
import HSync.Common.FSTree(FSTree, readFSTree)
import HSync.Server.Import



import HSync.Server.Handler.Auth(requireRead,requireWrite)


import Network.HTTP.Conduit( Request
                           , Response
                           , Manager
                           , CookieJar
                           , RequestBody(..)
                           , HttpException(..)
--                           , createCookieJar
--                           , withManager
                           , requestBody
                           , responseBody
                           , responseStatus
                           , responseCookieJar
                           )
import Network.HTTP.Types
import Network.Wai(requestBody)


import System.Directory( removeFile )

import Yesod.Client



import qualified Data.Conduit.List          as CL
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Conduit               as C
import qualified Network.HTTP.Conduit       as HC
import qualified Data.Text                  as T




--------------------------------------------------------------------------------

login :: ( MonadResource m, Failure HttpException m
         , MonadBaseControl IO m) => ActionT m Bool
login = do
  sync <- getSync
  resp <- runGetRoute $ MyLoginR (user sync) (hashedPassword sync)
  body <- lift $ responseBody resp C.$$+- sinkLbs
  case LB.unpack body of
    "VALID"   -> setSessionCreds resp >> return True
    "INVALID" -> return False


setSessionCreds :: Monad m => Response body -> ActionT m ()
setSessionCreds = updateCookieJar

--------------------------------------------------------------------------------

remoteFileInfo    :: MonadIO m => FilePath -> ActionT m (FileIdent,Path)
remoteFileInfo fp = do
  sync <- getSync
--  fi   <- liftIO $ fileIdent fp -- TODO: Get the fi if it exists, and Nonexistent otherwise
  return (NonExistent,toRemotePath sync fp)


--------------------------------------------------------------------------------


-- | Run a getFile
getFile   :: ( MonadResource m, Failure HttpException m
         , MonadBaseControl IO m) => Path -> ActionT m ()
getFile p = do
  sync <- getSync
  resp <- runGetRoute $ FileR p
  let fp     = toLocalPath sync p
      status = responseStatus resp
  when (status == ok200) $ lift (responseBody resp C.$$+- sinkFile fp)

--------------------------------------------------------------------------------



-- | Run a postPut action
putFile    :: ( MonadResource m, Failure HttpException m
              , MonadIO m, MonadBaseControl IO m) => FilePath -> ActionT m ()
putFile fp = do
  (fi,p) <- remoteFileInfo fp
  let h = PutFileR fi p
      s = sourceFile fp
  sync <- getSync
  liftIO $ print $ toUrl sync h
  resp <- runPostRoute h s
  liftIO $ print "woei"
