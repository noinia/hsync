module HSync.Client.Logger( logMIO
                          , logM
                          , debugM, infoM, noticeM
                          , warningM, errorM , criticalM, emergencyM

                          , Priority(..)
                          , LoggerName
                          , LogMessage
                          ) where


import Control.Applicative((<$>))
import Control.Monad.IO.Class(liftIO)


import HSync.Common.Types(ClientIdent)
import HSync.Client.ActionT
import HSync.Client.Sync(clientIdent)


-- import System.Log.Logger
-- import System.Log.Handler.Syslog
-- import System.Log.Handler.Simple
-- import System.Log.Handler (setFormatter)
-- import System.Log.Formatter

import System.Log(Priority(..))

import qualified Data.Text         as T
import qualified System.Log.Logger as Log

--------------------------------------------------------------------------------

type LoggerName = String
type LogMessage = String







logMIO      :: Maybe ClientIdent -> LoggerName -> Priority -> LogMessage -> IO ()
logMIO ci n = let cis = maybe "NoClient" T.unpack ci in
                  Log.logM ("HSync." ++ cis ++ "." ++ n)


logM       :: LoggerName -> Priority -> LogMessage -> Action ()
logM n p m = (Just . clientIdent <$> getSync) >>= \ci ->
               liftIO $ logMIO ci n p n


debugM   :: LoggerName -> LogMessage -> Action ()
debugM = flip logM DEBUG

infoM :: LoggerName -> LogMessage -> Action ()
infoM = flip logM INFO

noticeM :: LoggerName -> LogMessage -> Action ()
noticeM = flip logM NOTICE

warningM :: LoggerName -> LogMessage -> Action ()
warningM = flip logM WARNING

errorM :: LoggerName -> LogMessage -> Action ()
errorM = flip logM ERROR

criticalM :: LoggerName -> LogMessage -> Action ()
criticalM = flip logM CRITICAL

emergencyM :: LoggerName -> LogMessage -> Action ()
emergencyM = flip logM EMERGENCY
