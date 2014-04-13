module HSync.Server.Notifications where

import Prelude

import Control.Applicative((<$>))

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM(atomically)
import Control.Monad.IO.Class(MonadIO(..))

import Data.Conduit

import HSync.Common.Notification

import HSync.Server
import HSync.Server.AcidSync(AcidSync, notificationUpdate)
import HSync.Server.Foundation

import Yesod

--------------------------------------------------------------------------------
-- | Storing Notifications

-- | Log the given notification.
logNotification   :: Notification -> Handler ()
logNotification n = do
                    c <- notificationChan <$> getImplementation
                    lift $ atomically (writeTChan c n)


-- | Consume notifications by storing them in our FSState tree
notificationSink     :: MonadIO m
                     => HSyncServerImplementation -> Sink Notification m ()
notificationSink hss = let acid = acidSync hss in
                       awaitForever $ lift . notificationUpdate acid

--------------------------------------------------------------------------------
-- | Obtaining Notifications

-- | Get a stream of notifications in the Handler monad
notifications :: Handler (Source Handler Notification)
notifications = getImplementation >>= notifications'


-- | Get a stream of notifications
notifications' :: (Functor m, MonadIO m)
               => HSyncServerImplementation -> m (Source m Notification)
notifications' = fmap chanToSource . dupChan . notificationChan
  where
    dupChan c = liftIO $ atomically (dupTChan c)


chanToSource   :: MonadIO m => TChan a -> Source m a
chanToSource c = do
                   x <- liftIO $ atomically (readTChan c)
                   yield x
                   chanToSource c

--------------------------------------------------------------------------------

-- | Keep track of all notifications in the given HSyncServer. I.e. log all
--  notifications produced into our FSState tree
storeNotifications     :: (Functor m, MonadIO m)
                       => HSyncServerImplementation -> m ()
storeNotifications hss = notifications' hss >>= ($$ notificationSink hss)
