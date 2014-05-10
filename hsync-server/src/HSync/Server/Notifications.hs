module HSync.Server.Notifications where

import Prelude

import Control.Applicative((<$>))

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM(atomically)

import Data.Conduit

import Data.List(isPrefixOf)

import HSync.Common.Notification
import HSync.Common.Types(Path(..))
import HSync.Common.DateTime(DateTime)

import HSync.Server
import HSync.Server.AcidSync(notificationUpdate, NotificationsAsOf(..))
import HSync.Server.AcidState(queryAcid)
import HSync.Server.Foundation

import Yesod


import Data.Conduit.List as CL

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

-- | Get a stream of notifications, starting *now*, for path p
notificationsFor   :: Path -> Handler (Source Handler Notification)
notificationsFor p =     ($= CL.filter ((`matches` p) . affectedPath . event))
                     <$> notifications
  where
    (Path u ps) `matches` (Path u' ps') = u == u' && ps' `isPrefixOf` ps


-- | Get a stream of notifications for path p as of dt
notificationsAsOf      :: DateTime -> Path -> Handler (Source Handler Notification)
notificationsAsOf dt p = do
                           newNots <- notificationsFor p
                           oldNots <- loadNotificationsAsOf dt p
                           return $ oldNots `concatSources` newNots

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


-- Given two sources s1 and s2 generate a source that *first* streams everything
-- from s1. If s1 is done only then start producing results using s2
concatSources       :: Monad m => Source m a -> Source m a -> Source m a
concatSources s1 s2 = s1 =$ await >>= \m -> case m of
                        Nothing -> s2
                        Just x  -> yield x >> concatSources s1 s2

-- | Gives a source with all notifications, *up until now* in the tree.
loadNotificationsAsOf    :: Monad m => DateTime -> Path -> Handler (Source m Notification)
loadNotificationsAsOf dt = fmap sourceList . loadNotificationsAsOfList dt

loadNotificationsAsOfList      :: DateTime -> Path -> Handler [Notification]
loadNotificationsAsOfList dt p = queryAcid $ NotificationsAsOf dt p
