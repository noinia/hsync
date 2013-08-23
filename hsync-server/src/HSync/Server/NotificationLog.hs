module HSync.Server.NotificationLog where

import HSync.Server.Import

import Data.Function(on)
import Data.Maybe(isJust)
import Data.Conduit
import Data.Conduit.Binary(sinkFile)
import Data.Conduit.Internal(connectResume,sourceToPipe,Pipe(..),ResumableSource(..))

import qualified Data.Conduit.List as C
import qualified Data.ByteString.Char8 as B


import Data.Time.Calendar
import Data.Time.Clock

-- writeNotificationLog :: Handler ()
-- writeNotificationLog = do
--                          ns  <- notifications
--                          dir <- extraNotificationLogDir <$> getExtra
--                          runResourceT $ notificationsByDate dir ns



nextDay (DateTime (UTCTime d t)) = DateTime $ UTCTime (addDays 1 d) t

generateNots = do
  now <- currentTime
  let days = take 10 . iterate nextDay $ now
  return . map noti . concatMap (replicate 3) $ days
      where
        path = Path "" []
        noti = (Notification (FileAdded path) "")

-- -- notificationFileSink :: MonadIO m => Sink Notification m ()
-- notificationFileSink dir s = do
--                            (rs,_) <- s $$+ byDate =$



testNots = do
  n <- generateNots
  let s = C.sourceList n
  runResourceT $ notificationsByDate "/tmp" s




-- splitC   :: (MonadIO m, Show a, Num a) => (a -> Bool) -> Sink a m ()



-- notificationsByDate :: --Monad m => Conduit Notification m Notification

-- notificationsByDate       :: (MonadResource m, MonadIO m) => FilePath ->
--                              Source m Notification -> Sink Notification m ()
notificationsByDate dir s = (s $$+ sink) >>= f . fst
    where
      sink = takeWhileByDate =$ simpleNotificationSinkByDate dir
      f rs = protect (hasInputLeft rs)
                     (connectResume rs sink >>= f . fst)
                     (return ())
      hasInputLeft (ResumableSource s _ ) = (s $$ C.peek) >>= return . isJust


-- | A conduit that passes through the notifications as long as they occur on
-- the same day as the first notification
takeWhileByDate :: Monad m => Conduit Notification m Notification
takeWhileByDate = conduitFromFirst ((==) `on` (day . timestamp))


-- | A sink that logs all notifications to a single file. The log file that we
-- write to is stored in dir and its name is obtained from the (date) of the
-- first filename from the first
simpleNotificationSinkByDate dir = fromFirst (simpleNotificationSinkByDate' dir)


-- | A sink that logs all notifications to a single file. The log file that we
-- write to is stored in dir and its name is obtained from the timestamp of
-- notification n
simpleNotificationSinkByDate'       :: (MonadResource m, MonadIO m) =>
                                       FilePath -> Notification ->
                                       Sink Notification m ()
simpleNotificationSinkByDate' dir n = let ds = show . day . timestamp $ n
                                          fp = dir ++ "/" ++ ds           in
                                     simpleNotificationFileSink fp



-- | A Sink that simply logs all notifcations to the given file
simpleNotificationFileSink   :: (MonadResource m, MonadIO m) =>
                                FilePath -> Sink Notification m ()
simpleNotificationFileSink f = C.map printNotification =$ sinkFile f
    where
      printNotification = B.pack . (++"\n") . toLog


--------------------------------------------------------------------------------
-- | Helper functions


-- | conduitFromFirst mkP creates a conduit based on the first incoming element x
-- and the function mkP: the resulting stream can be considered as a source that
-- produces a stream of elements as long as they satisfy the predicate (mkP x).
--
-- precondition: (mkP x) x == True
conduitFromFirst     :: Monad m => (a -> a -> Bool) -> Conduit a m a
conduitFromFirst mkP = fromFirst (\n -> takeWhileC (mkP n))

fromFirst            :: Monad m => (i -> Conduit i m o) -> Conduit i m o
fromFirst mkConsumer = await >>= maybe (return ())
                                       (\n -> leftover n >> mkConsumer n)


-- | takewhile as a conduit: i.e. it passes through the incoming stream as long as
-- the elements satisfy predicate p. Once we reach an element that does *not* satisfy
-- the predicate, we stop producing elements
takeWhileC   :: Monad m => (a -> Bool) -> Conduit a m a
takeWhileC p = await >>= maybe (return ())
                               (\x -> if p x then yield x >> takeWhileC p
                                             else leftover x)

--------------------------------------------------------------------------------
-- | Testing stuff

eqToFrst :: (Eq a, Monad m) => Conduit a m a
eqToFrst = conduitFromFirst (==)


-- eqToF = fromFirst (=$)


sink' :: (MonadIO m, Eq a, Show a) => Sink a m ()
sink' = eqToFrst =$ sink

testF = runResourceT $  source $$ sink'

--testF = runResourceT $ xs




source :: Monad m => Source m Integer
source = C.sourceList [0,0,0,1,1,1,2,2,3,3,3]


sink :: (Show a, MonadIO m) => Sink a m ()
sink = await >>= maybe (return ()) (\x -> (liftIO $ print x) >> sink)

-- takeWhileC   :: Monad m => (a -> Bool) -> ConduitM a a m (Source m a)
-- takeWhileC p = await >>= maybe (return C.sourceNull)
--                                (\x -> if p x then return $ yield x >> takeWhileC p
--                                              else return $ leftover x)




-- splitC' p s = do
--              (next,_) <- s $$++ (takeWhileC p =$ sink)
--              mh       <- C.head next
--              liftIO $ print $ "PART DONE"
--              splitC' p (C.drop 1 next)

test = runResourceT $ source $$+ (takeWhileC (/=0) =$ sink)

test2 = runResourceT $ splitC (/=0) source



splitC p s = do
             (rs,_)   <- s  $$+  (takeWhileC p =$ sink)
             (rs',mh) <- rs $$++ C.head
             (s',_)   <- unwrapResumable rs'
             liftIO $ print $ "PART DONE: " ++ show mh
             splitC p s'


-- do
-- runResourceT $ do
-- let s = sourceFile "input.txt" -- $= B.lines
-- (next, head) <- s $$+ (B.takeWhile (/= nl) >+> L.consume) -- (B.lines >+> L.take 1)
-- liftIO $ print head
-- rest <- next $$+- L.consume
-- liftIO $ print rest
