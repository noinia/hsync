module HSync.Common.NotificationTree( NotificationTree

                                    , module HSync.Common.TimedFSTree
                                    ) where


import Control.Monad.IO.Class(MonadIO(..))

import HSync.Common.TimedFSTree
import HSync.Common.Notification


--------------------------------------------------------------------------------


-- TODO, We now store the path (and maybe even the associated time) twice:
-- implicitly in the tree + explicitly in a notification. Removing this can
-- decrease the size of the data structure. (which is important since we are
-- acidizing this thing.)
type NotificationTree = TimedFSTree Notification







-- readNotificationTree baseDir = readTimedFSTree baseDir f
--   where
--     f = modificationTime
