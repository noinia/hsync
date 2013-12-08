module HSync.Client.HSyncClient where

import Data.Text(Text)

type GlobalSettings = Text

-- | The global application
data HSyncClient = HSyncClient { globalSettings :: GlobalSettings
                               }


-- clientMain fp = return ()
