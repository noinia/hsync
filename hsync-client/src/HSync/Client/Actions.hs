module HSync.Client.Actions where

import Yesod
import Foundation

import HSync.Common.Types
import Data.List(intercalate)

import qualified Data.Text as T





login = do









-- class IsAction t where
--     toUrl           :: t -> String
--     reqMethod       :: t -> Method
--     responseHandler :: t -> Response -> Handler a

-- -- toAction          :: (IsAction a, Failure HttpException m) => String -> a -> m (Request m')
-- -- toAction base act = do
-- --   res <-


-- data Login = Login UserIdent HashedPassword
--            deriving (Show,Eq)

-- instance IsAction Login where
--     toUrl (Login u pw)   = intercalate "/" . map T.pack [u,pw]
--     reqMethod  _         = methodGet
--     responseHandler  _ r = undefined

-- -- data File = File Path

-- -- instance IsAction File where
-- --     toUrl (File p) = toFilePath "file" p
