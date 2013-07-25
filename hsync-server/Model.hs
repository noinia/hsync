module Model where

import Prelude
import Yesod
import Data.Text(Text)
import Database.Persist.Quasi

import Data.Typeable

-- import System.Locale
-- import Data.Time

-- import qualified Data.Time.Format as D
import qualified Data.Text as T

type UserIdent = Text
type Password = Text
type HashedPassword = Text


type DateTime = Text

-- newtype DateTime = DateTime LocalTime
--     deriving (Show,Eq,Ord)

-- instance ParseTime DateTime where
--     buildTime tl = DateTime . buildTime tl


-- dateTimeFormat :: String
-- dateTimeFormat = "%0C%F-%T"

-- instance PathPiece DateTime where
--     toPathPiece (DateTime t) = T.pack . formatTime defaultTimeLocale dateTimeFormat $ t
--     fromPathPiece = D.parseTime defaultTimeLocale dateTimeFormat . T.unpack


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")


type FileIdent = Text

data Path = Path UserIdent [Text]
    deriving (Show,Read,Eq,Ord)


instance PathMultiPiece Path where
    toPathMultiPiece (Path u ps) = u : ps -- map T.pack ps
    fromPathMultiPiece (u:ps) = Just $ Path u ps -- . map T.unpack
