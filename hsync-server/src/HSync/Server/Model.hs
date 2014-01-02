module HSync.Server.Model where

import Prelude
import Yesod
import Database.Persist.Quasi

import Data.Typeable

import HSync.Common.Types

import qualified Data.Text as T

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
