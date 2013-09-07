module HSync.Server.Import
    ( module HSync.Server.Import
    ) where

import Prelude              as HSync.Server.Import hiding (head, init, last,
                                                           readFile, tail, writeFile)
import Yesod                as HSync.Server.Import hiding (Route (..))
import Yesod.Auth           as HSync.Server.Import

import Control.Applicative  as HSync.Server.Import (pure, (<$>), (<*>))
import Data.Text            as HSync.Server.Import (Text)

import HSync.Server.Foundation           as HSync.Server.Import
import HSync.Server.Model                as HSync.Server.Import
import HSync.Server.Settings             as HSync.Server.Import
import HSync.Server.Settings.Development as HSync.Server.Import
import HSync.Server.Settings.StaticFiles as HSync.Server.Import

import HSync.Common.Types     as HSync.Server.Import
import HSync.Common.Import    as HSync.Server.Import
import HSync.Common.FileIdent as HSync.Server.Import
import HSync.Common.AtomicIO  as HSync.Server.Import
import HSync.Common.FSTree    as HSync.Server.Import

#if __GLASGOW_HASKELL__ >= 704
import           Data.Monoid          as HSync.Server.Import
                                                 (Monoid (mappend, mempty, mconcat),
                                                 (<>))
#else
import           Data.Monoid          as HSync.Server.Import
                                                 (Monoid (mappend, mempty, mconcat))

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif
