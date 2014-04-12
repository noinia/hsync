{-# LANGUAGE TypeFamilies #-}
{-# Language TemplateHaskell  #-}
module HSync.Server where

import Yesod.Auth(Auth)
import Yesod.Static(Static)

import Yesod.Core

import HSync.Common.Types
import HSync.Common.FileIdent(FileIdent)
import HSync.Common.DateTime(DateTime)

--------------------------------------------------------------------------------

-- | The base data type for our server. i.e. the types our routes etc will be
-- associated with. To construct a HSyncServer we need an implementation of the
-- server; i.e. an object of type `HSyncImplementation HSyncServer'.
--
-- We have this wrapper type so that we can separate the routes, i.e. the API, from
-- the implementation of the server.
newtype HSyncServer = HSyncServer { implementation :: Implementation HSyncServer }


-- | The type family that expresses that a server of type `server` can be
-- implemented through something of type `Implementation server`.
type family Implementation server :: *


-- | Generate the routes for a HSyncServer
mkYesodData "HSyncServer" $(parseRoutesFile "routes")
