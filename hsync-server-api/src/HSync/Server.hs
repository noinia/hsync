{-# LANGUAGE TypeFamilies #-}
{-# Language TemplateHaskell  #-}
{-# Language FlexibleContexts #-}
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


-- | The (Implementation HSyncServer) type should implement the following
--  typeclass.
class IsHSyncServerImplementation implementation where
  getStaticSubSite :: implementation -> Static


-- | Generate the routes for a HSyncServer
mkYesodData "HSyncServer" $(parseRoutesFile "routes")

-- | The getStatic function used in our routes
getStatic :: IsHSyncServerImplementation (Implementation HSyncServer)
          => HSyncServer -> Static
getStatic = getStaticSubSite . implementation
