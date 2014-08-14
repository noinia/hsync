{-# Language TemplateHaskell    #-}
{-# Language OverloadedStrings #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DeriveDataTypeable #-}
module HSync.Server.User where

import Prelude

import Data.Data(Data, Typeable)
import Data.Default
import Data.IxSet

import Data.SafeCopy(base, deriveSafeCopy)

import Data.Text(Text)

import HSync.Common.Types

import qualified Data.IxSet as I

--------------------------------------------------------------------------------

newtype RealName = RealName { unRealName :: Text }
                 deriving (Show,Read,Eq,Ord,Data,Typeable)
$(deriveSafeCopy 0 'base ''RealName)


--------------------------------------------------------------------------------

data User = User { userId   :: UserIdent
                 , realName :: RealName
                 , password :: HashedPassword
                 }
            deriving (Show,Read,Eq,Ord,Data,Typeable)

$(deriveSafeCopy 0 'base ''User)


instance Indexable User where
  empty = ixSet [ ixFun $ \u -> [ userId u ]
                , ixFun $ \u -> [ realName  u ]
                ]

--------------------------------------------------------------------------------

-- | Users have a unique userId
newtype UserIndex = UserIndex { unUIdx :: IxSet User }
                  deriving (Show,Read,Eq,Data,Typeable)
$(deriveSafeCopy 0 'base ''UserIndex)

instance Default UserIndex where
  def = UserIndex I.empty


insertUser                    :: User -> UserIndex -> Either ErrorMessage UserIndex
insertUser u ui@(UserIndex s) = case lookupUser (userId u) ui of
  Nothing -> Right $ UserIndex (I.insert u s)
  Just _  -> Left    "Username already taken."


lookupUser                  :: UserIdent -> UserIndex -> Maybe User
lookupUser ui (UserIndex s) = I.getOne $ s @= ui
