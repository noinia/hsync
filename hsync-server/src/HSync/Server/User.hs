{-# Language TemplateHaskell    #-}
{-# Language OverloadedStrings #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DeriveDataTypeable #-}
module HSync.Server.User where

import Prelude


import Data.Monoid

import Data.Data(Data, Typeable)
import Data.Default
import Data.IxSet

import Data.SafeCopy(SafeCopy(..), base, deriveSafeCopy)

import Data.Text(Text)

import HSync.Common.Types

import qualified Data.IxSet as I

--------------------------------------------------------------------------------

newtype FullName = FullName { unFullName :: Text }
                 deriving (Show,Read,Eq,Ord,Data,Typeable,SafeCopy)



data User = User { userId   :: UserIdent
                 , fullName :: FullName
                 , password :: HashedPassword

                 }
            deriving (Show,Read,Eq,Ord,Data,Typeable)

$(deriveSafeCopy 0 'base ''User)




instance Indexable User where
  empty = ixSet [ ixFun $ \u -> [ userId u ]
                , ixFun $ \u -> [ fullName u ]
                ]

-- | Users have a unique userId
newtype UserIndex = UserIndex { unUIdx :: IxSet User }
                  deriving (Show,Read,Eq,Data,Typeable,SafeCopy)


instance Default UserIndex where
  def = UserIndex I.empty

type ErrorMessage = Text

insert                 :: User -> UserIndex -> Either ErrorMessage UserIndex
insert u (UserIndex s) = case I.getOne $ s @= (userId u) of
  Nothing -> Right $ UserIndex (I.insert u s)
  Just _  -> Left    "Username already taken."


lookupUser                  :: UserIdent -> UserIndex -> Maybe User
lookupUser ui (UserIndex s) = I.getOne $ s @= ui
