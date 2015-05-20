-- Necessary:
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- Incidental:
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Main where

import Control.Applicative
import Control.Monad
import Data.Hashable
import Data.Traversable (for)
import Data.Typeable
import Haxl.Core

main :: IO ()
main = do
  let myid = 1
  let stateStore = stateSet UserState{me = myid} stateEmpty
  env0 <- initEnv stateStore ()

  putStrLn "\nTesting My Friends\n" 

  myfriends <- runHaxl env0 (getAllHerFriends myid)
  print myfriends

  let other = 3
  putStrLn "\nTesting Other's Friends\n" 

  othersfriends <- runHaxl env0 (getAllHerFriends other)
  print othersfriends


-- Data source API.

getAllHerFriends :: Id -> Haxl [Name]
getAllHerFriends id = do 
  userIds <- getFriendIdsById id 
  for userIds $ \userId -> do 
    getUsernameById userId

getAllUsernames :: Haxl [Name]
getAllUsernames = do
  userIds <- getAllUserIds
  for userIds $ \userId -> do
    getUsernameById userId

getAllUserIds :: Haxl [Id]
getAllUserIds = dataFetch GetAllIds

getUsernameById :: Id -> Haxl Name
getUsernameById = dataFetch . GetNameById

getFriendIdsById :: Id -> Haxl [Id]
getFriendIdsById = dataFetch . GetFriendIds

-- Aliases.

type Haxl = GenHaxl ()
type Id = Int
type Name = String

-- Data source implementation.

data UserReq a where
  GetAllIds   :: UserReq [Id]
  GetNameById :: Id -> UserReq Name
  GetFriendIds:: Id -> UserReq [Id]
  deriving (Typeable)

deriving instance Eq (UserReq a)
instance Hashable (UserReq a) where
    hashWithSalt s GetAllIds        = hashWithSalt s (0::Int)
    hashWithSalt s (GetNameById a)  = hashWithSalt s (1::Int, a)
    hashWithSalt s (GetFriendIds a) = hashWithSalt s (2::Int, a)

deriving instance Show (UserReq a)
instance Show1 UserReq where show1 = show

instance StateKey UserReq where
  data State UserReq = UserState {me :: Id}

instance DataSourceName UserReq where
  dataSourceName _ = "UserDataSource"

instance DataSource u UserReq where
  fetch _state _flags _userEnv blockedFetches = SyncFetch $ do
    mapM_ fetchOne blockedFetches

fetchOne :: BlockedFetch UserReq -> IO ()
fetchOne (BlockedFetch rq r) = sql rq >>= putSuccess r  





-- Mock SQL API.

allids :: [Id]
allids = [1..10]
idToName :: Id -> Name 
idToName = show
friends i | i > 1 && i < 10 = [i-1,i+1, 5]
friends 1                   = [2, 3, 5]
friends 10                  = [8, 9, 5]
friends _                   = []

sql :: UserReq a -> IO a
sql (GetFriendIds id) 
  = do { print ("Friends of " ++ show id ++ " are " ++ show (friends id)) 
       ; return $ friends id }

sql (GetNameById id) 
  = do { print ("Name of " ++ show id ++ " is " ++ show (idToName id)) 
       ; return $ idToName id }

sql GetAllIds 
  = do { print ("AllIds = " ++ show allids) 
       ; return allids}
