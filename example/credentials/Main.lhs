\begin{code}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Data.Hashable
import Data.Traversable (for)
import Data.Typeable
import Haxl.Core
\end{code}


Policies 

\begin{code}
main :: IO ()
main = do
  let myid  = 1
  let other = 2

  -- The environment has the credentials of the user me 
  -- in the user state, 
  let stateStore = stateSet UserState{me = myid} stateEmpty
  env <- initEnv stateStore ()

  -- This should type check
  putStrLn breakline
  putStrLn "\nTesting My Friends Locations\n" 
  myfriendslocs <- runHaxl env (getHerFiendsLocations myid)
  putStrLn "\nMy Friends Locations is:" 
  print myfriendslocs

  -- This should fail. 

  putStrLn breakline
  putStrLn breakline
  putStrLn "\nTesting Other's Friends Locations\n" 
  myfriendslocs <- runHaxl env (getHerFiendsLocations other)
  putStrLn "\nOther's Friends Locations is:" 
  print myfriendslocs


breakline = replicate 80 '-'
-- Data source API.
\end{code}

\begin{code}
getHerFiendsLocations :: Id -> Haxl [(Id, Location)]
getHerFiendsLocations id = do 
  userIds <- getFriendIdsById id -- userIds :: [{v:Id | isFriend id v }]
  locs    <- for userIds getUserLocation
  return $ zip userIds locs  

getUserLocation :: Id -> Haxl Location
getUserLocation = dataFetch . GetLocation

getFriendIdsById :: Id -> Haxl [Id]
getFriendIdsById = dataFetch . GetFriendIds

-- Aliases.

type Haxl      = GenHaxl ()
type Id       = Int
type Location = (Double, Double) 

-- Data source implementation.

data UserReq a where
  GetFriendIds:: Id -> UserReq [Id]
  GetLocation :: Id -> UserReq Location 
  deriving (Typeable)

deriving instance Eq (UserReq a)
instance Hashable (UserReq a) where
    hashWithSalt s (GetFriendIds a) = hashWithSalt s (1::Int, a)
    hashWithSalt s (GetLocation  a) = hashWithSalt s (2::Int, a)

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
\end{code}



Faking SQL QUERIES
------------------

\begin{code}
allids :: [Id]
allids = [1..10]

idToLocation x = (toEnum x, toEnum x) 
friends i | i > 1 && i < 10 = [i-1,i+1, 5]
friends 1                   = [2, 3, 5]
friends 10                  = [8, 9, 5]
friends _                   = []

sql :: UserReq a -> IO a
sql (GetFriendIds id) 
  = do { putStrLn ("Friends of " ++ show id ++ " are " ++ show (friends id)) 
       ; return $ friends id }

sql (GetLocation id) 
  = do { putStrLn ("Location of " ++ show id ++ " is " ++ show ((idToLocation id) :: Location))
       ; return $ idToLocation id }
\end{code}