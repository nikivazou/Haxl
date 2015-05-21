- TODO: check exaclty where I use the axioms

How to specify credentials in Haxl?
-----------------------------------

- Specify credentials at the `Request` constructors: 

    ```
    data UserReq a where  
      GetFriendIds:: Id -> UserReq [Id] 
      GetLocation :: Id -> UserReq Location 
    ```

  What each `UserReq` requires to run?
  
    ```
    GetFriends  :: x:Id -> {req : UserReq [{v:Id | isFriend x v}] | credential req = cfriends x}
    
    GetLocation :: x:Id -> {req : UserReq Location | credential req = clocation x}
    ```

  **POLICIES as axioms**
  - Anyone can query the list of  `x`'s friends
      `A1: forall y x. hasCredentials y (cfriends x)`
      
  - Only `x`'s friends can see `x`'s location
      `A2: forall x y. isFriend x y => hasCredentials x (clocation y)`
 
- Where do I use the `UserReq`? 
  - As arguments at the implementation of the `fetch`
    In our example, in the `sql`, but the `sql` can have true preconditions.
 
  - At the API: 
    For example 

    ```
    getUserLocation :: Id -> Haxl Location
    getUserLocation x = dataFetch $ GetLocation x
    ```
    
    I want the API to have the same credentials as the `UserRequest`. 
    Thus, `dataFetch` preserves the credentials:
    
    ```
   dataFetch :: (DataSource u r, Request r a) => {req: r a} -> {haxl : GenHaxl u a | credential req = credential haxl}
    ```
    So, I get
    
    ```
    getUserLocation  :: x:Id -> {haxl : Haxl Location | credential haxl = clocation x}
    getFriendIdsById :: x:Id -> {haxl : Haxl [{v:Id | isFriend x v}] | credential haxl = cfriends x}
    ```
    
  Note: the `isFriend x v` instide the result type is taken by polymorphism, as long as, type variable `a` in `dataFetch` is not comnstraint to be true by type classes... This needs some work.

- Using the API:
 
   ```
   getHerFiendsLocations x = do 
     userIds <- getFriendIdsById x           // {haxl : Haxl [{v:Id | isFriend x v}] | credential haxl = cfriends x}
     locs    <- for userIds getUserLocation  // {haxl : Haxl [Loc] | hasCredentials x haxl}
     return $ zip userIds locs  
   ```

  For the above typechecking we require the "connectives" to have appropriate types:
  
  
  - Type for `for`:  we can just use unification

  ```
    for  :: forall p :: m b -> Prop.
           [a] -> (y:a -> m <p> b) -> m <p> [b]
  ```
  
  Instantiate `p` with `hasCredentials x` to get a good type

  ```
    for  :: [a] 
        -> (y:a -> {haxl: m b | hasCredentials x haxl}) // since all ys are friends of x 
        -> {haxl:m [b]| hasCredentials x haxl}
  ```
  
  Alternatively, and more precicely, we can have the result return the _lub_ of the credentials, 
  but it is not clear how to express that.
  
  To use that we just need an abstract refinement 

  - Type of bind: As above it would be wonderful to use _lup_ and be precise, but we can again use unification 
    
    ```
    >>= :: forall <p :: m a -> Prop>. m <p> a -> (a -> m <p> b) -> m <p> b
    ```
    And instantiate again `p` with `hasCredentials x`, since `credential haxl = cfriends x => hasCredentials x haxl`. 

  - Type of return: Return should safisfy any predicate 

    ```
     return  :: forall <p :: m a -> Prop>. a -> m <p> a
    ```

  With the above we have the type 
  
  ```
    getHerFiendsLocations :: x:Id -> {haxl : Haxl [(Id, Location)] | hasCredentials x haxl}
  ```
  
  - Finally by giving the appropriate type to `runHaxl` we can verify that all the datafetches have the appropriate credentials
  
  ```
  runHaxl :: env:Env u -> {haxl:GenHaxl u a | hasCredentials (idOfEnv env) haxl} -> IO a
  ```
  
  We can verify that for _any_ ids `x` and `y` and any environment `env` 
  so that `idOfEnv env == x`,
  `runHaxl env (getHetFirendsLications x)` is SAFE, but 
  `runHaxl env (getHetFirendsLications y)` is UNSAFE. 
  
- Vocabulary 
  - `isFriend :: Id -> Id -> Prop`
  - `credential :: UserReq a -> Credential` and `credential :: GenHaxl u a -> Credential`
  - `cfriends :: Id -> Credential`
  - `clocation :: Id -> Credential`
  - `hasCredentials :: Id -> Credential -> Prop`
  - `idOfEnv :: Env u -> Id`
  
  Then, Axioms `A1, A2`
