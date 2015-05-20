How to specify credentials in Haxl?
-----------------------------------

(Bottom up description)
- Specify credentials at the bottom: 

  ```
  sql :: UserReq a -> IO a
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
    

# Vocabulary 
  - `isFriend :: Id -> Id -> Prop`
  - `credential :: UserReq a -> Credential`
  - `cfriends :: Id -> Credential`
  - `clocation :: Id -> Credential`
  - `hasCredentials :: Id -> Credential -> Prop`
  
  Then, Axioms `A1, A2`
