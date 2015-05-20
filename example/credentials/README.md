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
      ` forall y x. hasCredentials y (cfriends x)`
      
  - Only `x`'s friends can see `x`'s location
      `forall x y. isFriend x y => hasCredentials x (clocation y)`
    

