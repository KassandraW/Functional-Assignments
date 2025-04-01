module Interpreter.StateMonad

    // Use one of these state monads depending on which combination of green, yellow, and red exercises you are doing.
    // Feel free to just delete the rest to unclutter the code.
    
    open State
    open Language
    let (=>>) x f = x |> Result.bind f 
    type 'a stateMonad = SM of (state -> Result<'a * state, error>)
        
    let ret x      = SM (fun st -> Ok(x, st))
    let fail err= SM (fun _ -> Error err)
    
    let bind (SM f) g =
        SM (fun st -> 
            match f st with
            | Ok (x, st') -> let (SM h) = g x in h st'
            | Error err   -> Error err) 
    
    let declare str  : unit stateMonad =
        SM (fun st ->  declare str st =>> (fun newSt  -> Ok((),newSt)))
            //match declare str st with
            //| Ok newSt -> Ok((), newSt) how all my functions looked before I used Result.bind. I keep forgetting bind.
            //| Error err   -> Error err)
        
    let setVar str (v:int) : unit stateMonad =
        SM (fun st -> setVar str v st =>> (fun newSt  -> Ok((),newSt)))
        
    let getVar str: int stateMonad =
        SM (fun st -> getVar str st =>> (fun value -> Ok(value,st)))
    
    let alloc str size : unit stateMonad =
        SM (fun st -> alloc str size st =>> (fun newSt  -> Ok((),newSt)))
       
    let free ptr size : unit stateMonad =
        SM (fun st -> free ptr size st =>> (fun newSt  -> Ok((),newSt)))
        
    let setMem ptr v : unit stateMonad =
        SM (fun st -> setMem ptr v st =>> (fun newSt  -> Ok((),newSt)))
        
    let getMem ptr: int stateMonad =
        SM (fun st -> getMem ptr st  =>> (fun value -> Ok(value,st)))
       
        
    let random : int stateMonad =
        SM ( fun st -> Ok(random st,st))
    
    
    let evalState  (st : state) (a: 'a stateMonad) =
        match a with
        | SM b -> //unwrap that thang
            b st =>> (fun (v,_)  -> Ok(v))

        
    
    
    
    // Red Green exercises
    (*
    type stateContMonad<'a, 'r> =
        SCM of (state ->
               (int -> stateContMonad<unit, 'r>) list ->
               (('a * state) option -> 'r) ->
               'r)
        
    let ret x= SCM (fun st _ f -> f (Some (x, st)))
    let fail     = SCM (fun _  _ f -> f None)
    
    let bind (SCM f : stateContMonad<'a, 'b> ) (g : 'a -> stateContMonad<'c, 'b>)  =
        SCM(fun st rs h ->
            f st rs (function
                     | Some(x, st') ->
                       let (SCM i) = g x
                       i st' rs h
                     | None -> h None))

    
    let callCC f =
        SCM (fun st rs cont ->
                let (SCM g) = f (fun x -> SCM (fun st _ _ -> cont (Some(x, st))))
                g st rs cont)
        
    
    let fret x : stateContMonad<unit, 'a> =
        SCM (fun st rs cont ->
            match rs with
            | c :: rs -> let (SCM f) = c x in f (popFrame st) rs cont
            | []      -> failwith "Should never happen")
        
    let fcall (fname : string) (args : int list) (runBody: stmnt -> stateContMonad<unit, 'a>) : stateContMonad<int, 'a>=
        callCC (fun c -> SCM (fun st rs f -> 
                                  match pushFrame fname args st with
                                  | None -> f None
                                  | Some (st', body) ->
                                       let (SCM g) = runBody body
                                       g st' (c :: rs) (function
                                                        | Some((), st'') -> f (Some(0, st''))
                                                        | None           -> f None)))
                                                        
    *)                                                       
    // Red Yellow exercises
    (*
    type stateContMonad<'a, 'r> =
        SCM of (state ->
               (int -> stateContMonad<unit, 'r>) list ->
               (Result<'a * state, error> -> 'r) ->
               'r)
        
    let ret x       = SCM (fun st _ f -> f (Ok (x, st)))
    let fail err = SCM (fun _  _ f -> f (Error err))
    
    let bind (SCM f : stateContMonad<'a, 'b> ) (g : 'a -> stateContMonad<'c, 'b>)  =
        SCM(fun st rs h ->
            f st rs (function
                     | Ok(x, st') ->
                       let (SCM i) = g x
                       i st' rs h
                     | Error err -> h (Error err)))
        
        
    let callCC f =
        SCM (fun st rs cont ->
                let (SCM g) = f (fun x -> SCM (fun st _ _ -> cont (Ok(x, st))))
                g st rs cont)
        
    
    let fret x : stateContMonad<unit, 'a> =
        SCM (fun st rs cont ->
            match rs with
            | c :: rs -> let (SCM f) = c x in f (popFrame st) rs cont
            | []      -> failwith "Should never happen")

        
    let fcall (fname : string) (args : int list) (runBody: stmnt -> stateContMonad<unit, 'a>) : stateContMonad<int, 'a>=
        callCC (fun c -> SCM (fun st rs f -> 
                                  match pushFrame fname args st with
                                  | Error err -> f (Error err)
                                  | Ok (st', body) ->
                                       let (SCM g) = runBody body
                                       g st' (c :: rs) (function
                                                        | Ok((), st'') -> f (Ok(0, st''))
                                                        | Error err    -> f (Error err))))

    
    
    
    *)
    
    
    let (>>=) a f = bind a f
    let (>>>=) a b = a >>= (fun _ -> b)
