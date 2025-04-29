module Interpreter.Eval

    open Result
    open Language
    open StateMonad
    
    //Computational expression stuff
    type StateBuilder() =  
        member this.Bind(f, x) = (>>=) f x  
        member this.Return(x) = ret x  
        member this.ReturnFrom(x) = x  
        member this.Combine(a, b) = a >>= (fun _ -> b) 
      
    let eval = StateBuilder()
    
    //Normal stuff
    let readFromConsole () = System.Console.ReadLine().Trim()
    let tryParseInt (str : string) = System.Int32.TryParse str    
    let rec readInt () = 
        let input = readFromConsole()
        let success, result = tryParseInt(input)
        match success with
        |true -> ret result
        |false-> printfn($"{input} is not an integer") ; readInt() 
        
    let rec arithEval (a:aexpr) : int stateMonad =
        match a with
        | Num x -> ret x 
        | Var v -> getVar v
        | Add(b,c) -> 
            (arithEval b) >>= (fun x ->
            (arithEval c) >>= (fun y ->
            ret(x + y)))
            
        | Mul(b,c) ->
            (arithEval b) >>= (fun x ->
            (arithEval c) >>= (fun y ->
            ret(x * y)))
            
        | Div(b,c) ->
                (arithEval b) >>= (fun x ->
                (arithEval c) >>= (fun y ->
                if y = 0 then fail DivisionByZero else
                ret(x / y)))
        
        | Mod(b,c) ->
                (arithEval b) >>= (fun x ->
                (arithEval c) >>= (fun y ->
                if y = 0 then fail DivisionByZero else
                ret(x % y)))
        | MemRead e1 ->
            arithEval e1 >>= getMem
        | Random -> random
        | Read -> readInt()
        | Cond(b,a1,a2) ->
           boolEval b >>= (fun bool -> if bool then arithEval a1 else arithEval a2)
        | FunctionCall _ -> ret 1 
           
    and boolEval (b: bexpr) : bool stateMonad =
        match b with
        | TT -> ret true
        | Eq(a,c) ->
            (arithEval a) >>= (fun x ->
            (arithEval c) >>= (fun y ->
            ret(x = y)))
        
        | Lt(a,c) ->
            (arithEval a) >>= (fun x ->
            (arithEval c) >>= (fun y ->
            ret(x < y)))
        
        | Conj(a,c) ->
            (boolEval a) >>= (fun x ->
            (boolEval c) >>= (fun y ->
            ret(x && y)))
            
        | Not a ->
            (boolEval a) >>= (fun y -> ret(not y))
    
    
    //Computational Expression 
    let rec arithEval2 (a:aexpr) : int stateMonad = eval {
        match a with
        | Num x -> return x 
        | Var v -> return! getVar v
        | Add(b,c) ->
                let! x =  arithEval2 b
                let! y = arithEval2 c
                return x + y

        | Mul(b,c) ->
                let! x =  arithEval2 b
                let! y = arithEval2 c
                return x * y
            
        | Div(b,c) ->
                let! x =  arithEval2 b
                let! y = arithEval2 c
                if y <> 0 then return x / y
                else return! fail DivisionByZero
        
        | Mod(b,c) ->
                let! x =  arithEval2 b
                let! y = arithEval2 c
                if y <> 0 then return x % y
                else return! fail DivisionByZero

        | MemRead e1 ->
            let! x = arithEval2 e1
            let! y = getMem x 
            return y 
        | Random -> return! random
        | Read -> return! readInt()
        | Cond(b,a1,a2) ->
           let! bool = boolEval2 b
           if bool then
               let! x = arithEval2 a1
               return x 
           else
               let! y = arithEval2 a2
               return y
        
        | FunctionCall _ -> return 0}

           
    and boolEval2 (b: bexpr) : bool stateMonad = eval{
        match b with
        | TT -> return true
        | Eq(a,c) ->
            let! x =  arithEval2 a
            let! y = arithEval2 c
            return x = y
        | Lt(a,c) ->
            let! x =  arithEval2 a
            let! y = arithEval2 c
            return x < y
        | Conj(a,c) ->
            let! x =  boolEval2 a
            let! y = boolEval2 c
            return x && y
            
        | Not a ->
            let! x = boolEval2 a
            return (not x)
            }
            
    let split (s1 : string) (s2 : string) = s2 |> s1.Split |> Array.toList
        
    let mergeStrings (es : aexpr list) (s : string) =
        let s1 = split s "%"
        
        let rec mergeStringsA (aexprlist : aexpr list) (stringlist : string list) c=
          match stringlist with
                | [] ->  ret (c stringlist) 
                | x :: stringlist2 ->
                    match aexprlist with
                    | [] -> ret (c stringlist)
                    | y :: aexprlist2 ->
                        (arithEval y) >>= (fun v -> mergeStringsA aexprlist2 stringlist2 (fun r ->  c (x + string v :: r )))
                                              
        let result = mergeStringsA es s1 id 
        result >>= fun stringlist -> ret (String.concat "" stringlist )                 
        
    let rec stmntEval s =
        match s with
        | Skip -> ret ()
        | Declare v -> declare v
        | Assign(v,a) ->
            (arithEval2 a) >>= (fun x ->setVar v x)
            
        | Seq(s1,s2) ->
            (stmntEval s1) >>= (fun _ -> stmntEval s2)
     
        | If(guard,s1,s2) ->
            
            boolEval guard >>= (fun result ->
                if result
                then stmntEval s1
                else stmntEval s2)
            
        | While(guard, s') ->
             boolEval guard >>= (fun result ->
                if result
                then (stmntEval s') >>= fun _ -> stmntEval (While(guard,s'))
                else ret ())
        
        | Alloc(x,e) ->
            let size = arithEval2 e
            size >>= (fun size -> alloc x size)
            
        | Free (e1,e2) ->
            let ptr = arithEval2 e1
            let size = arithEval2 e2
            
            ptr >>= (fun ptr2 -> size >>= (fun size2 -> free ptr2 size2))
         
        | MemWrite(e1,e2) ->
            (arithEval2 e1) >>= (fun x ->
            (arithEval2 e2) >>= (fun y ->
            setMem x y))
            
        | Print(es, s) ->
            let check = 
                let evalList = List.map arithEval2 es //evaluate each aexpr in the list
                    
                let rec evaluate lst = // function to check if each aexpr was successfully evaluated
                   match lst with
                   | [] -> ret ()
                   | x :: xs ->
                            x >>= (fun _ -> evaluate xs)
                        
                evaluate evalList
              
            check >>= (fun _ -> mergeStrings es s >>= fun r -> printfn "%A" r ; ret ())
        
        | Return _ -> ret () 