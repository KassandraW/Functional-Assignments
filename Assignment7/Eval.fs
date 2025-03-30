module Interpreter.Eval

    open Result
    open Language
    open StateMonad
    
      
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
        
            
    let split (s1 : string) (s2 : string) = s2 |> s1.Split |> Array.toList
    let mergeStrings (es : aexpr list) (s : string): Result<string,error> =
        let s1 = split s "%"
        
        let rec mergeStringsA (aexprlist : aexpr list) (stringlist : string list) (acc : string list) : Result<string list, error>  =
            match stringlist with
                | [] ->  Ok acc
                | x :: stringlist2 ->
                    match aexprlist with
                    | [] -> Ok (List.rev(x :: acc))
                    | y :: aexprlist2 ->
                        (arithEval y st) >>= (fun v -> mergeStringsA aexprlist2 stringlist2 (x + string v :: acc))
                     
        let result = mergeStringsA es s1 []
        result |> map (String.concat "")         
        
    let mergeStrings2 (es : aexpr list) (s : string) (st : state) : Result<string,error> =
        let s1 = split s "%"
        
        let rec mergeStringsA (aexprlist : aexpr list) (stringlist : string list) c=
          match stringlist with
                | [] ->  Ok (c stringlist) 
                | x :: stringlist2 ->
                    match aexprlist with
                    | [] -> Ok (c stringlist)
                    | y :: aexprlist2 ->
                        (arithEval y st) >>= (fun v -> mergeStringsA aexprlist2 stringlist2 (fun r ->  c (x + string v :: r )))
                                              
        let result = mergeStringsA es s1 id 
        result |> map (String.concat "")                 
        
       
           
    
    
    let rec stmntEval s st =
        match s with
        | Skip -> ret st
        | Declare v -> declare v
        | Assign(v,a) ->
            (arithEval a) >>= (fun x ->setVar v x)
            
        | Seq(s1,s2) ->
            (stmntEval s1 st) >>= stmntEval s2
     
        | If(guard,s1,s2) ->
            match boolEval guard with
            | Ok x ->
                match x with
                | true -> stmntEval s1 st 
                | false -> stmntEval s2 st 
            | Error e -> Error e 
            
        | While(guard, s') ->
            match boolEval guard with
            | Ok x ->
                match x with
                | true ->
                    (stmntEval s' st) >>= stmntEval(While(guard,s'))
                | false -> Ok st 
            | Error e -> Error e
        
        | Alloc(x,e) ->
            let size = arithEval e st
            size >>= (fun size -> alloc x size st)
            
        | Free (e1,e2) ->
            let ptr = arithEval e1 st 
            let size = arithEval e2 st
            
            ptr >>= (fun ptr2 -> size >>= (fun size2 -> free ptr2 size2 st))
         
        | MemWrite(e1,e2) ->
            (arithEval e1 st) >>= (fun x ->
            (arithEval e2 st ) >>= (fun y ->
            setMem x y st ))
            
        | Print(es, s) ->
            let check =
                let evalList = List.map (fun x -> arithEval x st) es //evaluate each aexpr in the list 
                let rec evaluate lst = // function to check if each aexpr was successfully evaluated
                    match lst with
                    | [] -> Ok 1 
                    | x :: xs ->
                        x >>= (fun _ -> evaluate xs)
                    
                evaluate evalList
            
            match check with
            | Error e -> Error e
            | Ok _ ->
                let vs = List.map (fun x -> match arithEval x st with
                                            | Ok x -> x
                                            | _ -> 0) es
                
                let percents = s |> String.filter ((=) '%') |> String.length
                
                if vs.Length = percents then 
                    let result = mergeStrings2 es s st
                    result >>= (fun x -> printfn "%A" x ; Ok st)
                else Error (IllFormedPrint(s, vs))