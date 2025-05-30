﻿module ReExam2023

(* If you are importing this into F# interactive then comment out
   the line above and remove the comment for the line bellow.

   Do note that the project will not compile if you do this, but 
   it does allow you to work in interactive mode and you can just remove the '=' 
   to make the project compile again.

   You will also need to load JParsec.fs. Do this by typing
   #load "JParsec.fs" 
   in the interactive environment. You may need the entire path.

   Do not remove the module declaration (even though that does work) because you may inadvertently
   introduce indentation errors in your code that may be hard to find if you want
   to switch back to project mode. 

   Alternative, keep the module declaration as is, but load ExamInteractive.fsx into the interactive environment
   *)
(*
 module ReExam2023 = 
 *)

(* 1: Arithmetic *)
    
    type arith =
    | Num of int
    | Add of arith * arith
    
    let p1 = Num 42
    let p2 = Add(Num 5, Num 3)
    let p3 = Add(Add(Num 5, Num 3), Add(Num 7, Num (-9)))
    
(* Question 1.1: Evaluation *)
    let rec eval (a:arith) =
        match a with
        | Num x  -> x
        | Add (x,y) -> eval x + eval y 
    
(* Question 1.2: Negation and subtraction *)
    let rec negate a =
        match a with
        | Num x  -> Num (-x)
        | Add (x,y) ->Add(negate x, negate y)
        
    let subtract a b: arith  =
        Add(a,negate b)
        

(* Question 1.3: Multiplication *)
        
    let rec multiply a b : arith =
        match a,b with
        | Num x , Num y -> Num (x * y)
        | Num x, Add(c,d) -> Add (multiply (Num x) c, multiply (Num x) d) 
        | Add(c,d), Num y  -> Add(multiply (Num y) c, multiply (Num y) d)
        | Add(c,d), e -> Add (multiply c e, multiply d e)
        
        
    
(* Question 1.4: Exponents *)

    let pow a b : arith =
        let rec aux acc i =
            match eval i with
                | 1 -> acc
                | _ ->
                    aux (multiply acc a) (subtract i (Num 1) )
        aux a b
      
            
    
(* Question 1.5: Iteration *)

    let rec iterate (f : 'a -> 'a) (acc: 'a) (a : arith) : 'a =
        match eval a with
        | 0 -> acc
        | _ -> iterate f (f acc) (subtract a (Num 1))
        
        
    let pow2 a b =
        iterate (fun x -> multiply x a ) (Num 1) b 
    
(* 2: Code Comprehension *)
 
    let rec foo =
        function
        | 0            -> true
        | x when x > 0 -> bar (x - 1)
        | x            -> bar (x + 1)
        
    and bar =
        function
        | 0            -> false
        | x when x > 0 -> foo (x - 1)
        | x            -> foo (x + 1)
        
    let rec baz =
        function
        | []                 -> [], []
        | x :: xs when foo x ->
            let ys, zs = baz xs
            (x::ys, zs)
        | x :: xs ->
            let ys, zs = baz xs
            (ys, x::zs)
        

(* Question 2.1: Types, names and behaviour *)

    (* 
    
    Q: What are the types of functions foo, bar, and baz?

    A: foo: int -> bool
       bar: int -> bool
       baz: int list -> int list * int list 

    Q: What do the function foo, bar, and baz do.
       Focus on what they do rather than how they do it.

    A: foo checks if a given integer is even
       bar checks if a given integer is odd
       baz sorts a list of integers into two new lists of odd and even numbers
    
    Q: What would be appropriate names for functions 
       foo, bar, and baz?

    A: foo: isEven
       bar: isOdd
       baz: splitEvenOdd
        
    *)
        

(* Question 2.2: Code snippets *)

 
    (* 
    The function baz contains the following three code snippets. 

    * A: `baz xs`
    * B: `bar x`
    * C: `(ys, x::zs)`

    Q: In the context of the baz function, i.e. assuming that `x`, `xs`, `ys`, and `zs` all have the correct types,
       what are the types of snippets A, B, and C, expressed using the F# syntax for types, and what are they -- 
       focus on what they do rather than how they do it.
    
    A: 
        a: type - int list * int list
           does - recursively processes the rest of the list, partitioning it into a list of even and odd numbers, and returns a tuple of two lists.
        b: type - bool 
           does - called inside the foo function, not baz. checks if x is odd.
        c: type - int list * int list
           does - the first list is unchanged, x is prepended to the second list. Happens when x is not even.  
    
    Q: * Explain the use of the `and`-operator that connect the `foo` and the `bar` functions.
       * Argue if the program would work if you replaced `and` with `let rec`.

    A: The and operator is used when two recursive functions call each other. It declares them together. They are mutually dependent.
       Without it, it would fail to compile, because when foo is defined, bar hasn't been defined yet. Bar however, would be able to call foo. 

    *)

(* Question 2.3: No recursion *) 

    let foo2 x = x % 2 = 0 
    let bar2 x = x % 2 = 1 

(* Question 2.4: Tail Recursion *)

    (*

    Q: The function `baz` is not tail recursive. Demonstrate why.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation. 
       You need to make clear what aspects of the evaluation tell you that the function 
       is not tail recursive. Keep in mind that all steps in an evaluation chain must 
       evaluate to the same value ( (5 + 4) * 3 --> 9 * 3 --> 27 , for instance).
       
       You do not have to step through the foo- or the bar-functions. You are allowed to evaluate 
       those function immediately.

    A: 
        baz [2;4]
        -> let ys, zs = baz [4] in (2 :: ys, zs)
        -> let ys, zs = ([4], []) in (2 :: ys,zs)
        -> ([2;4],[])  
        
        at each step, the function must wait for the result of the recursive call before it can proceed with
        list construction ( 2 :: ys, zs). This list construction happens after the recursive call finishes, meaning
        it cannot reuse the call stack. It builds up deferred operations.
                    
    
    *)
(* Question 2.5: Continuations *)

    let bazTail (lst : int list ) : int list * int list  =
        let rec aux c liste : int list * int list=
            match liste with
            | [] -> c ([],[])
            | x :: rest when foo2 x -> aux (fun (even,odd) -> c(x::even,odd)) rest 
            | x :: rest -> aux (fun (even,odd) -> c(even, x ::odd)) rest 
        aux id lst 

(* 3: Balanced brackets *)
    let explode (str : string) = [for c in str -> c]
    let implode (lst : char list) = lst |> List.toArray |> System.String
    
(* Question 3.1: Balanced brackets *)
    
    let balanced str : bool =
        let rec aux stack lst =
            match lst with
            | [] -> stack = []
            | c :: rest when c = '{'-> aux ('}' :: stack) rest
            | c :: rest when c = '(' -> aux (')' :: stack) rest
            | c :: rest when c = '[' -> aux (']' :: stack) rest
            | c :: rest ->
                    match stack with
                    | [] -> false 
                    | s :: restOfStack when s = c -> aux restOfStack rest
                    | _ -> false
                    
        str |> explode |> aux [] 
                
        
(* Question 3.2: Arbitrary delimiters *)
    
    let balanced2 (m : Map<char,char>) str : bool =
        
        let rec aux stack lst =
            match lst with
            | [] -> stack = []
            | c :: rest ->
                let asOpener =
                    if Map.containsKey c m then
                        aux (Map.find c m :: stack) rest
                    else false 
                    
                let asCloser =
                    match stack with
                    | s :: restOfStack when s = c -> aux restOfStack rest 
                    | _ -> false
                    
                asOpener || asCloser
  
        str |> explode |> aux []
        
        
    
(* Question 3.3: Matching brackets and palindromes *)    
    
    let balanced3 s : bool  =
        let m = Map[('(',')');('{','}');('[',']')]
        balanced2 m s  
    
    let symmetric (s : string ) : bool =
        let filterString = s.ToLower() |> Seq.filter System.Char.IsLetter |> Seq.toList |> implode 
        let m = s.ToLower() |> Seq.distinct |> Seq.filter System.Char.IsLetter |> Seq.map (fun c -> (c,c)) |> Map.ofSeq
        
        balanced2 m filterString  
         
        
(* Question 3.4: Parsing balanced brackets *)    
               
    open JParsec.TextParser
        
    let ParseBalanced, bref = createParserForwardedToRef<unit>()
    
    let parseBalancedAux = pstring "yo"
        
        
        
    // uncomment after you have done parseBalancedAUX
    
    // let parseBalanced = parseBalancedAux .>> pstring "**END**"
    // do bref := parseBalancedAux
            
(* Question 3.5: Parallel counting *)

    let countBalanced _ = failwith "not implemented"

(* 4: BASIC *)
    
    
    type var = string

    type expr =  
    | Num    of int              // Integer literal
    | Lookup of var              // Variable lookup
    | Plus   of expr * expr      // Addition
    | Minus  of expr * expr      // Subtraction
    
    type stmnt =
    | If of expr * uint32       // Conditional statement (if-then (no else)).
    | Let of var * expr        // Variable update/declaration
    | Goto of uint32           // Goto
    | End                      // Terminate program
      
    type prog = (uint32 * stmnt) list  // Programs are sequences of commands with their own line numbers 

    
    let (.+.) e1 e2 = Plus(e1, e2)  
    let (.-.) e1 e2 = Minus(e1, e2)  
    
    let fibProg xarg =  
        [(10u, Let("x",    Num xarg))                         // x = xarg
         (20u, Let("acc1", Num 1))                            // acc1 = 1
         (30u, Let("acc2", Num 0))                            // acc2 = 0
         
         (40u, If(Lookup "x", 60u))                           // if x > 0 then goto 60 (start loop)
         
         (50u, Goto 110u)                                     // Goto 110 (x = 0, terminate program)
         
         (60u, Let ("x", Lookup "x" .-. Num 1))               // x = x - 1
         (70u, Let ("result", Lookup "acc1"))                 // result = acc1
         (80u, Let ("acc1", Lookup "acc1" .+. Lookup "acc2")) // acc1 = acc1 + acc2
         (90u, Let ("acc2", Lookup "result"))                 // acc2 = result
         (100u, Goto 40u)                                     // Goto 40u (go to top of loop)
         
         (110u, End)                                          // Terminate program
                                                              // the variable result contains the
                                                              // fibonacci number of xarg
         ]

(* Question 4.1: Basic programs *)

    type basicProgram = Map<uint32, stmnt>
    
    let mkBasicProgram _ = failwith "not implemented"
    let getStmnt _ = failwith "not implemented"
    
    let nextLine _ = failwith "not implemented"
    
    let firstLine _ = failwith "not implemented"
    
(* Question 4.2: State *)

    type state = unit // Replace by your type type goes here
    
    let emptyState _ = failwith "not implemented"
    
    
    let goto _ = failwith "not implemented"

    let getCurrentStmnt _ = failwith "not implemented"
    
    let update _ = failwith "not implemented"
    
    let lookup _ = failwith "not implemented"
    
    
(* Question 4.3: Evaluation *)
    
    let evalExpr _ = failwith "not implemented"
    
    
    let step _ = failwith "not implemented"
  
        
    let evalProg _ = failwith "not implemented"
    
(* Question 4.4: State monad *)
    type StateMonad<'a> = SM of (basicProgram -> state -> 'a * state)  
      
    let ret x = SM (fun _ s -> (x, s))
    
    let bind f (SM a) : StateMonad<'b> =   
        SM (fun p s ->
            let x, s' = a p s
            let (SM g) = f x
            g p s')
          
    let (>>=) x f = bind f x  
    let (>>>=) x y = x >>= (fun _ -> y)  
      
    let evalSM p (SM f) = f p (emptyState p)

    let goto2 _ = failwith "not implemented"
    
    let getCurrentStmnt2 _ = failwith "not implemented"
    
    
    let lookup2 _ = failwith "not implemented"
    let update2 _ = failwith "not implemented"
    
    let step2 _ = failwith "not implemented"

(* Question 4.5: State monad evaluation *)

    type StateBuilder() =

        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = StateBuilder()

    let evalExpr2 _ = failwith "not implemented"
    
    let evalProg2 _ = failwith "not implemented"
        