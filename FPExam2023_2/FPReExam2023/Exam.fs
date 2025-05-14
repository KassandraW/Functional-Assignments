module ReExam2023

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
    let eval _ = failwith "not implemented"
    
(* Question 1.2: Negation and subtraction *)
    let negate _ = failwith "not implemented"
        
    let subtract _ = failwith "not implemented"

(* Question 1.3: Multiplication *)
        
    let multiply _ = failwith "not implemented"
    
(* Question 1.4: Exponents *)

    let pow _ = failwith "not implemented"
    
(* Question 1.5: Iteration *)

    let iterate _ = failwith "not implemented"
        
    let pow2 _ = failwith "not implemented"
    
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

    A: <Your answer goes here>


    Q: What do the function foo, bar, and baz do.
       Focus on what they do rather than how they do it.

    A: <Your answer goes here>
    
    Q: What would be appropriate names for functions 
       foo, bar, and baz?

    A: <Your answer goes here>
        
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
    
    A: <Your answer goes here>
    
    Q: * Explain the use of the `and`-operator that connect the `foo` and the `bar` functions.
       * Argue if the program would work if you replaced `and` with `let rec`.

    A: <Your answer goes here>

    *)

(* Question 2.3: No recursion *) 

    let foo2 _ = failwith "not implemented"
    let bar2 _ = failwith "not implemented"

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

    A: <Your answer goes here>
    
    *)
(* Question 2.5: Continuations *)

    let bazTail _ = failwith "not implemented"

(* 3: Balanced brackets *)

      
    let explode (str : string) = [for c in str -> c]
    let implode (lst : char list) = lst |> List.toArray |> System.String
    
(* Question 3.1: Balanced brackets *)
    
    let balanced _ = failwith "not implemented"
        
(* Question 3.2: Arbitrary delimiters *)
    
    let balanced2 _ = failwith "not implemented"
    
(* Question 3.3: Matching brackets and palindromes *)    
    
    let balanced3 _ = failwith "not implemented" 
    
    let symmetric _ = failwith "not implemented"
        
(* Question 3.4: Parsing balanced brackets *)    
               
    open JParsec.TextParser
    
        
    let ParseBalanced, bref = createParserForwardedToRef<unit>()
    
    let parseBalancedAux = pstring "Your parser goes here"
        
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
        