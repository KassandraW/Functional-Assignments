module Exam2024

open System
    
(* If you are importing this into F# interactive then comment out
   the line above and remove the comment for the line bellow.

   Do note that the project will not compile if you do this, but 
   it does allow you to work in interactive mode and you can just remove the '=' 
   to make the project compile again.

   You will also need to load JParsec.fs. Do this by typing
   #load "JParsec.fs" 
   in the interactive environment. You may need the entire path.

   Do not remove the module declaration (even though that does work) because you may inadvertently
   introduce indentation errors in your code that may be hard to find when switching back to project mode.

   Alternative, keep the module declaration as is, but load ExamInteractive.fsx into the interactive environment
   *)
(*
 module Exam2024 = 
 *)

(* 1: Transactions *)
    
    type shape =
        | Rectangle of float * float
        | Circle of float
        | Triangle of float * float
        
    type shapeList =
        | Empty
        | AddShape of shape * shape * shapeList
        
    let area (s : shape) : float  =
        match s with
        | Rectangle(w,h) -> w * h 
        | Circle(r) -> Math.PI * r * r
        | Triangle(b,h) ->  (b*h)/2.0 
    
    let circumference (s : shape) : float =
        match s with
        | Rectangle(w,h) -> 2.0 * w + 2.0 * h 
        | Circle(r) -> Math.PI * r * 2.0
        | Triangle(b,h) ->  b + h + Math.Sqrt(b*b + h*h)
        
    let rec totalArea (sl : shapeList) : float =
        match sl with
        | Empty -> 0.0
        | AddShape(s1,s2,shapeList) -> area s1 + area s2 + totalArea(shapeList)
        
    let totalCircumference (sl : shapeList) : float =
        let rec aux acc sl2 : float =
            match sl2 with
            | Empty -> acc
            | AddShape(s1,s2,shapeList) -> aux (acc + circumference s1 + circumference s2) shapeList
        
        aux 0.0 sl 
        
    let rec shapeListFold  (f : 'a -> shape -> 'a) (acc : 'a) (sl : shapeList) =
        match sl with
        | Empty -> acc
        | AddShape(s1,s2,rest) -> shapeListFold f (f (f acc s1 ) s2) rest 
       
        

    let isCircle =
        function
        | Circle _ -> true
        | _        -> false

    let containsCircle trs = 
        shapeListFold (fun acc c -> acc || isCircle c) false trs
        
    let totalArea2 (sl : shapeList) : float =
        shapeListFold (fun acc shape -> acc + area shape) 0.0 sl

    let totalCircumference2 (sl : shapeList) : float =
        shapeListFold (fun acc shape -> acc + circumference shape) 0.0 sl 
    
(* 2: Code Comprehension *)
        
    let foo =
        function
        | c when Char.IsWhiteSpace c -> c 
        | c when c > 'w'             -> char (int c - 23)
        | c when c < 'x'             -> char (int c + 3)
        
    
        
        
    let bar (str : string) = [for c in str -> c]
    
    let baz str =
        let rec aux = 
            function
            | [] -> ""
            | c :: cs -> string (foo c) + (aux cs)
            
        aux (bar str)
        
    
(* Question 2.1 *)

    (* 
    
    Q: What are the types of functions foo, bar, and baz?

    A: foo: char -> char
       bar: string -> char list
       baz: string -> string 

    Q: What do the function foo, bar, and baz do.
       Focus on what they do rather than how they do it.

    A: bar turns a given string into a character list. foo encrypts a character by offsetting it with 3.
        baz takes a given string and encrypts it.
    
    Q: What would be appropriate names for functions 
       foo, bar, and baz?

    A: Bar = stringToCharList
       foo = encryptChar
       baz = encryptString
        
*)
    
(* Question 2.2 *)
    let foo2 =
        function
        | c when Char.IsWhiteSpace c -> c 
        | c when c > 'w'             -> char (int c - 23)
        | c when c < 'x'             -> char (int c + 3)
        | c -> c 

(* Question 2.3 *)
   
    let baz2 str = (bar str) |> List.fold (fun acc r -> string (foo r) + acc) ""
    
(* Question 2.4 *)

    (*

    Q: The function `baz` from Question 2.1 is not tail recursive. Demonstrate why.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation. 
       You need to make clear what aspects of the evaluation tell you that the function 
       is not tail recursive. Keep in mind that all steps in an evaluation chain must 
       evaluate to the same value ( (5 + 4) * 3 --> 9 * 3 --> 27 , for instance).
       
       You do not have to step through the foo- or the bar functions. 
       You are allowed to evaluate these function immediately.
      
    
       
    A:  Evaluation: 
        ~> aux ['h'; 'e'; 'j'] 
        ~> "h" + aux ['e';'j']
        ~> "h" + ("e" +  aux ['j'])
        ~> "h" + ("e" +  ("j" + aux []))
        ~> "h" + ("e" +  ("j" + ("")))
        ~> "h" + ("e" +  ("j"))
        ~> "h" + ("ej")
        ~> "hej"
        
        its not a tail recursion because it doesn't use an acculumator or continuation.
        It does not compute the result as it goes, which means that when it is done doing its recursive calls, it has yet to compute everything.
        so after ~> "h" + ("e" +  ("j" + aux [])) it has to step back towards every function and compute it. 

    *)
    
(* Question 2.5 *)
    
    let bazTail str  : string =
        let rec aux rest c =
            match rest with
            | [] -> c ""
            | x :: xs -> aux xs (fun result -> c(string(foo x) + result))
            
        aux (bar str) id 
       

(* 3: Atbash Ciphers *)

(* Question 3.1 *)
    
    let encrypt (text : string) : string  =
        let convert_char c = if Char.IsWhiteSpace c then c else 'z' - c + 'a'
        String.map convert_char text 
        (*
        let toCharList (str : string) = [for c in str -> c]
        let alphabet = ['a' .. 'z']
        let revAlphabet = List.rev alphabet
        
        let helper (c : char ) =
         match c with
         | ' ' -> c
         | c ->
             let index = alphabet |> List.findIndex (fun x -> x = c) 
             revAlphabet[index]
         
        List.fold (fun acc r -> acc + string (helper r) ) "" (toCharList text) *)
  
    
(* Question 3.2 *)

    let decrypt (text : string ) : string  =
        encrypt text 
    
(* Question 3.3 *)

    let splitAt (i : int) (str : string) : string list  =
        let rec helper c (s:string) : string list  =
            match s with
            | "" -> c [s]
            | _ -> 
                    let substring = s[0 .. i - 1]
                    let rest = s[i ..]
                    helper (fun r -> c (substring :: r)) rest       
        helper id str 
        

    
(* Question 3.4 *)
    let parEncrypt (str: string ) (i : int) : string =
        let encryptAsync s : Async<string> =
            async{
                return encrypt s
            }
            
        let encryptedStrings =
            splitAt i str 
            |> List.map encryptAsync 
            |> Async.Parallel//fordel til elever
            |> Async.RunSynchronously // sig START
        
        String.concat "" encryptedStrings //saml alle eksamenssættene
    
(* Question 3.5 *)
        
    open JParsec.TextParser
    let pletter = satisfy Char.IsLetter
    let charListToString (lst: char list ) =
        lst |> List.toArray |> System.String

    let parseEncrypt : Parser<String> =
        let charParser = satisfy (fun c -> Char.IsLetter c || Char.IsWhiteSpace c)
        many charParser  |>> (fun x -> List.toArray x |> String  |> encrypt)
        

(* 4: Letterboxes *)
    
(* Question 4.1 *)
    
    type clicker =
        char list // insert your own type here
    
    let newClicker (lst : char list) (numWheels : int ) : clicker = failwith "not implemented"

(* Question 4.2 *)
    
    let click _ = failwith "not implemented"
        
    let read _ = failwith "not implemented"

    
(* Question 4.3 *)
    type StateMonad<'a> = SM of (clicker -> 'a * clicker)  
      
    let ret x = SM (fun cl -> (x, cl))
    
    let bind f (SM a) : StateMonad<'b> =
        SM (fun cl ->
               let x, cl'  = a cl
               let (SM g) = f x
               g cl')
          
    let (>>=) x f = bind f x  
    let (>>>=) x y = x >>= (fun _ -> y)  
      
    let evalSM cl (SM f) = f cl
    
    let click2 _ = failwith "not implemented"
    
    let read2 _ = failwith "not implemented"

(* Question 4.4 *)
    
    let multipleClicks _ = failwith "not implemented"
        
(* Question 4.5 *)

    type StateBuilder() =

        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = StateBuilder()
    
    let multipleClicks2 _ = failwith "not implemented"