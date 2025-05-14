module Exam2024

    open JParsec.TextParser
    open Option

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

    type transactions =
        | Empty
        | Pay     of string * int * transactions
        | Receive of string * int * transactions
        
    let rec balance (trs : transactions) : int  =
        match trs with
        | Empty -> 0
        | Pay(_,a,trsList) -> balance trsList - a 
        | Receive(_,a,trsList) -> balance trsList + a 
        
    let balanceAcc (trs : transactions) : int =
        let rec aux acc tra = 
            match tra with
            | Empty -> acc
            | Pay(_,a,trsList) -> aux (acc - a) trsList
            | Receive(_,a,trsList) -> aux (acc + a) trsList
        aux 0 trs 
        
    let participants (trs : transactions) : Set<string> * Set<string> =
        let rec aux (s1,s2) tra = 
            match tra with
            | Empty -> (s1,s2)
            | Pay(n,_,trsList) -> aux (Set.add n s1,s2) trsList
            | Receive(n,_,trsList) -> aux (s1,Set.add n s2) trsList
        aux (Set.empty,Set.empty) trs
        
    
    let rec balanceFold (payFolder: 'a -> string -> int -> 'a) (receiveFolder :'a -> string -> int -> 'a) acc trs: 'a=
        match trs with
        | Empty -> acc
        | Pay(name,amount, trsList) -> balanceFold payFolder receiveFolder (payFolder acc name amount) trsList
        | Receive(name,amount,trsList) -> balanceFold payFolder receiveFolder (receiveFolder acc name amount) trsList
    
    let balance' trs = balanceFold (fun acc _ x -> acc - x) (fun acc _ x -> acc + x) 0 trs
    
    let collect (trs : transactions) : Map<string,int> =
        let payFolder acc name amount =
            Map.change name (function
                             | Some v -> Some (v - amount)
                             | None -> Some (-amount)) acc
            
        let receiveFolder acc name amount =
            Map.change name (function
                            | Some v -> Some (v + amount)
                            | None -> Some amount) acc
                 
        balanceFold payFolder receiveFolder Map.empty trs
        
    
    
(* 2: Code Comprehension *)
        
    let foo (x : char) = x |> int |> fun y -> y - (int '0')
    
    let bar (x : string) = [for c in x -> c]
            
    let rec baz =
        function
        | [] -> 0
        | x :: xs -> x + 10 * baz xs
    
(* Question 2.1 *)

    (* 
    
    Q: What are the types of functions foo, bar, and baz?

    A: foo: char -> int
       bar: string -> char list
       baz: int list -> int 

    Q: What do the function foo, bar, and baz do.

    A: foo converts a digit character into its corresponding integer. 
       bar converts a string into a list of characters
       baz converts a list of digits to the corresponding integer value in reverse.
    
    Q: What would be appropriate names for functions 
       foo, bar, and baz?

    A: foo: digitToInt
       bar: charToString
       baz: reversedDigitsToInt
    
    Q: The function foo only behaves reasonably if certain 
       constraint(s) are met on its argument. 
       What is/are these constraints?
        
    A: The argument should be constrained to digits only.
    
    Q: The function baz only behaves reasonably if certain 
       constraint(s) are met on its argument. 
       What is/are these constraints?
        
    A: The integers should be in the range 0-9 *)
    
(* Question 2.2 *)
    
    let stringToInt (s : string ) : int =
        s |> bar |> List.map foo |> List.rev |> baz
        

(* Question 2.3 *)
    
    let baz2 lst : int  =
        List.fold (fun acc x -> x + 10 * acc) 0 (List.rev lst) 
        
    
(* Question 2.4 *)

    (*

    Q: The function `bar` from Question 2.1 is not tail recursive. Demonstrate why.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation. 
       You need to make clear what aspects of the evaluation tell you that the function 
       is not tail recursive. Keep in mind that all steps in an evaluation chain must 
       evaluate to the same value ( (5 + 4) * 3 --> 9 * 3 --> 27 , for instance).
       
       You do not have to step through the foo-function. You are allowed to evaluate 
       that function immediately.

    A: <Your answer goes here>

    *)
    
(* Question 2.5 *)

    let bazTail lst: int =
        let rec aux c lst =
            match lst with
            | [] -> c 0
            | x :: xs -> aux (fun result -> c(x + 10 * result)) xs
        aux id lst 
   
        
(* 3: Caesar Ciphers *)

(* Question 3.1 *)
    
    let encrypt (text : string) (offset : int) : string =
        let alphabet = [|'a'..'z'|]
        let length = alphabet.Length 
        
        let offsetCharacter (c:char) : char =
            match c with
            | ' ' -> c
            | _ ->
                let newIndex = ((Array.findIndex (fun ch -> ch = c) alphabet) + offset) % length // 7 + 3 = 10 -> 10 % 26
                alphabet[newIndex]
                    
        text |> Seq.map offsetCharacter |> Seq.toArray |> System.String
                    
                
    
(* Question 3.2 *)
    let decrypt (text:string) offset =
        let alphabet = [|'a'..'z'|]
        let length = alphabet.Length
        
        let offsetCharacter (c:char) : char =
            match c with
            | ' ' -> c
            | _ ->
                let newIndex = ((Array.findIndex (fun ch -> ch = c) alphabet) - offset) % length
                let wrappedIndex = if newIndex  < 0 then newIndex + length else newIndex
                alphabet[wrappedIndex]
                    
        text |> Seq.map offsetCharacter |> Seq.toArray |> System.String
    
(* Question 3.3 *)
    let decode (plainText : string) (encryptedText : string) : int option =
        if plainText.Length <> encryptedText.Length then None
        else 
            let alphabet = [|'a'..'z'|]
            let plainIndex = Array.findIndex ((=) plainText[0]) alphabet
            let encryptedIndex = Array.findIndex ((=)encryptedText[0]) alphabet
            let offset = (encryptedIndex - plainIndex + 26) % 26 
            
            if System.String.Equals(encrypt plainText offset, encryptedText ) then Some offset else None 
        
         
    
(* Question 3.4 *)
    let parEncrypt (text:string) offset =
        let splitText = text.Split ' '
        let encryptAsync s : Async<string> =
            async{
                return encrypt s offset 
            }
            
        let encryptedStrings =
            splitText  
            |> Array.map encryptAsync 
            |> Async.Parallel
            |> Async.RunSynchronously 
        
        String.concat " " encryptedStrings
    
(* Question 3.5 *)
        
    open JParsec.TextParser
    
    let parseEncrypt offset : Parser<string> =
        let charParser = satisfy (fun c -> System.Char.IsLetter c || System.Char.IsWhiteSpace c)
        many charParser  |>> (fun x -> encrypt (System.String (List.toArray x)) offset)
        

(* 4: Letterboxes *)
    
(* Question 4.1 *)
    
    type letterbox = Map<string, string list>   
    
    let empty() : letterbox  =
        Map.empty 
        

(* Question 4.2 *)
    let post sender message (mb:letterbox)  : letterbox =
        let updateMessages = match mb.TryFind sender with
                                | Some messages -> messages @ [message] 
                                | None -> [message]
        
        Map.add sender updateMessages mb 
                
    let read sender (mb:letterbox) : Option<string*letterbox> =
        match mb.TryFind sender with
        | Some (x :: xs) ->
            let updatedMB =
                if xs.IsEmpty then mb.Remove sender
                else mb.Add(sender,xs)
            Some(x, updatedMB)
        | _ -> None  
            
        
 

    
(* Question 4.3 *)
    type StateMonad<'a> = SM of (letterbox -> ('a * letterbox) option)  
      
    let ret x = SM (fun s -> Some (x, s))  
    let fail  = SM (fun _ -> None)  
    let bind f (SM a) : StateMonad<'b> =   
        SM (fun s ->   
            match a s with   
            | Some (x, s') ->  let (SM g) = f x               
                               g s'  
            | None -> None)
          
    let (>>=) x f = bind f x  
    let (>>>=) x y = x >>= (fun _ -> y)  
      
    let evalSM (SM f) = f (empty ())
    
    let post2 sender message : StateMonad<unit>=
        SM (fun lb -> (Some ((), post sender message lb)) )
    let read2 sender  : StateMonad<string> =
        SM (fun lb -> (read sender lb))
                

(* Question 4.4 *)

    type StateBuilder() =

        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = StateBuilder()

    type MType =
        | Post of string * string
        | Read of string
    type log = MType list
    
    let trace (l : log) : StateMonad<string list> =
        let rec loop acc log =
            state {
                match log with
                | [] -> return List.rev acc 
                | Post(s,m) :: rest ->
                    do! post2 s m
                    return! loop acc rest
                | Read(s) :: rest ->
                    let! msg = read2 s
                    return! loop (msg :: acc) rest 
        }
        loop [] l 
