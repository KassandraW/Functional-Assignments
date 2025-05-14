open System
open ReExam2023

let testQ1 () =
    (* Testsfor Q1.1 *)
    printfn "Testing Question 1"
   (*
    printfn "%A" (eval p1)
    printfn "%A" (eval p2)
    printfn "%A" (eval p3)
    
    printfn "%A" (negate p1)
    printfn "%A" (negate p2)
    printfn "%A" (negate p3) 
    
    printfn "%A" (subtract p1 p2)
    printfn "%A" (subtract p2 p3)
    printfn "%A" (subtract p3 p1)
    printfn "%A" (eval (subtract p3 p1)) *)
    
    printfn "%A" (multiply p1 p1)
    printfn "%A" (multiply p2 p2)
    printfn "%A" (multiply p3 p3)
    printfn "%A" (eval(multiply p3 p3))

    ()

let testQ2 () =
    printfn "Testing Question 2"
    // place debug prints for Q2 here
    ()

let testQ3 () =
    printfn "Testing Question 3"
    // place debug prints for Q3 here
    ()

let testQ4 () =
    printfn "Testing Question 4"
    // place debug prints for Q4 here
    ()


open JParsec.TextParser

[<EntryPoint>]
let main argv =
    testQ1 ()
  
    0 // return an integer exit code
