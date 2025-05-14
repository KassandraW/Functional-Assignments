open System
open Exam2024

let testQ1 () =
    (* Testsfor Q1.1 *)
    printfn "Testing Question 1"
    
    printfn "%A" (balance Empty)
    printfn "%A" (balance (Pay("Alice", 500, Receive("Bob", 200, Empty))))
    

    ()

let testQ2 () =
    printfn "%A" ([Post("Alice", "Hello John!"); Post("Bob", "John, how are you?");
    Post("Alice", "Good bye John.");
    Read("Bob"); Read("Alice"); Read("Alice")] |>
    trace |> evalSM |> Option.map fst)
    
    printfn "%A" ([Post("Alice", "Hello John!"); Post("Bob", "John, how are you?");
    Post("Alice", "Good bye John.");
    Read("Charlie"); Read("Bob"); Read("Alice"); Read("Alice")] |>
    trace |> evalSM |> Option.map fst)
    ()

let testQ3 () =
    printfn "Testing Question 3"
    // place debug prints for Q3 here
    ()

let testQ4 () =
    printfn "Testing Question 4"
    // place debug prints for Q4 here
    ()

[<EntryPoint>]
let main argv =
    testQ2 ()
    0 // return an integer exit code
