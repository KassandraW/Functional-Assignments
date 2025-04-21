// For more information see https://aka.ms/fsharp-console-apps
open Interpreter
open Interpreter.Parser
open JParsec.TextParser
open FParsec 

let rec parseArgs =
    function
    | []                -> Map.empty
    | name::value::rest -> Map.add name (System.Int32.Parse(value)) (parseArgs rest)
    | _                 -> failwith "Invalid input"


[<EntryPoint>]
let main _ =
    (* let m = args.[2..] |> Array.toList |> parseArgs
 
    System.IO.File.ReadAllText(args.[1]) |>
    runProgramParser |>
    Result.map
        (fun (prog, body) ->
            Eval.stmntEval body |>
            StateMonad.evalState
                (State.mkState
                     (m |> Map.tryFind "--memSize" |> Option.defaultValue 0)
                     (m |> Map.tryFind "--seed")
                     prog)) |>
    printfn "\n\n%A" *)
    printfn "%A" (run paexpr "a < b ? c < d ? x : 7 : e < f ? 27 * x : 42") 
    0
    
    //"[5 / y % 0] * (a < b ? 27 : x)"
    
    
