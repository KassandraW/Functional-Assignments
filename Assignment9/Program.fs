// For more information see https://aka.ms/fsharp-console-apps
open Interpreter
open Interpreter.Parser
open JParsec.TextParser
open FParsecLight.TextParser 

let rec parseArgs =
    function
    | []                -> Map.empty
    | name::value::rest -> Map.add name (System.Int32.Parse(value)) (parseArgs rest)
    | _                 -> failwith "Invalid input"


[<EntryPoint>]
let main _ =
    printfn "%A" (run pstmnt "[x + 23] := [y]") 
    0
    
    
