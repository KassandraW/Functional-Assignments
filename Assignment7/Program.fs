// For more information see https://aka.ms/fsharp-console-apps

open Interpreter.Programs
open Interpreter.Eval
open Interpreter.Language
open Interpreter.State
open Interpreter.StateMonad

let [<EntryPoint>] runProgram _ =
    printfn "%A"(stmntEval (Print ([Num 7; Num 6; Div (Num 7, Num 0)], "% times % is %, which is the answer to life, the universe, and everything")) |> evalState (mkState 100 None Map.empty))
    
    0 

// Uncomment the program you want to run

//runProgram guessANumber
//runProgram bubbleSort