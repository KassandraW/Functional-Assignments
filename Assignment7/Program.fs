// For more information see https://aka.ms/fsharp-console-apps

open Interpreter.Programs
open Interpreter.Eval
open Interpreter.Language
open Interpreter.State
open Interpreter.StateMonad

let [<EntryPoint>] runProgram _ =
    printfn "%A"(evalState (getVar "x") (mkState 0 None Map.empty))
    0 

// Uncomment the program you want to run

//runProgram guessANumber
//runProgram bubbleSort