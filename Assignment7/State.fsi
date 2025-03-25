module Interpreter.State

open Interpreter.Language

type state
val mkState : int -> int option -> state
val declare : string -> state -> Result<state,error>
val getVar : string -> state -> Result<int,error>
val setVar : string -> int -> state -> Result<state,error>
val alloc : string -> int -> state -> Result<state,error>
val free : int -> int -> state -> Result<state,error>
val getMem : int -> state -> Result<int,error>
val setMem : int -> int -> state -> Result<state,error>
val random : state -> int 

