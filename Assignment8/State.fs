module Interpreter.State

    open Result
    open Language
    open Memory
    
    let validVariableName (v:string) =
        System.Char.IsAsciiLetter(v[0]) || v[0] = '_' && String.forall (fun x -> System.Char.IsAsciiLetterOrDigit x || x = '_') v
        
    let reservedVariableName (v:string) =
        let matches = ["if";"then";"else";"while";"declare";"print";"random";"fork";"__result__"]
        matches |> List.exists (fun x -> x = v)
    
    type state ={
        Data : Map<string,int>
        Memory : memory
        RNG : System.Random
    }
    
    let mkState (memSize:int) (oseed : int option) (prog: program) : state = {
        Data = Map.empty
        Memory = empty memSize
        RNG =
            match oseed with
            | Some x -> System.Random(x)
            | _ -> System.Random()
    }
    let random (st:state) : int =
        st.RNG.Next()
    
    let declare (x:string) (st: state) =
        if (st.Data.ContainsKey x)
        then
            Error(VarAlreadyExists(x))
        else if not (validVariableName x)
        then
            Error(InvalidVarName x)
        else if reservedVariableName x
        then
            Error(ReservedName(x))
        else
            Ok({ st with Data = st.Data.Add(x,0)})
    
    let getVar x (st:state) =
        let bool,value = st.Data.TryGetValue x
        if  bool
        then Ok(value)
        else Error(VarNotDeclared x)
        
    let setVar x v st=
       let bool = st.Data.ContainsKey x
       if bool
       then Ok {st with Data = st.Data.Add(x,v)}
       else Error(VarNotDeclared x)
    
    let alloc x size st =
        let memIntOpt = alloc size st.Memory
        match memIntOpt with
        | Ok (memory,startAddress) ->
            let pointer = setVar x startAddress st
            match pointer with
            | Ok st' -> Ok{st' with Memory = memory ; Data = st'.Data}
            | Error e -> Error e 
        | Error e -> Error e
        
    let free ptr size st =
        let f = free ptr size st.Memory
        match f with
        | Ok s -> Ok {st with Memory = s}
        | Error e -> Error(e)
        
    let getMem ptr st  =
        getMem ptr st.Memory
        
    let setMem ptr v st =
        let f = setMem ptr v st.Memory
        match f with
        | Ok s -> Ok {st with Memory = s}
        | Error e -> Error(e)
    
    let push _ = failwith "not implemented"
    let pop _ = failwith "not implemented"
    
    let pushFrame _ = failwith "not implementd"
    let popFrame _ = failwith "not implemented"