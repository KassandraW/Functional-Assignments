module Interpreter.Memory
    open Interpreter.Language
    
    module oldMemory = 
        type memory = {
            Data : Map<int,int>
            Next : int
        }

        let empty (memSize: int) =
             let newEmptyMemory = {
                 Data = Map.empty
                 Next = 0
             }
             newEmptyMemory

        let alloc size (mem: memory)=
            match size with
            | s when s <= 0 -> Error(NegativeMemoryAllocated(s))  
            | _ -> 
                let upperBound = mem.Next+size
                let rec loop current (memo: memory) =
                    match current with
                    | x when x = upperBound -> memo
                    | _ ->
                        let newMap = memo.Data.Add(current,0);
                        loop (current+1) {memo with Data = newMap; Next = current+1}
                        
                Ok(loop mem.Next mem,mem.Next)
        
        let free ptr size (mem:memory )=
            let rec check current stop = // Loop to check if the memory is there
                match current with
                | _ when current = stop -> Ok(-1)
                | _ ->
                    match mem.Data.ContainsKey current with
                    | true -> check (current+1) stop
                    | false -> Error(MemoryNotAllocated(current))
               
               
            let keyCheck = check ptr (ptr + size)
            match keyCheck with
            | Ok _ ->
                let upperBound = ptr+size
                
                let rec loop current (memo: memory) =
                    match current with
                    | x when x = upperBound -> memo
                    | _ ->
                        let newMap = memo.Data.Remove current
                        loop (current+1) {memo with Data = newMap}
                        
                Ok(loop ptr mem)
            | Error e -> Error(e)
        
        let setMem ptr v (mem:memory) =
            match mem.Data.TryFind ptr with
            | Some _ -> Ok({mem with Data = mem.Data.Add(ptr,v)})
            | None -> Error(MemoryNotAllocated(ptr))
            
        let getMem ptr mem = 
            match mem.Data.TryFind ptr with
                | Some x -> Ok(x)
                | None -> Error(MemoryNotAllocated(ptr))
        
    
    type message =
        | Alloc     of int * AsyncReplyChannel<Result<int, error>>
        | Free      of int * int * AsyncReplyChannel<Result<unit, error>>
        | SetMem    of int * int * AsyncReplyChannel<Result<unit, error>>
        | GetMem    of int * AsyncReplyChannel<Result<int, error>> 
       
    type memory = Mem of MailboxProcessor<message>
    
    
    let inbox s (i : MailboxProcessor<message>) =
        let rec messageLoop (mem : oldMemory.memory) =
            async{
                let! msg = i.Receive()
                match msg with
                | Alloc (size,reply) ->
                    match oldMemory.alloc size mem with
                    | Ok (newMem, addr) ->
                        reply.Reply(Ok addr)
                        return! messageLoop newMem
                    | Error e ->
                        reply.Reply(Error e)
                        return! messageLoop mem
                | Free (ptr,size,reply) ->
                    match oldMemory.free ptr size mem with
                    | Ok newMem ->
                        reply.Reply(Ok())
                        return! messageLoop newMem
                    | Error e ->
                        reply.Reply(Error e)
                        return! messageLoop mem
                | SetMem (ptr, v, reply) -> 
                    match oldMemory.setMem ptr v mem with
                    | Ok newMem ->
                        reply.Reply(Ok())
                        return! messageLoop newMem
                    | Error e ->
                        reply.Reply(Error e)
                        return! messageLoop mem
                | GetMem (ptr,reply )->
                    match oldMemory.getMem ptr mem with
                    | Ok (ptr) ->
                        reply.Reply(Ok ptr)
                        return! messageLoop mem 
                    | Error e ->
                        reply.Reply(Error e)
                        return! messageLoop mem
            }
        messageLoop(oldMemory.empty s)
        
    let empty s = Mem (MailboxProcessor.Start (inbox s))
        
    let free addr size (Mem mbox)= mbox.PostAndReply(fun reply -> Alloc(size,reply))
    let empty = oldMemory.empty
    let alloc = oldMemory.alloc
    let setMem = oldMemory.setMem
    let getMem = oldMemory.getMem