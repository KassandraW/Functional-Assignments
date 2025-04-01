module Interpreter.Parser

    open Interpreter.Language

    (*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)

    open JParsec.TextParser             // Example parser combinator library.
    // open FParsecLight.TextParser     // Industrial parser-combinator library. Use if performance gets bad
    

    let pif       : Parser<string> = pstring "not implemented"
    let pelse     : Parser<string> = pstring "not implemented"
    let palloc    : Parser<string> = pstring "not implemented"
    let pfree     : Parser<string> = pstring "not implemented"
    let pwhile    : Parser<string> = pstring "not implemented"
    let pdo       : Parser<string> = pstring "not implemented"
    let pdeclare  : Parser<string> = pstring "not implemented"
    let ptrue     : Parser<string> = pstring "not implemented"
    let pfalse    : Parser<string> = pstring "not implemented"
    let pprint    : Parser<string> = pstring "not implemented"
    let prandom   : Parser<string> = pstring "not implemented"
    let pread     : Parser<string> = pstring "not implemented"
    let pfunction : Parser<string> = pstring "not implemented"
    let pret      : Parser<string> = pstring "not implemented"
    
    let pwhitespaceChar = pchar '_' // not implemented
    let pletter         = pchar '_' // not implemented
    let palphanumeric   = pchar '_' // not implemented

    let spaces         = pchar '_' |>> fun x -> [x]
    let spaces1        = pchar '_' |>> fun x -> [x]

    let (.>*>.) _ _ = failwith "not implemented"
    let (.>*>) _ _  = failwith "not implemented"
    let (>*>.) _ _  = failwith "not implemented"

    let parenthesise p = p // incorrect (not implemented)
    let parseString= pstring "not implemented"

    let pid = pstring "not implemented"

    
    let unop _ = failwith "not implemented"
    let binop _ = failwith "not implemented"

    let TermParse, tref = createParserForwardedToRef<aexpr>()
    let ProdParse, pref = createParserForwardedToRef<aexpr>()
    let AtomParse, aref = createParserForwardedToRef<aexpr>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    do tref := choice [AddParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    do pref := choice [MulParse; AtomParse]

    let NParse   = pint32 |>> Num <?> "Int"
    let ParParse = parenthesise TermParse
    do aref := choice [NParse; ParParse]

    let paexpr = pstring "not implemented" |>> (fun _ -> Num 42)

    let pbexpr = pstring "not implemented" |>> (fun _ -> TT)

    let pstmnt = pstring "not implemented" |>> (fun _ -> Skip)
    
    let pprogram = pstmnt |>> (fun s -> (Map.empty : program), s)
    
    let run = run
       
    let runProgramParser = run (pprogram .>> eof)  
