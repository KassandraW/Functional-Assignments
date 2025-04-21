module Interpreter.Parser

    open FParsec
    open Interpreter.FParsecLight.TextParser
    open Interpreter.JParsec.TextParser
    open Interpreter.Language

    (*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)

    //open JParsec.TextParser             // Example parser combinator library.
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use if performance gets bad
    

    let pif       : Parser<string> = pstring "if"
    let pelse     : Parser<string> = pstring "else"
    let palloc : Parser<string> = pstring "alloc"
    let pfree     : Parser<string> = pstring "free"
    let pwhile    : Parser<string> = pstring "while"
    let pdo       : Parser<string> = pstring "do"
    let pdeclare  : Parser<string> = pstring "declare"
    let ptrue     : Parser<string> = pstring "true"
    let pfalse    : Parser<string> = pstring "false"
    let pprint    : Parser<string> = pstring "print"
    let prandom   : Parser<string> = pstring "random"
    let pread     : Parser<string> = pstring "read"
    let pfunction : Parser<string> = pstring "function"
    let pret      : Parser<string> = pstring "ret"
    
    let pwhitespaceChar = satisfy System.Char.IsWhiteSpace
    let pletter         = satisfy System.Char.IsLetter
    let palphanumeric   = satisfy System.Char.IsLetterOrDigit

    let spaces         = many pwhitespaceChar
    let spaces1        = many1 pwhitespaceChar

    let (.>*>.) p1 p2 = (p1 .>> spaces) .>>. p2 
    let (.>*>) p1 p2  = (p1 .>> spaces) .>> p2 
    let (>*>.) p1 p2  = (p1 .>> spaces) >>. p2 

    let parenthesise p = pchar '(' >*>. p .>*> pchar ')'
    
    let squareBrackets p = pchar '[' >*>. p .>*> pchar ']'
    let curlyBrackets p = pchar '{' >*>. p .>*> pchar '}'
    
    let charListToString (lst: char list ) =
        lst |> List.toArray |> System.String
        
    let parseString =
        pchar '"' >*>. many (satisfy(fun char -> char <> '"')) .>*> pchar '"' |>> charListToString // >:[[[[
    

    let pid = pletter <|> pchar '_' .>>. many (palphanumeric <|> pchar '_')  |>> (fun (x,xs) -> x :: xs |>  charListToString)

    
    let unop op a = op >*>. a 
    let binop op a b = a .>*> op .>*>. b

    let CondParse, cref = createParserForwardedToRef<aexpr>()
    let TermParse, tref = createParserForwardedToRef<aexpr>()
    let ProdParse, pref = createParserForwardedToRef<aexpr>()
    let AtomParse, aref = createParserForwardedToRef<aexpr>()
    let BTermParse, btref = createParserForwardedToRef<bexpr>()
    let BProdParse, bpref = createParserForwardedToRef<bexpr>()
    let S1, s1ref = createParserForwardedToRef<stmnt>()
    let S2, s2ref = createParserForwardedToRef<stmnt>()
   
    
    //level 1
    //Conditional B1 ? A1 : A1
    let CondParser = BTermParse .>> spaces .>> pchar '?' .>> spaces .>>. CondParse .>> spaces .>> pchar ':' .>> spaces .>>. CondParse |>> (fun((b1,aex1),aex2) -> Cond(b1,aex1,aex2))  <?> "conditional"
    do cref := choice [CondParser;TermParse; ProdParse]
    
    //Level 2
    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let MinParse = binop (pchar '-') ProdParse TermParse |>> (fun (a,b) -> a .-. b) <?> "Subtract"
    do tref := choice [AddParse; MinParse; ProdParse]
    
    //Level 3
    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/' ) AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%' ) AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]
    
    
    //Level 4
    let NegParse = unop (pchar '-') TermParse |>> (fun x -> Mul(Num -1, x) ) <?> "Negative"
    let NParse   = pint32 |>> Num <?> "Int"
    let ParParse = parenthesise CondParse <?> "Parenthesis"
    let BrackParse = squareBrackets CondParse |>> MemRead  <?> "Read Memory"
    let ReadParse = pread |>> (fun _ -> Read) <?> "Read"
    let RandomParse = prandom |>> (fun _ -> Random) <?> "Random"
    let VParse = pid |>> (fun x -> Var x) <?> "Var" 
    
    do aref := choice [NegParse; NParse; ParParse; BrackParse; ReadParse; RandomParse; VParse]
    

    
    //B
    //Level 1
    let ConjParse = binop (pstring "/\\") BProdParse BTermParse |>> Conj <?> "Conjunction"
    let DisjParse = binop (pstring "\\/") BProdParse BTermParse |>> (fun (x,y) -> x .||. y) <?> "Disjunction"
    do btref := choice [ConjParse; DisjParse; BProdParse]
    
    //level 2
    let TrueParse = ptrue |>> fun _ -> TT
    let FalseParse = pfalse |>> fun _ -> FF
    let NotParse = unop (pchar '~') BProdParse |>> fun x -> Not x
    let EqualParse = binop (pchar '=') TermParse CondParse |>> Eq
    let InequalParse = binop (pstring "<>") TermParse CondParse |>> fun(x,y) -> x .<>. y
    let SmallerThanParse = binop (pchar '<') TermParse CondParse |>> Lt
    let SmallerOrEqualParse = binop (pstring "<=") TermParse CondParse |>> fun(x,y) -> x .<=. y 
    let GreaterThanParse = binop (pchar '>') TermParse CondParse |>> fun (x,y) -> x .>. y
    let GreaterOrEqualParse = binop (pstring ">=") TermParse CondParse |>> fun(x,y) -> x .>=. y 
    let BParParse = parenthesise BTermParse
    
    do bpref := choice [TrueParse; FalseParse; NotParse; EqualParse; InequalParse; SmallerThanParse
                        SmallerOrEqualParse; GreaterThanParse; GreaterOrEqualParse; BParParse]

    //S1
    let sequence = binop (pchar ';') S2 S1 |>> Seq <?> "Sequence"
    do s1ref := choice [sequence; S2]
   
    //S2
    let assign = pid .>*> pstring ":=" .>*>. CondParse |>> Assign
    let declare = pdeclare >>. pwhitespaceChar >>. pid |>> Declare
    let ifElse = pif >*>. parenthesise BTermParse .>*>. curlyBrackets S1 .>*> pelse .>*>. curlyBrackets S1 |>> (fun ((b1,s1),s2) -> If(b1,s1,s2))
    let sif = pif >*>. parenthesise BTermParse .>*>. curlyBrackets S1 |>> (fun(b1,s1) -> If(b1,s1,Skip))
    let swhile = pwhile >*>. parenthesise BTermParse .>*>. curlyBrackets S1 |>> While <?> "While"
    let alloc = palloc >>. parenthesise(pid .>> pchar ',' .>*>. CondParse) |>> Alloc
    let free = pfree >>. parenthesise(CondParse .>> pchar ',' .>*>. CondParse) |>> Free
    let print = pprint >>. parenthesise(parseString .>*>. many1 (pchar ',' >*>. CondParse)) |>> (fun(s,list) -> Print(list,s))
    let memwrite = squareBrackets CondParse .>*> pstring ":=" .>*>. CondParse |>> MemWrite
    
    do s2ref := choice [assign; declare; ifElse; sif; swhile; alloc; free; print; memwrite]
    let paexpr = CondParse

    let pbexpr = BTermParse

    let pstmnt = S1
    
    let pprogram = pstmnt |>> (fun s -> (Map.empty : program), s)
    
    let run = run
       
    let runProgramParser = run (pprogram .>> eof)  
