module Interpreter.Parser

    open FParsec
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
    let ParParse = parenthesise CondParse
    let BrackParse = squareBrackets CondParse |>> MemRead 
    let ReadParse = pread |>> fun _ -> Read
    let RandomParse = prandom |>> fun _ -> Random
    let VParse = pid |>> fun x -> Var x 
    
    do aref := choice [NegParse; NParse; ParParse; BrackParse; ReadParse; RandomParse; VParse]
    

    
    //B
    //Level 1
    let ConjParse = binop (pstring "/\\") BProdParse BTermParse |>> Conj
    let DisjParse = binop (pstring "\\/") BProdParse BTermParse |>> fun (x,y) -> Not(Conj(Not x, Not y))
    do btref := choice [ConjParse; DisjParse; BProdParse]
    
    //level 2
    let TrueParse = ptrue |>> fun _ -> TT
    let FalseParse = pfalse |>> fun _ -> Not TT
    let NotParse = unop (pchar '~') BProdParse |>> fun x -> Not x
    let EqualParse = binop (pchar '=') TermParse CondParse |>> Eq
    let InequalParse = binop (pstring "<>") TermParse CondParse |>> fun(x,y) -> Not(Eq(x,y))
    let SmallerThanParse = binop (pchar '<') TermParse CondParse |>> Lt
    let SmallerOrEqualParse = binop (pstring "<=") TermParse CondParse |>> fun(x,y) -> x .<=. y 
    let GreaterThanParse = binop (pchar '>') TermParse CondParse |>> fun (x,y) -> x .>. y
    let GreaterOrEqualParse = binop (pstring ">=") TermParse CondParse |>> fun(x,y) -> x .>=. y 
    let BParParse = parenthesise BTermParse
    
    do bpref := choice [TrueParse; FalseParse; NotParse; EqualParse; InequalParse; SmallerThanParse
                        SmallerOrEqualParse; GreaterThanParse; GreaterOrEqualParse; BParParse]

    let paexpr = CondParse

    let pbexpr = BTermParse

    let pstmnt = pstring "not implemented" |>> (fun _ -> Skip)
    
    let pprogram = pstmnt |>> (fun s -> (Map.empty : program), s)
    
    let run = run
       
    let runProgramParser = run (pprogram .>> eof)  
