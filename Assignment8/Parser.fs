module Interpreter.Parser

    open Interpreter.Language

    (*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)

    open JParsec.TextParser             // Example parser combinator library.
    // open FParsecLight.TextParser     // Industrial parser-combinator library. Use if performance gets bad
    

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

    let TermParse, tref = createParserForwardedToRef<aexpr>()
    let ProdParse, pref = createParserForwardedToRef<aexpr>()
    let AtomParse, aref = createParserForwardedToRef<aexpr>()
    
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
    let NegParse = unop (pchar '-') TermParse |>> (fun x -> Mul(x, Num -1) ) <?> "Negative"
    let NParse   = pint32 |>> Num <?> "Int"
    let ParParse = parenthesise TermParse
    let BrackParse = squareBrackets TermParse 
    do aref := choice [NegParse; NParse; ParParse; BrackParse]

    let paexpr = TermParse

    let pbexpr = pstring "not implemented" |>> (fun _ -> TT)

    let pstmnt = pstring "not implemented" |>> (fun _ -> Skip)
    
    let pprogram = pstmnt |>> (fun s -> (Map.empty : program), s)
    
    let run = run
       
    let runProgramParser = run (pprogram .>> eof)  
