module Parser

open FParsec

open FParsec.Primitives

open FParsec.CharParsers

open LispVal
open System

type UserState = unit // doesn't have to be unit, of course
type Parser<'t> = Parser<'t, UserState>

let numberParser : Parser<_> = pfloat |>> LNumber

let trueParser : Parser<_> = pstring "#t" >>% LBool true

let falseParser : Parser<_> = pstring "#f" >>% LBool false

let boolParser : Parser<_> = trueParser <|> falseParser

let nilParser : Parser<_> = pstring "Nil" >>% LNil

let atomIdentOptions = 
    let isStart c = 
        isAsciiLetter c || c = '_' || c = '-' ||
        c = '+' || c = '-' || c = '*' || c = '/'
    let isContinue c = isAsciiLetter c || isDigit c || c = '_' || c = '-'
    IdentifierOptions(isAsciiIdStart = isStart,
                      isAsciiIdContinue = isContinue)
                  
let atomParser : Parser<_> = identifier(atomIdentOptions) |>> LAtom

let str s = pstring s

let stringLiteral =
    let escape =  anyOf "\"\\/bfnrt"
                  |>> function
                      | 'b' -> "\b"
                      | 'f' -> "\u000C"
                      | 'n' -> "\n"
                      | 'r' -> "\r"
                      | 't' -> "\t"
                      | c   -> string c // every other char is mapped to itself

    let unicodeEscape =
        let hex2int c = (int c &&& 15) + (int c >>> 6)*9

        str "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
            (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
            |> char |> string
        )

    let escapedCharSnippet = str "\\" >>. (escape <|> unicodeEscape)
    let normalCharSnippet  = manySatisfy (fun c -> c <> '"' && c <> '\\')

    between (str "\"") (str "\"")
            (stringsSepBy normalCharSnippet escapedCharSnippet)

let stringParser : Parser<_> = stringLiteral |>> LString

let lispValue, lispValueRef = createParserForwardedToRef<LispVal, unit>();

let listParser : Parser<_> = 
    let listOuter = (sepBy lispValue spaces1) |>> LList
    between (pchar '(') (pchar ')') listOuter

let quoteParser = pchar ''' >>. lispValue |>> fun x -> LList [LAtom "quote"; x]

let lispParser : Parser<_> = 
    stringParser <|> atomParser <|> numberParser <|> boolParser <|> nilParser <|> quoteParser <|> listParser

do lispValueRef := lispParser 


let lispProgramParser : Parser<_> =
    sepBy lispParser spaces1 |>> fun x -> LList (LAtom "begin" :: x)

let test p str =
    match run p str with
    | Success(result, _, _)   -> sprintf "Success: %A" result
    | Failure(errorMsg, _, _) -> sprintf "Failure: %s" errorMsg

let parseList parser strings =
    let rec recFilter l = 
        match l with
        | (Success(result, _, _))::xs -> result :: recFilter xs
        | _::xs -> recFilter xs
        | [] -> []
    List.map (run parser) strings |> recFilter
