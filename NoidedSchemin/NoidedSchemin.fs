module NoidedSchemin

open LispVal 

open Parser

let names = [
    "#f";
    "#t";
    "(define (factorial x)
                  (if (eq x 0) 
                    1 
                    (* x (factorial  
                           (- x 1)))))
     (print (factorial 10))
     (print (factorial 12))"
]

let testParse str = (str, test lispParser str)

let printOut (n, b) = printfn "%A parses to %A" n b 
[<EntryPoint>]
let main argv =
    List.map (test lispProgramParser >> printfn "%s") names |> ignore
    parseList lispProgramParser names |> List.map (evaluate initialEnv >> printfn "%A") |> ignore
    0