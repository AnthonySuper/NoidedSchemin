module NoidedSchemin

open LispVal 

open Parser

let names = [
    "#f";
    "#t";
    "(letrec (factorial (lambda (x) 
                  (if (eq x 0) 
                    1 
                    (* x (factorial  
                           (- x 1))))))
                (factorial 50))
"
]

let testParse str = (str, test lispParser str)

let printOut (n, b) = printfn "%A parses to %A" n b 
[<EntryPoint>]
let main argv =
    List.map (test lispParser >> printfn "%s") names |> ignore
    parseList lispParser names |> List.map (evaluate initialEnv >> printfn "%A") |> ignore
    0