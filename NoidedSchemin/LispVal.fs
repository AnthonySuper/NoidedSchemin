module LispVal 

open System.Collections.Generic
open System.Drawing

type LispEnv = Map<string, LispVal> list
and LispFunc = { fn : LispEnv -> LispVal list -> LispVal }
and LispVal = 
    | LAtom of string
    | LList of LispVal list
    | LNumber of float
    | LString of string
    | LBool of bool
    | LNil
    | LLambda of LispFunc * (LispEnv ref) 
    | LFunc of LispFunc

let rec showLisp l =
    match l with
    | LAtom s -> s
    | LString s -> "\"" + s + "\""
    | LNumber n -> n.ToString()
    | LList l -> "(" + (Seq.map showLisp l |> String.concat " ") + ")"
    | LBool true -> "#t"
    | LBool false -> "#f"
    | LNil -> "Nil"
    | LFunc _ -> "(internal function)"
    | LLambda _ -> "(lambda function)"

let rec lookupVar env name =
    match env with
    | [] -> failwithf "Undefined binding %s" name 
    | x::xs -> 
        let value = Map.tryFind name x 
        match value with
        | Some x -> x
        | None -> lookupVar xs name

let extractVar a = match a with 
    | LAtom name -> name 
    | _ -> failwith "Tried to define an invalid name"

let pushEnv env p = Map.ofList p :: env

let rec lispEq a b = 
    match a with
    | LNumber n -> 
        match b with
        | LNumber n2 -> n = n2
        | _ -> false 
    | LAtom n -> 
        match b with
        | LAtom n2 -> n2 = n
        | _ -> false 
    | LString s ->
        match b with 
        | LString s2 -> s = s2
        | _ -> false
    | LList s -> 
        match b with
        | LList s2 -> 
            List.zip s s2 
            |> List.map (fun (x, y) -> lispEq x y)
            |> List.reduce (&&)
        | _ -> false 
    | LBool b_ ->
        match b with 
        | LBool b2 -> b_ = b2
        | _ -> false 
    | LNil -> 
        match b with 
        | LNil -> true 
        | _ -> false 


let rec evaluate env term =
    match term with
    | LNumber i -> LNumber i 
    | LString s -> LString s 
    | LBool b -> LBool b
    | LNil -> LNil
    | LAtom a -> lookupVar env a
    | LList [LAtom "if"; pred; good; bad] ->
        let predRes = evaluate env pred 
        match predRes with 
        | LBool true -> evaluate env good 
        | LBool false -> evaluate env bad 
        | _ -> failwith "Invalid expr, if didn't recieve true or false"
    | LList [LAtom "write"; l] -> LString (sprintf "%A" l)
    | LList (LAtom "write" :: rest) -> List.map (sprintf "%A" >> LString) rest |> LList
    | LList [LAtom "let"; LList defn; expr] -> 
        let defList = toConsList env defn
        let newEnv = pushEnv env defList
        evalBody newEnv expr
    | LList [LAtom "letrec"; LList defn; expr] ->
        let defList = toConsList env defn
        let newEnvs = pushEnv env defList 
        let newEnv = tieEnvKnot newEnvs
        evalBody newEnv expr
    | LList [(LAtom "lambda"); LList vars; body] ->
        let lambda = makeLambda vars body
        LLambda ({fn = lambda}, ref env)
    | LList (a::rest) ->
        let funVar = evaluate env a
        let applied = List.map (evaluate env) rest
        match funVar with
        | LFunc fn -> fn.fn env applied
        | LLambda (fn, boundEnv) ->
            fn.fn !boundEnv applied
        | _ -> failwithf "Attempted to apply arguments to %A which isn't a function" funVar

and evalBody env term =
    match term with
    | LList ((LList [LAtom "define"; LAtom name; defnRest]) :: rest) ->
        let evaluated = evaluate env defnRest 
        let newEnv = pushEnv env [(name, evaluated)]
        LList rest |> evaluate newEnv
    | _ -> evaluate env term
and makeLambda decl body env args =
    if List.length args <> List.length decl then
        failwithf "Lambda expected %i arguments, got %i" (List.length decl) (List.length args)
    let scope = List.zip decl args |> List.map (fun (x,y) -> extractVar x, y) |> pushEnv env 
    evaluate scope body 
and toConsList env c = 
    match c with 
    | [_] -> failwith "Malformed cons pattern has odd number of clauses"
    | [] -> []
    | a::b::rest -> (extractVar a, evaluate env b) :: toConsList env rest
and tieEnvKnot env = 
    if List.length env = 1 then []
    else
        let (head :: tail) = env
        let replaceEnv lval =
            match lval with 
            | LLambda (_, body) -> body := env
            | x -> ()
        let nhead = 
            Map.toSeq head |> Seq.iter (fun (x, y) -> replaceEnv y)
        head :: tail 

let builtinFuncs : (string * LispVal) list  = 
    let binaryMath op _ args =
        match args with 
        | [LNumber a; LNumber b] -> LNumber (op a b)
        | _ -> failwith "Expected two number arguments"
    let binaryBool op _ args =
        match args with
        | [LBool a; LBool b] -> LBool (op a b)
        | _ -> failwith "Expected two boolean arguments"
    let eq _ args = 
        match args with
        | [a; b] -> LBool (lispEq a b)
        | _ -> failwith "Expected two arguments"
    let neq env args = 
        match (eq env args) with
        | LBool true -> LBool false
        | LBool false -> LBool true
        | _ -> failwith "Not possible"
    [
        "+", binaryMath (+);
        "*", binaryMath (*);
        "/", binaryMath (/);
        "-", binaryMath (-);
        "eq", eq;
        "neq", neq;
        "&&", binaryBool (&&);
        "||", binaryBool (||);
        "and", binaryBool (&&);
        "or", binaryBool (||);
    ] |> List.map (fun (a, b) -> (a, LFunc {fn = b}))


let initialEnv = [Map.ofList builtinFuncs]