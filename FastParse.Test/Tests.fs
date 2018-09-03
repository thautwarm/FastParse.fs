module Tests

open System
open Xunit
open Xunit.Abstractions
open FastParse.Parser
open FastParse.Lexer


type Add = {
    l: string
    r: string
}

type sexpr = 
    | Term of string
    | S    of sexpr list

let def_token str_lst =
    str_lst
    |> Array.ofList
    |> Array.map
       (fun str ->
            {filename = ""; value = str ; name = "const" ; colno = 1; lineno = 1; offset = 1;})

type MyTests2(output:ITestOutputHelper) =
    
    [<Fact>]
    member __.``simple`` () =
        let p = token_by_value "123"
        let pp =
            both p p 
            <| fun a b -> {l = a.value; r = b.value}
        let tokens = def_token ["123"; "123"]
        let tokens = {arr = tokens; offset = 0}
        let value = parse tokens pp
        sprintf "%A" value |> output.WriteLine
        0
    
    [<Fact>]
    member __.``lisp`` () =
        let lexer_tb = [
            R "term"  "[^\(\)\s]+"
            R "space" "\s+"
            C "unname" ["("]
            C "unname" [")"]
        ]

        let term = FastParse.Parser.token_by_name "term"
        let l = token_by_value "("
        let r = token_by_value ")"
        let rec lisp tokens  =
            let l = 
                fun tokens ->
                    let many = rep lisp 0 -1 <| fun lst -> lst
                    let l = both l many 
                            <| fun _ lst -> lst
                    let r = both l r 
                            <| fun lst _ -> S lst
                    r tokens
            let m = map term <| fun it -> Term it.value
            
            either l m tokens 

        let filt = Seq.filter <| fun (it: token) -> it.name <> "space"
        let tokens = filt >> Array.ofSeq <| lex None lexer_tb {filename = ""; text = "(add 1 (mul 1 2 3))"}
        let tokens = {arr = tokens; offset = 0}
        let value = parse tokens lisp
        sprintf "%A" value |> output.WriteLine
        0