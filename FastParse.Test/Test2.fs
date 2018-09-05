module Tests2

open System
open Xunit
open Xunit.Abstractions
open FastParse.LRParser
open FastParse.Lexer
open FastParse.Infras


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

type add2 =
| Term2 of string
| Add2 of add2 * add2

type MyTests2(output:ITestOutputHelper) =

    [<Fact>]
    member __.``simple`` () =
        let session = Session()
        let p = session.token_by_value "123"
        let pp =
            session.both p p
            <| fun a b -> {l = a.value; r = b.value}
        let tokens = def_token ["123"; "123"]
        let value = parse pp <| session.make_state tokens
        sprintf "%A" value |> output.WriteLine
        0
    
    [<Fact>]
    member __.``lisp`` () =
        let session = Session()
        let lexer_tb = [
            R "term"  "[^\(\)\s]+"
            R "space" "\s+"
            C "unname" ["("]
            C "unname" [")"]
        ]

        let term = session.token_by_name "term"
        let l = session.token_by_value "("
        let r = session.token_by_value ")"
        let rec lisp  =
            let l =
                session.make_parser 
                <|
                fun state ->

                    let many = session.rep lisp 0 -1 <| fun lst -> lst
                    let l = session.both l many
                            <| fun _ lst -> lst
                    let r = session.both l r
                            <| fun lst _ -> S lst
                    r <*> state >>=
                    function
                    | (a, b) -> Success(a, b)

            let m = session.trans term <| fun it -> Term it.value
            session.either l m


        let filt = Seq.filter <| fun (it: token) -> it.name <> "space"
        let tokens = filt >> Array.ofSeq <| lex None lexer_tb {filename = ""; text = "(add 1 (mul 1 2 3))"}
        let value = parse lisp <| session.make_state tokens
        sprintf "%A" value |> output.WriteLine
        0

    [<Fact>]
    member __.``lr`` () =
        let session = Session()
        let lexer_tb = [
            R "term"  "[^\+\s]+"
            R "space" "\s+"
            C "add_op" ["+"]
        ]

        let term = session.token_by_name "term"
        let plus = session.token_by_value "+"

        let right = session.both plus term <| fun _ term_token -> Term2(term_token.value)
        let terminator =
                session.trans term <| fun term_token -> Term2(term_token.value)
        let rec add_expr =
            
            let recur =
                session.make_parser
                <|
                fun state -> 
                    add_expr <*> state >>=
                    fun (a, b) -> Success(a, b)

            session.either 
            <| (session.both recur right <| fun a b -> Add2(a, b))
            <| terminator



        let filt = Seq.filter <| fun (it: token) -> it.name <> "space"
        let tokens = filt >> Array.ofSeq <| lex None lexer_tb {filename = ""; text = "a + b + c + d"}
        let value = parse add_expr <| session.make_state tokens
        sprintf "%A" value |> output.WriteLine
        0