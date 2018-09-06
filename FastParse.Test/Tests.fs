module Tests

open System
open Xunit
open Xunit.Abstractions
open FastParse.Parser
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

type MyTests(output:ITestOutputHelper) =

    [<Fact>]
    member __.``simple`` () =
        let p = token_by_value "123"
        let pp =
            both p p
            <| fun a b -> {l = a.value; r = b.value}
        let tokens = def_token ["123"; "123"]
        let tokens = {arr = tokens; offset = 0}
        let value = parse pp tokens
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

        let term = token_by_name "term"
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
            let m = trans term <| fun it -> Term it.value

            either l m tokens

        let filt = Seq.filter <| fun (it: token) -> it.name <> "space"
        let tokens = filt >> Array.ofSeq <| lex None lexer_tb {filename = ""; text = "(add 1 (mul 1 2 3))"}
        let tokens = {arr = tokens; offset = 0}
        let value = parse lisp tokens
        sprintf "%A" value |> output.WriteLine
        0

    [<Fact>]
    member __.``lr`` () =
        let lexer_tb = [
            R "term"  "[^\+\s]+"
            R "space" "\s+"
            C "add_op" ["+"]
        ]

        let term = token_by_name "term"
        let plus = token_by_value "+"
        let add_expr =
            let right = both plus term <| fun _ term_token -> Term2(term_token.value)
            let lr_branch =
                fun (it: add2) tokens ->
                right tokens >>=
                function
                | (res, tokens) -> Just(Add2(it, res), tokens)
            let terminator =
                trans term <| fun term_token -> Term2(term_token.value)

            recur lr_branch terminator


        let filt = Seq.filter <| fun (it: token) -> it.name <> "space"
        let tokens = filt >> Array.ofSeq <| lex None lexer_tb {filename = ""; text = "a + b + c + d"}
        let tokens = {arr = tokens; offset = 0}
        let value = parse add_expr tokens
        sprintf "%A" value |> output.WriteLine
        0

    [<Fact>]
    member __.``pgen`` () =
        let lexer_tb = [
            R "term"  "[^\s]+"
            R "space" "\s+"
        ]

        let state = ref 1
        let int_parser i = trans <| token_by_value (sprintf "%d" i)  <| fun it -> Int32.Parse it.value
        let foo_parser = int_parser 5
        let bar_parser = int_parser 10
        let pad_parser = int_parser 120

        let parser_generator: int parser parser =
            function
            | As({value = "foo"}, tail) -> Just(foo_parser, tail)
            | As({value = "bar"}, tail) -> Just(bar_parser, tail)
            | As({value = "pad"}, tail) -> Just(pad_parser, tail)
            | _ -> Nothing

        let assert_parser (i: int) (p1: int parser): bool parser =
            fun tokens ->
            p1 tokens >>=
            function
            | (res, tokens) -> Just(res.Equals i, tokens)

        let test1 = assert_parser 5 <| pgen parser_generator
        let test2 = assert_parser 10 <| pgen parser_generator
        let test3 = assert_parser 120 <| pgen parser_generator

        let ``filter and to view`` =
              Seq.filter <| fun (it: token) -> it.name <> "space"
              >> Array.ofSeq
              >> fun it -> {arr = it; offset = 0}

        lex None lexer_tb {filename=""; text="foo 5"}
        |> ``filter and to view``
        |> parse test1
        |> fun it -> Assert.True(it, "foo test failed")

        lex None lexer_tb {filename=""; text="bar 10"}
        |> ``filter and to view``
        |> parse test2
        |> fun it -> Assert.True(it, "bar test failed")

        lex None lexer_tb {filename=""; text="pad 120"}
        |> ``filter and to view``
        |> parse test3
        |> fun it -> Assert.True(it, "pad test failed")