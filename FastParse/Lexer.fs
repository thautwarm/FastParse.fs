module FastParse.Lexer

open FastParse.Infras
open FastParse.Parser
open Helper
open System.Text.RegularExpressions


type lexer_factor =
    | RegexFactor  of Regex
    | StringFactor of string list

type string_view = {
    ref    : string
    offset : int
}

let lexer_factor_match str_view =
    function
    | RegexFactor r ->
        let r = r.Match(str_view.ref, str_view.offset)
        if r.Success then r.Value |> Some
        else None
    | StringFactor ss ->
        List.tryFind
        <| fun it ->
            StrUtils.StartsWithAt(str_view.ref, it, str_view.offset)
        <| ss

type lexer_matcher = string_view -> string option

type lexer = {
    name    : string
    factor  : lexer_factor
}

type source = {
    filename : string
    text     : string
}

type lexer_table = lexer list

type cast_map = (string, string) Map
// `cast_map` is the most difficult concept in RBNF.
// when a word `w` is extracted from the view of source text,
//if there is a `cast_map`, if `cast_map` contains the key `w`,
//a token whose value is `CachingPool.cast(w)` and named `cast_map[w]` would be yielded out.

//illustration:

//lexer_table:
//[...; {name = "identifier", matcher = matcher}, ...]

//string view:
//{
//value  = "1 match ...}
//offset = 2
//}

//matcher "match..." -> Some "match"

//cast_map:
//Map [("match", "keyword"), ...]

//yield out:
//Token(name = cast_map.["match"], value = Cachingpool.cast "match", ...)


let lex (cast_map: cast_map option)
        (lexer_table: lexer_table)
        (src: source) =
    let view = {ref = src.text; offset = 0}
    let n = src.text.Length
    let mutable lineno = 0
    let mutable colno  = 0
    let filename = src.filename
    match cast_map with
    | None ->
        let rec loop view = seq{
            match view with
            | {ref = _; offset = offset} when offset = n ->
                ()
            | _ ->
            let picked =
                let match' = lexer_factor_match view
                List.tryPick
                <| fun {name=name; factor=factor} ->
                    match match' factor with
                    | None -> None
                    | Some it -> Some (name, it)
                <| lexer_table
            match picked with
            | None ->
                let {ref = value; offset = offset;}: string_view = view
                let sample = value.Substring(offset, offset + 15)
                failwithf "unknown string head: `%s` at line %d, column %d, file %s"
                            sample lineno colno filename
            | Some (name, word) ->
                yield {
                        name = cast_const name
                        value = word
                        filename = filename
                        colno = colno
                        offset = view.offset
                        lineno = lineno
                      }
                let word_len = String.length word
                match StrUtils.StringCount(word, '\n') with
                | 0 ->
                    colno <- colno + word_len
                | line_inc ->
                    lineno <- lineno + line_inc
                    colno  <- word_len - StrUtils.StringFindIndexRight(word, '\n') - 1

                yield! loop({view with offset = view.offset + word_len})
            }
        in loop view
    | Some cast_map ->
    let rec loop view = seq{
        match view with
        | {ref = _; offset = offset} when offset = n ->
            ()
        | _ ->
        let picked =
            let match' = lexer_factor_match view
            List.tryPick
            <| fun {name=name; factor=factor} ->
                match match' factor with
                | None -> None
                | Some it -> Some (name, it)
            <| lexer_table
        match picked with
        | None ->
            let {ref = value; offset = offset} = view
            let sample = value.Substring(offset, offset + 15)
            failwithf "unknown string head: `%s` at line %d, column %d, file %s"
                        sample lineno colno filename
        | Some (name, word) ->
            let tk_name, tk_word =
                match Map.tryFind word cast_map with
                | None      -> cast_const name, word
                | Some name -> cast_const name, cast_const word
            yield  {
                    name = tk_name
                    value = tk_word
                    offset = view.offset
                    filename = filename
                    colno = colno
                    lineno = lineno
                   }
            let word_len = String.length word
            match StrUtils.StringCount(word, '\n') with
            | 0 ->
                colno <- colno + word_len
            | line_inc ->
                lineno <- lineno + line_inc
                colno  <- word_len - StrUtils.StringFindIndexRight(word, '\n') - 1
            yield! loop({view with offset = view.offset + word_len})
        }

    in loop view



let lexer_tb_to_const lexer_tb =
    [
        for each in lexer_tb ->
        match each with
        |{factor = StringFactor lst; name = name} ->
            {name = cast_const name; factor = StringFactor <| List.map cast_const lst}
        | {name = name} -> {each with name = cast_const name}
    ]

let mergeable = function
    | StringFactor _, StringFactor _ -> true
    | RegexFactor l, RegexFactor r -> l.ToString() = r.ToString()
    | _ -> false

let merge_lexer_tb (tb: lexer array) (lexer: lexer): lexer array =
    Array.tryFindIndexBack
    <| fun (it: lexer)  -> it.name = lexer.name && mergeable(it.factor, lexer.factor)
    <| tb
    |>
    function
    | None ->
        Array.append tb [|lexer|]

    | Some i ->
    let var =
        match tb.[i], lexer with
        | {name=name;factor = StringFactor ls}, {factor = StringFactor rs}->
            {name=name; factor = StringFactor <| (List.distinct <| List.append ls rs)}
        | {name = name; factor = RegexFactor _}, it -> it
        | _ -> failwith "impossible"
    tb.[i] <- var
    tb

let merge_lexer_tbs (tb1: lexer array) (tb2: lexer array): lexer array =
    Array.fold merge_lexer_tb tb1 tb2

let R name (str: string) = {name = cast_const name; factor = ("\G" + str) |> Regex |> RegexFactor}
let C name strs = {name = cast_const name; factor = List.map cast_const strs |> StringFactor}
