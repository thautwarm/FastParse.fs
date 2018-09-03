```F#
    open FastParse.Parser
    open FastParse.Lexer

    type sexpr = 
    | Term of string
    | S    of sexpr list

    (** auto lexer *)
    let lexer_tb = [
        R "term"  "[^\(\)\s]+"
        R "space" "\s+"
        C "unname" ["("]
        C "unname" [")"]
    ]
    let term : token parser = token_by_name "term"
    let l: token parser = token_by_value "("
    let r: token parser = token_by_value ")"
    let rec lisp tokens  =
        let l = 
            fun tokens ->
                let many: sexpr list parser = 
                    rep lisp 0 -1 <| fun lst -> lst
                let l: sexpr list parser =
                    both l many <| fun _ lst -> lst
                let r: sexpr parser = 
                    both l r    <| fun lst _ -> S lst
                r tokens
        let m: sexpr parser = map term <| fun it -> Term it.value
        either l m tokens 
    
    let filt = Seq.filter <| fun (it: token) -> it.name <> "space"
    
    let tokens = filt >> Array.ofSeq 
                 <| lex None lexer_tb 
                    {filename = "<test>"
                     text = "(add 1 (mul 1 2 3))"}

    let tokens = {arr = tokens; offset = 0}
    
    let value = parse tokens lisp
    sprintf "%A" value

    (**
    output:
    S [S [Term "3"; Term "2"; Term "1"; Term "mul"]; Term "1"; Term "add"]
    *)
```