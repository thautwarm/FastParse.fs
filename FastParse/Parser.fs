module FastParse.Parser
open FastParse.Infras


type token = {
    filename : string
    lineno   : int
    colno    : int
    offset   : int
    name     : string
    value    : string
}

type 't view = {
    arr    : 't array
    offset : int 
}

let (<*>) f arg = (f, f arg)

type 't parser = token view -> ('t * token view) maybe

let (|As|Empty|) ({arr = arr; offset = offset}: 't view) = 
    if arr.Length <= offset then Empty
    else As(arr.[offset], {arr = arr; offset = offset + 1})



let recur (stack: 'a -> token view -> ('a * token view) maybe) (final: 'a parser)= 
    fun tokens ->
    final tokens >>=
    function
    | (a, tokens) -> 
        let rec loop res tokens = 
            match stack res tokens with
            | Just(res, tokens) -> loop res tokens
            | _                 -> Just(res, tokens)
        loop a tokens

let pred (pred: 'a -> bool) (pa: 'a parser) =
    fun tokens ->
    pa tokens >>=
    function 
    | (a, tokens) when pred a -> Just(a, tokens)
    | _  -> Nothing

let both (pa: 'a parser) (pb: 'b parser) (setter: 'a -> 'b -> 'c): 'c parser = 
    fun tokens ->
    pa tokens >>= 
    fun (a, tokens) ->
    pb tokens >>=
    fun (b, tokens) ->
    Just(setter a b, tokens)
    
let either (pa: 'a parser) (pb: 'a parser): 'a parser = 
    fun tokens ->
    match pa tokens >>= fun (a, tokens) -> Just(a, tokens) with 
    | Just _ as it -> it
    | Nothing ->
    pb tokens >>= fun (b, tokens) -> Just(b, tokens)

let anyToken: token parser =
    function
    | As(head, tail) -> Just(head, tail)
    | _              -> Nothing

let not (pa: 'a parser) (pb: 'b parser): 'b parser =
    fun tokens ->
    match pa tokens with
    | Just _  -> Nothing
    | Nothing -> pb tokens

let rep (pa: 'a parser) at_least at_most (transform: 'a list -> 'b): 'b parser = 
    if at_most <= 0 then 
        fun tokens ->
        let rec loop tokens now lst = 
            match pa tokens with
            | Nothing when now >= at_least -> Just(lst, tokens)
            | Nothing -> Nothing
            | Just(a, tokens) -> loop tokens <| now + 1 <| a :: lst
        match loop tokens 0 [] with
        | Nothing   -> Nothing
        | Just(lst, tokens) -> Just(transform <| List.rev lst, tokens)
    else
        fun tokens ->
        let rec loop tokens now lst = 
            let at_most = at_most - 1
            match pa tokens with
            | Nothing when now >= at_least -> Just(lst, tokens)
            | Nothing -> Nothing
            | Just(a, tokens) when now < at_most -> loop tokens <| now + 1 <| a :: lst
            | Just(a, tokens) when now = at_most -> Just(a :: lst, tokens)
            | _ -> failwith "impossible"
        match loop tokens 0 [] with
        | Nothing   -> Nothing
        | Just(lst, tokens) -> Just(transform <| List.rev lst, tokens)

let trans (pa: 'a parser) (map: 'a -> 'b): 'b parser = 
    fun tokens ->
    pa tokens >>=
    fun (a, tokens) -> Just(map a, tokens)

let token_by_value str: token parser =
    function 
    | As(head, tail) when head.value = str ->
        Just(head, tail)
    | _ -> Nothing

let token_by_value_addr (str: string): token parser =
    let str = cast_const str
    function 
    | As(head, tail) when head.value &= str ->
        Just(head, tail)
    | _ -> Nothing

let pgen (pa: 'b parser parser): 'b parser = 
    fun tokens ->
    pa tokens >>=
    function
    | (parser, tokens) -> parser tokens

let token_by_name (name: string): token parser =
    let name = cast_const name
    function 
    | As(head, tail) when head.name &= name ->
        Just(head, tail)
    | _ -> Nothing

let parse (parser: 't parser) (tokens: token view)  = 
    match parser tokens with
    | Just(res, Empty) -> res
    | _ as it -> failwithf "%A" it
    
    
    



    

