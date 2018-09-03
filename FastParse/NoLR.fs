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
    arr   : 't array
    offset: int
}

let (|As|Empty|) ({arr = arr; offset = offset}: 't view) = 
    if arr.Length <= offset then Empty
    else As(arr.[offset], {arr = arr; offset = offset + 1})

type 't parser = (token view -> ('t * token view) M)

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

let any: token parser = 
    function
    | As(head, tail) -> Just(head, tail)
    | _              -> Nothing

let nor (pa: 'a parser): token parser =
    fun tokens ->
    match pa tokens with
    | Just _  -> Nothing
    | Nothing -> any tokens

let rep (pa: 'a parser) at_least at_most (setter: 'a list -> 'b): 'b parser = 
    if at_most <= 0 then 
        fun tokens ->
        let rec loop tokens now lst = 
            match pa tokens with
            | Nothing when now >= at_least -> Just(lst, tokens)
            | Nothing -> Nothing
            | Just(a, tokens) -> loop tokens <| now + 1 <| a :: lst
        match loop tokens 0 [] with
        | Nothing   -> Nothing
        | Just(lst, tokens) -> Just(setter lst, tokens)
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
        | Just(lst, tokens) -> Just(setter lst, tokens)

let map (pa: 'a parser) (map: 'a -> 'b): 'b parser = 
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

let jump (map: (string, 'a parser) Map): 'a parser = 
    function
    | Empty -> Nothing
    | As(head, tail) ->
    match Map.tryFind head.value map with
    | Some parser -> parser tail
    | _           -> Nothing

let token_by_name (name: string): token parser =
    let name = cast_const name
    function 
    | As(head, tail) when head.name &= name ->
        Just(head, tail)
    | _ -> Nothing

let parse (tokens: token view) (parser: 't parser) = 
    match parser tokens with
    | Just(res, Empty) -> res
    | _ as it -> failwithf "%A" it
    
    
    



    

