module LRParser
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

type 't parser = {
    apply: (state -> 't parsing)
    id: int
}

and 't parsing =
    | Success of   't * state
    | LR      of (obj -> state -> 't parsing) * obj * state
    | Failure

and state = {
    tokens  : token view
    trace   : int Set
    lr      : (int * int) Set
    parsers : int Set
    }

and 'a handler = | H of 'a parser * 'a parsing
    with
    (** parsing monad*)
    static member inline Bind ((ma: 't handler , f: ('t * state) -> 'g parsing)) =
        match ma with
        | H(_, Success(a, state)) -> f(a, state)
        | H(_, Failure) -> Failure
        | H(self, LR(stack, recur_parser, state)) ->
        (** in this case, 't must be 'g! *)
        if obj.ReferenceEquals(self, recur_parser) then
            handler.parsing_monad_app self state >>=
            fun (a, ({tokens = {offset = offset} as tokens; lr = lr; trace = trace} as state)) ->
            let rec loop (res: 't) state =
                match stack res state with
                | Success(res, state) -> loop res state
                | _               ->
                Success(res :> obj :?> 'g, {state with lr = Set.remove (offset, self.id) lr})
            loop a state
        else
        let stack (obj: obj) state: 'g parsing =
            match stack obj state with
            | Success(a, state) -> f(a, state)
            | _             -> Failure
        LR(stack, recur_parser, {state with trace = Set.remove self.id state.trace})

    static member inline parsing_monad_app (p: 'a parser) (state: state): 'a handler =
        match state with
        | {lr = lr; tokens = {offset = offset}}
          when Set.contains (offset, p.id) lr ->
           H(p, Failure)
        | {trace = trace; lr = lr; tokens = {offset = offset}}
           when Set.contains p.id trace ->
           let stack (res: obj) state = Success(res :?> 't, state)
           H(p, LR(stack, p :> obj, {state with lr = Set.add (offset, p.id) lr}))
        | {trace = trace} ->
            H(p, p.apply {state with trace = Set.add p.id trace})

let (<*>): 'a parser -> state -> 'a handler = handler.parsing_monad_app

let (|Match|UnMatch|) =
    function
    | H(_, Success(a, state)) -> Match(a, state)
    | H _ -> UnMatch

let (|As|Empty|) ({tokens = {arr = arr; offset = offset}} as state)=
    if arr.Length <= offset then Empty
    else As(arr.[offset], {state with tokens = {arr = arr; offset = offset + 1}; trace = set []})

type language = {
        id: int
    }
    with
    member inline this.pred (pred: 'a -> bool) (pa: 'a parser) =
        fun tokens ->
        pa <*> tokens >>=
        function
        | (a, tokens) when pred a -> Success(a, tokens)
        | _  -> Failure


    member inline this.both (pa: 'a parser) (pb: 'b parser) (setter: 'a -> 'b -> 'c) =
        fun tokens ->
        pa <*> tokens >>=
        fun (a, tokens) ->
        pb <*> tokens >>=
        fun (b, tokens) ->
        Success(setter a b, tokens)

    member inline this.either (pa: 'a parser) (pb: 'a parser) =
        fun tokens ->
        match pa <*> tokens >>= fun (a, tokens) -> Success(a, tokens) with
        | Success _ as it -> it
        | _ -> pb <*> tokens >>= fun (b, tokens) -> Success(b, tokens)



    member inline this.not (pa: 'a parser) (pb: 'b parser) =
        fun state ->
        match pa <*> state with
        | Match _ -> Failure
        | UnMatch ->
        pb <*> state >>=
        fun (a, b) -> Success(a, b)


    member inline this.rep (pa: 'a parser) at_least at_most (transform: 'a list -> 'b) =
        if at_most <= 0 then
            fun state ->
            let rec loop state now lst =
                match pa <*> state with
                | UnMatch when now >= at_least -> Success(lst, state)
                | UnMatch -> Failure
                | Match(a, state) -> loop state <| now + 1 <| a :: lst
                | _ -> failwith "Impossible"
            match loop state 0 [] with
            | Success(lst, state) -> Success(transform <| List.rev lst, state)
            | _ -> Failure

        else
            fun state ->
            let rec loop state now lst =
                let at_most = at_most - 1
                match pa <*> state with
                | UnMatch when now >= at_least -> Success(lst, state)
                | UnMatch -> Failure
                | Match(a, state) when now < at_most -> loop state <| now + 1 <| a :: lst
                | Match(a, state) when now = at_most -> Success(a :: lst, state)
                | _ -> failwith "impossible"
            match loop state 0 [] with
            | Success(lst, state) -> Success(transform <| List.rev lst, state)
            | _   -> Failure
        
    member inline this.trans (pa: 'a parser) (map: 'a -> 'b) = 
        fun state ->
        pa <*> state >>=
        fun (a, state) -> Success(map a, state)
    
    member inline this.anyToken =
        function
        | As(current, new_state) ->
            Success(current, new_state)
        | _ -> Failure

    member inline this.token_by_value str =
        function 
        | As(current, new_state) when current.value = str ->
            Success(current, new_state)
        | _ -> Failure
    
    member inline this.token_by_value_addr (str: string) =
        let str = cast_const str
        function 
        | As(current, new_state) when current.value &= str ->
            Success(current, new_state)
        | _ -> Failure
    
    member inline this.pgen (pa: 'b parser parser) = 
        fun state ->
        pa <*> state >>=
        function
        | (parser, state) -> 
        parser <*> state >>=
        function
        | (result, state) -> Success(result, state)

    member this.token_by_name (name: string)=
        let name = cast_const name
        function 
        | As(current, new_state) when current.name &= name ->
            Success(current, new_state)
        | _ -> Failure

let parse (parser: 't parser) (state: state)  = 
    match parser <*> state with
    | Match(a, Empty) -> a
    | _ -> failwith "emmmm"



    