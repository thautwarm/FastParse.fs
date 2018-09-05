module FastParse.Infras

type token = {
    filename : string
    lineno   : int
    colno    : int
    offset   : int
    name     : string
    value    : string
}

type ('k, 'v) hashtable = System.Collections.Generic.Dictionary<'k, 'v>
type 't hashset = System.Collections.Generic.HashSet<'t>
type 'T arraylist = System.Collections.Generic.List<'T>

type 'a maybe =
    | Just of 'a
    | Nothing
    static member inline Return (a: 'a) = Just a
    static member inline Bind   ((ma: 'a maybe, f: 'a -> 'b maybe)) = 
        match ma with 
        | Just a -> f a
        | Nothing -> Nothing
    
let inline Return< ^a, ^b when ^b: (static member Return: ^a -> ^b)> (a: ^a) = (^b : (static member Return: ^a -> ^b) a)

let inline (>>=) (a: ^a when ^a: (static member Bind: (^a * (^e -> ^b)) -> ^b)) (f: ^e -> ^b): ^b = 
                let tp = (a, f)
                in (^a: (static member Bind: (^a * (^e -> ^b)) -> ^b) tp)

let inline (&=) a b =
    obj.ReferenceEquals(a, b)

let caching_pool : (string, string) hashtable = hashtable()

let inline cast_const str =
    match caching_pool.TryGetValue str with
        | (false, _) ->
            caching_pool.Add(str, str)
            str
        | (true, r) -> r