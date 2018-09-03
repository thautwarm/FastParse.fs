module FastParse.Infras


type ('k, 'v) hashtable = System.Collections.Generic.Dictionary<'k, 'v>
type 't hashset = System.Collections.Generic.HashSet<'t>
type 'T arraylist = System.Collections.Generic.List<'T>

type 'a M =
    | Just of 'a
    | Nothing

let (>>=) (m : 'a M) (f : 'a -> 'b M) : 'b M =
    match m with
    | Just a ->  f a
    | Nothing -> Nothing

let Return (a: 'a) : 'a M =
    Just a

let (&=) a b =
    obj.ReferenceEquals(a, b)


let caching_pool : (string, string) hashtable = hashtable()

let cast_const str =
    match caching_pool.TryGetValue str with
        | (false, _) ->
            caching_pool.Add(str, str)
            str
        | (true, r) -> r
