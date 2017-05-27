module Itc.Event

type Event =
    | EventLeaf of int
    | EventNode of int * Event * Event
    static member Create(n) =
        if n<0 
        then failwith "N must be 0 or greater" 
        else EventLeaf(n) 
    static member Create(n, l, r) =
        if n<0 
        then failwith "N must be 0 or greater" 
        else EventNode(n, l, r) 
    static member Create(n, l, r) =
        if n<0 
        then failwith "N must be 0 or greater" 
        else EventNode(n, Event.Create l, Event.Create r)
    static member Zero =
        EventLeaf(0)

let EventLeaf n =
    Event.Create(n)

let EventNode n (l:Event) (r:Event) =
    Event.Create(n,l,r)

let lift m e = 
    match e with
    | EventLeaf(n) -> EventLeaf (m + n)
    | EventNode(n, l, r) -> EventNode (m + n) l r

let rec sink m = function
    | EventLeaf(n) -> EventLeaf(n - m)
    | EventNode(n, l, r) ->
        let i = n - m
        if (i < 0) 
        then 
            let sinkI = sink (abs i)
            EventNode 0 (sinkI l) (sinkI r)  
        else EventNode i l r

let rec minN e =
    match e with
    | EventLeaf(n) -> n
    | EventNode(n, l, r) -> n + min (minN l) (minN r)

let rec maxN e =
    match e with
    | EventLeaf(n) -> n
    | EventNode(n, l, r) -> n + max (maxN l) (maxN r)

let rec leq e1 e2 =
    match (e1,e2) with
    | (EventLeaf(n1), EventLeaf(n2)) -> n1 <= n2
    | (EventLeaf(n1), EventNode(n2, _, _)) -> n1 <= n2
    | (EventNode(n1, l1, r1), EventLeaf(n2)) -> n1 <= n2 && leq (lift n1 l1) e2 && leq (lift n1 r1) e2
    | (EventNode(n1, l1, r1), EventNode(n2, l2, r2)) -> n1 <= n2 && leq (lift n1 l1) (lift n2 l2) && leq (lift n1 r1) (lift n2 r2)

let rec normalize = function
    | EventLeaf(n) -> EventLeaf n
    | EventNode(n, l, r) -> 
        let mval e = 
            match e with
            | EventLeaf(n1) -> n1
            | _ -> -1
        let m1, m2 = mval l, mval r
        if (m1 > -1 && m2 > -1 && m1 = m2) 
        then EventLeaf(n + m1)
        else 
            let m = min (minN l) (minN r)
            EventNode (n+m) (sink m l |> normalize) (sink m r |> normalize)

let rec join l r =
    match (l, r) with
    | (EventLeaf(n1), EventLeaf(n2)) -> EventLeaf(max n1 n2)
    | (EventLeaf(n1), _) -> join (EventNode n1 (EventLeaf 0) (EventLeaf 0)) r
    | (_, EventLeaf(n2)) -> join l (EventNode n2 (EventLeaf 0) (EventLeaf 0))
    | (EventNode(n1, l1, r1), EventNode(n2, l2, r2)) -> 
        if (n1>n2) then join r l else 
            let ln = join l1 (lift (n2-n1) l2)
            let rn = join r1 (lift (n2-n1) r2)
            EventNode n1 ln rn |> normalize

let rec print = function
    | EventLeaf(n) -> sprintf "%i" n
    | EventNode(n, l, r) -> sprintf "(%i,%s,%s)" n (print l) (print r)

    