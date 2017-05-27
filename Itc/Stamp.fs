module Itc.Stamp

open Itc.Id
open Itc.Event

[<CustomComparison; StructuralEquality>]
type Stamp = 
    { i:Id; e:Event }
    static member Seed = { i = One; e = EventLeaf(0)}  
    static member Create(i:Id, e:Event) =
        { i = i; e = e} 
    member x.leq (y:Stamp) =
        leq x.e y.e
    interface System.IComparable<Stamp> with
        member x.CompareTo y =
            if(x.e = y.e) then 0 else if(x.leq y) then -1 else 1
    interface System.IComparable with
        member x.CompareTo yobj =
            match yobj with
            | :? Stamp as y -> (x :> System.IComparable<_>).CompareTo y
            | _ -> invalidArg "yobj" "cannot compare values of different types"   

let rec internal fill s =   
    match s.i with
    | Zero -> s.e
    | One -> EventLeaf(maxN s.e)
    | Node(l, r) -> 
        match (l, r) with
        | (One, _) -> 
            match s.e with
            | EventNode(n, el, er) -> 
                let e'r = {i = r; e = er} |> fill
                (EventNode n (EventLeaf (max (maxN el) (maxN e'r))) e'r) |> normalize
            | _ -> s.e
        | (_, One) ->
            match s.e with
            | EventNode(n, el, er) ->
                let e'l = {i = l; e = el} |> fill
                (EventNode n e'l (EventLeaf (max (maxN er) (maxN e'l)))) |> normalize
            | _ -> s.e
        | (_, _) -> 
            match s.e with
            | EventNode(n, el, er) ->
                let e'l = {i = l; e = el} |> fill
                let e'r = {i = r; e = er} |> fill
                (EventNode n e'l e'r) |> normalize
            | _ -> s.e

let rec internal grow s =
    match s.e with
    | EventLeaf(n) ->
        match s.i with
        | One -> (EventLeaf(n+1), 0)
        | _ ->
            let (e', c) = {i=s.i; e = Event.Create(n, 0, 0)} |> grow
            (e', c + 1000)
    | EventNode(n, el, er) ->
        match s.i with
        | Zero | One -> failwith "failed to grow stamp"
        | Node(il, ir) ->
            match (il, ir) with
            | (Zero, _) -> 
                let (e'r, cr) = {i = ir; e = er} |> grow
                ((EventNode n el e'r), cr + 1)
            | (_, Zero) ->
                let (e'l, cl) = {i = il; e = el} |> grow
                ((EventNode n e'l er), cl + 1)
            | (_, _) ->
                let (e'l, cl) = {i = il; e = er} |> grow
                let (e'r, cr) = {i = ir; e = er} |> grow
                if (cl<cr) 
                then ((EventNode n e'l er), cl + 1)
                else ((EventNode n el e'r), cr + 1)
        
let fork s =
     let it = split s.i
     match it with
     | Node(l, r) -> ({i = l; e = s.e}, {i = r; e = s.e})
     | _ -> failwith "split did not produce a node"

let fork3 s =
    let (a,c) = fork s
    let (a',b) = fork a
    (a',b,c)

let fork4 s =
    let (a,c) = fork s
    let (a',b) = fork a
    let (c',d) = fork c
    (a',b,c',d)

let peek s = {i = Zero; e = s.e}

let event s =
    let fillEvent = fill s
    if (s.e <> fillEvent) 
    then {i = s.i; e = fillEvent}
    else {i = s.i; e = fst (grow s) }

let join s1 s2 = 
    {i = sum s1.i s2.i; e = join s1.e s2.e}

let send s =
    let inflated = event s
    let annonymous = peek inflated
    (inflated, annonymous)

let receive s o =
    (join s o) |> event

let sync s o =
    (join s o) |> fork

let leq (s:Stamp) (o:Stamp) =
    s.leq o

let isConcurrent s o =
    not(leq s o) && not(leq o s)    

let print s = 
    sprintf "(%s,%s)" (Itc.Id.print s.i) (Itc.Event.print s.e) 
