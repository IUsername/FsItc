module Itc.Id

type Id =
    | Zero
    | One
    | Node of Id * Id

let rec print id = 
    match id with
    | Zero -> "0"
    | One -> "1"
    | Node(l, r) -> sprintf "(%s,%s)" (print l) (print r)

let normalize = function
    | Node(Zero, Zero) -> Zero
    | Node(One, One) -> One
    | t -> t

let rec sum l r =
    match (l, r) with
    | (Zero, _) -> r  
    | (_, Zero) -> l
    | (Node(l1, r1), Node(l2, r2)) -> Node(sum l1 l2, sum r1 r2) |> normalize
    | (One, _) | (_, One) -> One

let rec split t = 
    match t with
    | Zero -> Node(Zero, Zero)
    | One -> Node(Node(One, Zero), Node(Zero, One))
    | Node(Zero, r) -> 
        split r |> function
        | Node(il, ir) -> Node(Node(Zero, il), Node(Zero, ir))
        | _ -> failwith "Expected a Node"
    | Node(l, Zero) ->
        split l |> function
        | Node(il, ir) -> Node(Node(il, Zero), Node(ir, Zero))
        | _ -> failwith "Expected a Node"            
    | Node(l, r) -> Node(Node(l, Zero), Node(Zero, r))