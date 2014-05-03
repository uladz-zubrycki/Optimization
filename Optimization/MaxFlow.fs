module Optimization.MaxFlow

(*
  net S{I, U}, where I - nodes, U - edges
  I+(i) - I(U) where exist (i, j)
  I-(i) - I(U) where exist (j, i)
 
  vector x - flow on S {I, U} if
     sum (j from I+) x'ij  - sum (j from I-) x'ij = a'i
*)

type net = 
  | Node of nodedata * edge list 
  | Leaf of nodedata 
and nodedata =
  { index: int; 
    density: float }
and edge =
  { net: net 
    flow: int;
    cost: float }

//type net = node list * edge list    (* oriented net S {I, U} *)
//and node = float                    (* node density *)
//and edge =                             
//  { output: node;                   (* node i in edge (i, j) *)
//    input: node;                    (* node j in edge {i, j} *)
//    flow: int }                     (* current flow on edge *)
                                       
type basisFlow = flow * basis       (* net flow with basis *)
and flow = int list                 (* net edges flows *)
and basis = edge list               (* full edges set *)

let getPotentials (net: net) = 
  let rec getPotentialsImpl node res = 
    match node with
    | Leaf (data) -> failwith "not implemented"
    | Node (data, edges) -> 
      

      failwith "not implemented"
  
  getPotentialsImpl net

let potentials (net: net) =
  failwith "not implemented"




