#if INTERACTIVE
#r @"bin\Debug\FSharp.PowerPack.dll"
#load "EqualityPatterns.fs" 
#load "Vector.fs" 
#load "RowVector.fs"
#load "List.fs"
#load "Matrix.fs" 
#endif

open Linear

(*
  Task:
    n: int - count of product types.
    m: int - count of resource types.
    b: vector - count of recources available.
    c: vector - profit from products. Length = n.
    a: matrix - cost of product production. Dimensions = (m,n).
    x: vector - plan. satisfies all conditions.
    J: indices - set of indeces for x vector. Length = n.

    Task canonical form:
    c * x -> max
    x >= 0
    A * x = b
*)

// Example   
let A = matrix [[3.0;  1.0; 1.0; 0.0];
                 [1.0; -2.0; 0.0; 1.0]]
let b = vector [1.0; 1.0]
let c = vector [1.0; 4.0; 1.0; -1.0]
let m = 2
let n = 4

let x'0 = vector [0.0; 0.0; 1.0; 1.0;]
let J = [0..3]
let J'b = [2; 3]

let rec simplex A c J plan = 
  let (x:vector), J'b = plan
  let J'n = J |> List.difference J'b
  let A'b = A |> Matrix.fromColumns J'b
  let c'b = c |> Vector.items J'b
  
  let B = A'b |> Matrix.inv 
  let u = c'b.Transpose * B  
  
  let delta = J |> List.map (fun j -> 
                      u * A.Column j - c.[j]
                   )
  
  J'n 
  |> List.tryFind (fun j -> delta.[j] < 0.0) 
  |> function
     | Some(j'0) ->
         let z = B * A.Column j'0
         let s, O'0 = J'b
                      |> List.filteri (fun i _ -> z.[i] > 0.0)
                      |> List.mapi (fun i j -> i, x.[j] / z.[i])
                      |> List.minBy snd
         
         let newX = x |> Vector.mapi (fun i x'i -> 
                           match i with
                           | Equals j'0 -> O'0
                           | In J'n -> 0.0
                           | _ -> x'i - O'0 * z.[List.findIndex ((=)i) J'b]
                         )
         let newJ'b = J'b |> List.mapi (fun i j -> 
                               match i with
                               | Equals s -> j'0
                               | _ -> j
                             )
         
         simplex A c J (newX, newJ'b)
     | None -> plan (*Decision is found*) 

let plan = simplex A c J (x'0, J'b)


