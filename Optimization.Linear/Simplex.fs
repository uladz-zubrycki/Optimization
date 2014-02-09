#if INTERACTIVE
#r @"bin\Debug\FSharp.PowerPack.dll"
#load "EqualityPatterns.fs" 
#load "Vector.fs" 
#load "RowVector.fs"
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

    c * x -> max
    x >= 0
    A * x <= b
*)

// Example   
let A = matrix [[3.0;  1.0; 1.0; 0.0];
                [1.0; -2.0; 0.0; 1.0]]
let b = vector [1.0; 1.0]
let c = vector [1.0; 4.0; 1.0; -1.0]
let m = 2
let n = 4

let plan = (vector [0.0; 0.0; 1.0; 1.0;], [2; 3])


let matr = matrix [[3.0;  1.0; 4.0];
                   [1.0; -2.0; 5.0];
                   [2.0; 3.0; -2.0]]

let inv = matr 
          |> Matrix.inv
//          |> Matrix.map ceil



  
 
