#if INTERACTIVE
#r @"bin\Debug\FSharp.PowerPack.dll"
#endif

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

type Matrix<'a> with 
  member self.Columns(indices: int list) =
    let indToCol =
      self.Column 
        >> Vector.Generic.toArray 
        >> List.ofArray
         
    indices 
      |> List.map (indToCol) 
      |> Matrix.Generic.ofList
end

type Vector<'a> with
  member self.Items(indices: int list) =
    indices 
      |> List.map (Vector.Generic.get self) 
      |> Vector.Generic.ofList
end

// Example   
let A = matrix [[3.0;  1.0; 1.0; 0.0];
                [1.0; -2.0; 0.0; 1.0]]
let b = vector [1.0; 1.0]
let c = vector [1.0; 4.0; 1.0; -1.0]
let m = 2
let n = 4

let plan = (vector [0.0; 0.0; 1.0; 1.0;], [2; 3])
let A'b = A.Columns <| snd plan
let c'b = c.Items <| snd plan



  
 
