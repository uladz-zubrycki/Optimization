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

module Vector = begin 
  module Generic = begin
     let toList vector =
       vector
         |> Vector.Generic.toArray 
         |> List.ofArray 
  end
end

module RowVector = begin
  module Generic = begin
     let toList vector =
       vector
         |> RowVector.Generic.toArray 
         |> List.ofArray
  end
end 

type Vector<'a> with
  member self.Items indices =
    indices
      |> Seq.map (Vector.Generic.get self) 
      |> Vector.Generic.ofSeq

  member self.ToList() =
    self |> Vector.Generic.toList 
end

module Matrix = begin
  module Generic = begin
    let ofRows (rows: RowVector<'a> seq) = 
      rows |> Matrix.Generic.ofSeq

    let isSquare (matrix: Matrix<'a>) =
      matrix.NumCols = matrix.NumRows

    let augment (first:Matrix<'a>) (second:Matrix<'a>) = 
      if first.NumRows <> second.NumRows then
        failwith "Must have same count of rows"

      let appendRows (first, second) = 
        [first; second]
          |> Seq.map RowVector.Generic.toList
          |> Seq.reduce (@)
          |> RowVector.Generic.ofSeq

      let allRows (matrix:Matrix<'a>) =
        [0..matrix.NumRows - 1]
          |> Seq.map matrix.Row

      (allRows first, allRows second)
        ||> Seq.zip
        |> Seq.map appendRows
        |> ofRows

    let inv (matrix: Matrix<'a>) = 
      if not (isSquare matrix) then
        invalidArg "matrix" "must be square"

      let dim = matrix.NumCols

      let kickZero matrix =
        matrix
        
      let addIdentity matrix = 
        let identity = Matrix.Generic.identity dim
        augment matrix identity 
        
      matrix
        |> kickZero
        |> addIdentity
  end
end

type Matrix<'a> with 
  member self.Columns(indices: int list) =
    let indToCol =
      self.Column >> Vector.Generic.toList 
         
    indices 
      |> List.map (indToCol) 
      |> Matrix.Generic.ofList

   member self.IsSquare =
     Matrix.Generic.isSquare self
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



  
 
