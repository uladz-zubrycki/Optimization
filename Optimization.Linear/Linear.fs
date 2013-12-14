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

[<AutoOpen>]
module EqualityPatterns = begin
  let private factory (predicate: 'a -> 'b -> bool) value arg =
    if predicate arg value then
      Some()
    else
      None
  
  let (|Equals|_|) value arg = 
    factory (=) value arg

  let (|Unequals|_|) value arg = 
    factory (<>) value arg
  
  let (|Less|_|) value arg = 
    factory (<) value arg 
  
  let (|Bigger|_|) value arg = 
    factory (>) value arg  
  
  let (|BiggerOrEqual|_|) value arg = 
    factory (>=) value arg  
  
  let (|LessOrEqual|_|) value arg = 
    factory (<=) value arg 
end 

module Vector = begin 
  let toList vector =
    vector
      |> Vector.toArray 
      |> List.ofArray
  
  let items indices vector =
    indices
      |> Seq.map (Vector.get vector) 
      |> Vector.ofSeq

  let tryFindIndex predicate vector =
    vector
      |> Vector.toArray
      |> Array.tryFindIndex predicate
end

module RowVector = begin
  let toList vector =
    vector
      |> RowVector.toArray 
      |> List.ofArray

  let concat (first, second) = 
    [first; second]
      |> Seq.map toList
      |> Seq.reduce (@)
      |> RowVector.ofSeq

  let map func row =
    row
      |> RowVector.toArray
      |> Array.map func
      |> RowVector.ofArray

  let div num row= 
    row
      |> map ((/) num)
end

module Matrix = begin
  let ofRows (rows: rowvec seq) = 
    rows |> Matrix.ofSeq

  let ofColumns (columns: vector seq) = 
    columns 
      |> Seq.map Vector.transpose
      |> ofRows
      |> Matrix.transpose

  let columns indices (matrix:matrix) =
    indices 
      |> List.map (matrix.Column) 
      |> ofColumns

  let rows indices (matrix:matrix) =
    indices 
      |> List.map (matrix.Row) 
      |> ofRows

  let allRows (matrix:matrix) =
    [0..matrix.NumRows - 1]
      |> Seq.map matrix.Row

  let allColumns (matrix:matrix) =
    [0..matrix.NumCols - 1]
      |> Seq.map matrix.Column

  let isSquare (matrix: matrix) =
    matrix.NumCols = matrix.NumRows

  let exchangeRows first second (matrix:matrix) =
    if first < 0 || first > matrix.NumRows then
      invalidArg "first" "invalid index" 

    if second < 0 || first > matrix.NumRows then
      invalidArg "second" "invalid index" 
    
    if first = second then
      matrix
    else
      let getRow = function
        | Equals first -> matrix.Row second
        | Equals second -> matrix.Row first
        | x -> matrix.Row x

      [0..matrix.NumRows - 1]
        |> Seq.map getRow
        |> ofRows
     
  let augment (first:matrix) (second:matrix) = 
    if first.NumRows <> second.NumRows then
      failwith "Must have same count of rows."

    (allRows first, allRows second)
      ||> Seq.zip
      |> Seq.map RowVector.concat
      |> ofRows

  let appendRow (row:rowvec) matrix = 
    matrix
      |> allRows
      |> Seq.append <| [row]
      |> ofRows

  let appendCol (col:vector) matrix = 
    matrix
      |> allColumns
      |> Seq.append <| [col]
      |> ofColumns

  let inv (matrix: matrix) = 
    if not (isSquare matrix) then
      invalidArg "matrix" "must be square"

    let dim = matrix.NumCols

    let removeZero ind (matrix:matrix) =
      matrix.Column ind
        |> Vector.tryFindIndex ((<>) 0.0)
        |> function 
           | None -> failwith "Determinant is zero, cause matrix contains zero column"
           | Some(foundInd) when foundInd = ind -> matrix
           | Some(foundInd) -> 
               matrix |> exchangeRows ind foundInd

    let lowTriangular (matrix:matrix) = 
      let getSubtractor (cur:rowvec) (ind: int) (next:rowvec)= 
        cur
          |> RowVector.div cur.[ind]  
          |> (*) next.[ind] 

      [0..matrix.NumRows - 2]
        |> Seq.fold
            (fun (acc:matrix) ind -> 
                let rows = acc
                             |> removeZero ind
                             |> allRows

                let top = rows |> Seq.take (ind + 1)
                let bottom = 
                      rows
                        |> Seq.skip (ind + 1)
                        |> Seq.map (fun row -> 
                                      row
                                        |> getSubtractor (acc.Row ind) ind 
                                        |> (-) row 
                                    ) 
                top  
                  |> Seq.append bottom
                  |> ofRows  
            ) 
            matrix 

    let addIdentity matrix = 
      Matrix.identity dim |> augment matrix

    let getResult (matrix:matrix) =
      matrix

    matrix
      |> addIdentity
      |> lowTriangular
      |> getResult
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



  
 
