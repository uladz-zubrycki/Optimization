namespace Optimization

[<AutoOpen>]
module Tuple = begin
  let snd3 (x, y, z) = y
end

[<AutoOpen>]
module FloatOperators = begin
  let precision = pown 0.1 3
  
  let equalPrec precision x y = abs(x - y) < precision 
  let equal = equalPrec precision 
  let notEqual x y = not (equal x y) 
  
  let cond op x y = op x y && notEqual x y
  let condOrEqual op x y = op x y || equal x y
  
  let condSeq op (fst: float seq) (snd: float seq) = 
   (fst, snd) 
   ||> Seq.map2 op
   |> Seq.forall id

  let bigger = cond (>)
  let biggerOrEqual = condOrEqual (>)
  let less = cond (<)
  let lessOrEqual = condOrEqual (<)
  let equalSeq x y = condSeq equal x y
  let biggerSeq x y = condSeq bigger
  let biggerOrEqualSeq x y = (x, y) ||> condSeq biggerOrEqual 
  let lessSeq x y = (x, y) ||> condSeq less
  let lessOrEqualSeq x y = (x, y) ||> condSeq lessOrEqual
end

[<AutoOpen>]
module Recognizers = begin
  let private matchBool = 
    function
    | true -> Some()
    | false -> None

  let private check (predicate: 'a -> 'b -> bool) x y =
    (x, y) 
    ||> predicate 
    |> matchBool

  let (|Equal|_|) value target = check (=) target value
  let (|Less|_|) value target = check (<) target value 
  let (|Bigger|_|) value target = check (>) target value  
  let (|Lessf|_|) value target = check less target value 
  let (|BiggerOrEqualf|_|) value target = check biggerOrEqual target value 

  let (|InSeq|_|) value target = 
    value 
    |> Seq.exists ((=) target) 
    |> matchBool
end

/// Set of extensions for Microsoft.FSharp.Collections.List module
module List = begin
  /// Creates list without specified items.
  let without (items: 'a list) (list: 'a list) =
    list
    |> List.filter (fun i ->
         not (items |> List.exists ((=) i))
       )   
 
  /// Filters list using provided predicate
  /// Predicate takes element index and element
  let filteri (predicate: int -> 'a -> bool) (list: 'a list) =
    Seq.zip [0..list.Length] list 
    |> Seq.filter (fun (i, item) -> predicate i item) 
    |> Seq.map snd
    |> List.ofSeq

  /// Replaces all occurences of target with value in list
  let replace target value list =
    list 
    |> List.map(fun i ->
         match i with
         | Equal target -> value
         | _ -> i
       )

  /// Set value at specified index
  let set index value list = 
    list 
    |> List.mapi (fun i x -> 
         if i = index then value
         else x
       )
end

/// Set of extensions for Microsoft.FSharp.Math.Vector module.
[<RequireQualifiedAccess>]
module Vector = begin
  let concat (fst: vector) (snd: vector) = 
    [fst; snd]
    |> Seq.concat 
    |> Vector.ofSeq
  
  /// Creates new vector from elements at specified indices.
  let items indices vector =
    indices
    |> Seq.map (Vector.get vector) 
    |> Vector.ofSeq

  /// Transposition of current vector.
  let transpose (vector: vector) = vector.Transpose
  
  /// Appends sequence of items to vector
  let append items vector = 
    Seq.append vector items
    |> Vector.ofSeq   

  /// Creates rowvector of specified length, 
  /// where all values equals 0, 
  /// except value at specified index, which equals 1
  let E length index = 
    Vector.init length (fun i ->
      if i = index then 1.0
      else 0.0
    )
end

/// Set of extensions to module Microsoft.FSharp.Math.RowVector module.
[<RequireQualifiedAccess>]
module RowVector = begin
  /// Creates list from row-vector.
  let toList (row: rowvec) = row |> List.ofSeq

  /// Appends sequence of items to rowvector
  let append items (row: rowvec) = 
    items
    |> Seq.append row
    |> RowVector.ofSeq

  /// Concatenates two row-vectors.
  let concat (first: rowvec) (second: rowvec) = 
    second
    |> Seq.append first 
    |> RowVector.ofSeq
  
  /// Maps row-vector using provided mapper function.
  let map mapper (row: rowvec) =
    row
    |> Seq.map mapper
    |> RowVector.ofSeq
  
  /// Divides every element by provided number. 
  let div num row = 
    if num = 0.0 then invalidArg "num" "Can't divide by zero."
    row |> map (fun el -> el / num)

  /// Creates new rowvector from elements at specified indices.
  let items indices rowvec =
    indices
    |> Seq.map (RowVector.get rowvec) 
    |> RowVector.ofSeq

  /// Creates vector of specified length, 
  /// where all values equals 0, 
  /// except value at specified index, which equals 1
  let E length index = 
    RowVector.init length (fun i ->
      if i = index then 1.0 
      else 0.0 
    ) 

  /// RowVector dot product
  let dot (row1: rowvec) (row2: rowvec) =
    (row1, row2)
    ||> Seq.map2 (*)
    |> Seq.sum
end

/// Set of extensions for Microsoft.FSharp.Math.Matrix module.
[<RequireQualifiedAccess>]
module Matrix = begin
  /// Create matrix using sequence of rows.
  let ofRows (rows: rowvec seq) = 
    rows |> Matrix.ofSeq
  
  /// Create matrix using sequence of columns.
  let ofColumns (columns: vector seq) = 
    columns 
    |> Matrix.ofSeq
    |> Matrix.transpose

  /// Get matrix columns count.
  let colCount (matrix: matrix) = matrix.NumCols

  /// Get matrix rows count.
  let rowCount (matrix: matrix) = matrix.NumRows

  /// Create new matrix using columns at specified indices. 
  let sliceCols indices (matrix: matrix) =
    indices 
    |> List.map (matrix.Column) 
    |> ofColumns

  /// Create new matrix using rows at specified indices.
  let sliceRows indices (matrix: matrix) =
    indices 
    |> List.map (matrix.Row) 
    |> ofRows

  /// Get matrix rows as sequence.
  let rows (matrix: matrix) =
    [0..matrix.NumRows - 1] 
    |> Seq.map matrix.Row

  /// Get matrix columns as sequence.
  let columns (matrix: matrix) =
    [0..matrix.NumCols - 1]
    |> Seq.map matrix.Column

  /// Check if matrix is square.
  let isSquare (matrix: matrix) =
    matrix.NumCols = matrix.NumRows

  /// Exchange rows at specified indices.
  let exchangeRows first second (matrix:matrix) =
    if first < 0 || first > matrix.NumRows then
      invalidArg "first" "invalid index" 

    if second < 0 || first > matrix.NumRows then
      invalidArg "second" "invalid index" 
    
    if first = second then
      matrix
    else
      let getRow = function
      | Equal first -> matrix.Row second
      | Equal second -> matrix.Row first
      | x -> matrix.Row x

      [0..matrix.NumRows - 1]
      |> Seq.map getRow
      |> ofRows
   
  /// Augment matrix with another one.
  let augment (first:matrix) (second:matrix) = 
    if first.NumRows <> second.NumRows then
        failwith "Must have same count of rows."

    let concatRowPair pair = 
      pair ||> RowVector.concat 

    (rows first, rows second)
    ||> Seq.zip
    |> Seq.map concatRowPair
    |> ofRows

  /// Append row to matrix.
  let appendRow (row: rowvec) matrix = 
    Seq.append (rows matrix) [row]
    |> ofRows

  /// Append column to matrix.
  let appendCol (col: vector) matrix = 
    Seq.append (columns matrix) [col]
    |> ofColumns

  /// Get inversed matrix using Gauss-Jordan elimination.
  let inv (matrix: matrix) = 
    if not (isSquare matrix) then
      invalidArg "matrix" "must be square"

    let removeZero ind (matrix:matrix) =
      matrix.Column ind
      |> Seq.skip ind
      |> Seq.tryFindIndex ((<>) 0.0)
      |> function 
         | None -> failwith "Determinant is zero, cause matrix contains zero column"
         | Some(foundInd) when foundInd = 0 -> matrix
         | Some(foundInd) -> 
             matrix |> exchangeRows ind (foundInd + ind)
    
    let forward (matrix: matrix) = 
      [0..matrix.NumRows - 1]
      |> Seq.fold (fun (acc: matrix) ind -> 
           let rows = 
             acc
             |> removeZero ind
             |> rows
          
           let curRow = rows |> Seq.nth ind 
           let curRow = curRow |> RowVector.div curRow.[ind] 
          
           let top = rows |> Seq.take ind
           let bottom = 
             rows
             |> Seq.skip (ind + 1)
             |> Seq.map (fun row -> 
                   curRow * row.[ind] 
                   |> (-) row 
                )
          
           bottom
           |> Seq.append [curRow] 
           |> Seq.append top
           |> ofRows  
         ) 
         matrix 

    let backward (matrix: matrix) =
      [matrix.NumRows - 1..-1..0]
      |> Seq.fold (fun (acc: matrix) ind ->
           let rows = acc |> rows 
           let curRow = rows |> Seq.nth ind 
           
           let top = 
               rows 
               |> Seq.take ind
               |> Seq.map (fun row ->
                     curRow * row.[ind]
                     |> (-) row
                  )
           
           let bottom = Seq.skip ind rows 
           
           Seq.append top bottom |> ofRows
         )
         matrix

    let addIdentity matrix =
      augment matrix <|
        (matrix.NumCols
         |> Matrix.identity) 

    let dim = matrix.NumCols

    matrix
    |> addIdentity
    |> forward
    |> backward
    |> sliceCols [dim..dim * 2 - 1]
end

