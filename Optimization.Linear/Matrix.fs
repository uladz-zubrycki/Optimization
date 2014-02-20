namespace Linear

#if INTERACTIVE
open Linear
open Linear.EqualityPatterns
#endif

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
  let colCount (matrix: matrix) =
    matrix.NumCols

  /// Get matrix rows count.
  let rowCount (matrix: matrix) =
    matrix.NumRows

  /// Create new matrix using columns at specified indices. 
  let fromColumns indices (matrix: matrix) =
    indices 
    |> List.map (matrix.Column) 
    |> ofColumns

  /// Create new matrix using rows at specified indices.
  let fromRows indices (matrix: matrix) =
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
      | Equals first -> matrix.Row second
      | Equals second -> matrix.Row first
      | x -> matrix.Row x

      [0..matrix.NumRows - 1]
      |> Seq.map getRow
      |> ofRows
   
  /// Augment matrix with another one.
  let augment (first:matrix) (second:matrix) = 
    if first.NumRows <> second.NumRows then
        failwith "Must have same count of rows."

    let concatRowPair (first: rowvec, second: rowvec) = 
      RowVector.concat first second

    (rows first, rows second)
    ||> Seq.zip
    |> Seq.map concatRowPair
    |> ofRows

  /// Append row to matrix.
  let appendRow (row:rowvec) matrix = 
    matrix
    |> rows
    |> Seq.append [row]
    |> ofRows

  /// Append column to matrix.
  let appendCol (col:vector) matrix = 
    matrix
    |> columns
    |> Seq.append [col]
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
             matrix |> exchangeRows ind foundInd
    
    let forward (matrix: matrix) = 
      [0..matrix.NumRows - 1]
      |> Seq.fold (fun (acc: matrix) ind -> 
            let rows = 
              acc
              |> removeZero ind
              |> rows

            let curRow = Seq.nth ind rows
            let curRow = RowVector.div curRow.[ind] curRow

            let top = rows |> Seq.take (ind)
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
            let rows = rows acc
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
        (matrix
         |> colCount
         |> Matrix.identity) 

    let dim = matrix.NumCols

    matrix
    |> addIdentity
    |> forward
    |> backward
    |> fromColumns [dim..dim * 2 - 1]
end
