namespace Linear

/// Set of extensions to module Microsoft.FSharp.Math.RowVector module.
[<RequireQualifiedAccess>]
module RowVector = begin
  /// Creates list from row-vector.
  let toList (vector: rowvec) =
    vector |> List.ofSeq
  
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

  /// Transposition of current row-vector.
  let transpose (row: rowvec) =
    row.Transpose
end
