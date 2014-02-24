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

     /// Partitions vector to two according to predicate result
  let partition predicate (row:rowvec) =
    row
    |> toList
    |> List.partition predicate
    |> function
       | (fst, snd) -> (fst |> RowVector.ofList, snd |> RowVector.ofList)

  /// Partitions vector to two according to predicate result
  let partitioni predicate (row:rowvec) = 
    let fromIndexedTuple tuple =
       tuple |> List.map snd |> RowVector.ofList

    row
    |> toList
    |> List.mapi (fun i item -> i, item)
    |> List.partition (fun (i, item) -> predicate i item)
    |> function
       | (fst, snd) -> (fromIndexedTuple fst, fromIndexedTuple snd)
end
