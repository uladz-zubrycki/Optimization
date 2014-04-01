namespace Optimization.Utils

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

  /// Creates new rowvector from elements at specified indices.
  let items indices rowvec =
    indices
    |> Seq.map (RowVector.get rowvec) 
    |> RowVector.ofSeq

  /// Creates vector of specified length, 
  /// where all values equals 0, 
  /// except value at k index, which equals 1
  let E length k = 
    RowVector.init length (fun i ->
      match i with 
      | Equals k -> 1.0
      | _ -> 0.0
    ) 

  /// RowVector dot product
  let dot (row1: rowvec) (row2: rowvec) =
    Seq.map2 (fun fst snd -> 
      fst * snd
    ) row1 row2
    |> Seq.sum
end
