namespace Linear

/// Set of extensions for Microsoft.FSharp.Math.Vector module.
[<RequireQualifiedAccess>]
module Vector = begin
  /// Creates list from current vector.
  let toList (vector: vector) =
    vector |> List.ofSeq
  
  /// Creates new vector from elements at specified indices.
  let items indices vector =
    indices
    |> Seq.map (Vector.get vector) 
    |> Vector.ofSeq

  /// Transposition of current vector.
  let transpose (vector: vector) = 
    vector.Transpose 
end
