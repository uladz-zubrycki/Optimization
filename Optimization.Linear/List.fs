namespace Linear

/// Set of extensions for Microsoft.FSharp.Collections.List module
module List = 
  /// Set difference of two lists.
  let difference second first =
    [first; second]
    |> List.map Set.ofList
    |> fun list ->
        let (f, s) = (list.[0], list.[1])
        s 
        |> Set.difference f
        |> List.ofSeq  

  /// Filters list using provided predicate
  /// Predicate takes element index and element
  let filteri (predicate: int -> 'a -> bool) (list: 'a list) =
    Seq.zip [0..list.Length] list 
    |> Seq.filter (fun (i, item) -> predicate i item) 
    |> Seq.map snd
    |> List.ofSeq