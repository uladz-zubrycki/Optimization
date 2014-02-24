namespace Linear

[<AutoOpen>]
module ListRecognizers = 
  let (|In|_|) arg value = 
    arg 
    |> List.exists ((=) value) 
    |> function
       | true -> Some()
       | false -> None

/// Set of extensions for Microsoft.FSharp.Collections.List module
module List = 
  /// Creates list without specified items.
  let without items list =
    let itemsSet = items |> Set.ofList 
    
    list 
    |> List.filter (itemsSet.Contains >> not)

  /// Filters list using provided predicate
  /// Predicate takes element index and element
  let filteri (predicate: int -> 'a -> bool) (list: 'a list) =
    Seq.zip [0..list.Length] list 
    |> Seq.filter (fun (i, item) -> predicate i item) 
    |> Seq.map snd
    |> List.ofSeq