namespace Optimization.Utils

[<AutoOpen>]
module ListRecognizers = begin
  let (|In|_|) arg value = 
    arg 
    |> List.exists ((=) value) 
    |> function
       | true -> Some()
       | false -> None

  let (|NotIn|_|) arg value = 
    arg 
    |> List.exists ((=) value) 
    |> function
       | true -> None
       | false -> Some()
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
    list |> List.map(fun i ->
              match i with
              | Equals target -> value
              | _ -> i
            )
end