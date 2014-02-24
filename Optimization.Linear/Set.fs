namespace Linear

[<AutoOpen>]
module SetExtensions =
  type Set<'a> when 'a: comparison with 
    member self.Contains item =
      self |> Set.contains item 