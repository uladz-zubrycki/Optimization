namespace Linear

[<AutoOpen>]
module ListPatterns = begin
  let (|In|_|) arg value = 
    arg 
    |> List.exists ((=) value) 
    |> function
       | true -> Some()
       | false -> None
end


