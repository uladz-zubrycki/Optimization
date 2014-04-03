namespace Optimization.Utils

[<AutoOpen>]
module EqualityRecognizers = begin
  let private check (predicate: 'a -> 'b -> bool) arg value =
   if predicate arg value then Some() else None
  
  let (|Equals|_|) arg value = 
    check (=) arg value

  let (|Unequals|_|) arg value = 
    check (<>) arg value
  
  let (|Less|_|) arg value = 
    check (>) arg value 
  
  let (|Bigger|_|) arg value = 
    check (<) arg value  
  
  let (|BiggerOrEqual|_|) value vector =
    vector 
    |> Vector.forall ((<=) value)
    |> function
       | true -> Some()
       | false -> None
  
  let (|LessOrEqual|_|) value vector =
    vector 
    |> Vector.forall ((>=) value)
    |> function
       | true -> Some()
       | false -> None
end
