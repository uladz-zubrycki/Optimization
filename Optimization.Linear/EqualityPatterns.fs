namespace Linear

[<AutoOpen>]
module EqualityPatterns = begin
  let private check (predicate: 'a -> 'b -> bool) arg value =
   if predicate arg value then Some() else None
  
  let (|Equals|_|) arg value = 
    check (=) arg value

  let (|Unequals|_|) arg value = 
    check (<>) arg value
  
  let (|Less|_|) arg value = 
    check (<) arg value 
  
  let (|Bigger|_|) arg value = 
    check (>) arg value  
  
  let (|BiggerOrEqual|_|) arg value = 
    check (>=) arg value  
  
  let (|LessOrEqual|_|) value arg = 
    check (<=) arg value 
end
