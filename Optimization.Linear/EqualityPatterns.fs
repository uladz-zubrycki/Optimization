namespace Linear

[<AutoOpen>]
module EqualityPatterns = begin
  let private check (predicate: 'a -> 'b -> bool) value arg =
   if predicate arg value then Some() else None
  
  let (|Equals|_|) value arg = 
    check (=) value arg

  let (|Unequals|_|) value arg = 
    check (<>) value arg
  
  let (|Less|_|) value arg = 
    check (<) value arg 
  
  let (|Bigger|_|) value arg = 
    check (>) value arg  
  
  let (|BiggerOrEqual|_|) value arg = 
    check (>=) value arg  
  
  let (|LessOrEqual|_|) value arg = 
    check (<=) value arg 
end
