namespace Optimization.Utils

[<AutoOpen>]
module Recognizers = begin
  let private matchBool = 
    function
    | true -> Some()
    | false -> None
  
  let private check (predicate: 'a -> 'b -> bool) x y =
    (x, y) 
    ||> predicate 
    |> matchBool

  let (|Equal|_|) value target = 
      check (=) target value
  
  let (|Less|_|) value target = 
    check (<) target value 

  let (|Bigger|_|) value target = 
    check (>) target value  
  
  let (|InSeq|_|) value target = 
    value 
    |> Seq.exists ((=) target) 
    |> matchBool

  let (|NotInSeq|_|) value target = 
    value 
    |> Seq.forall ((<>) target) 
    |> matchBool

  let (|Lessf|_|) value target = 
    check less target value 

  let (|BiggerOrEqualf|_|) value target = 
    check biggerOrEqual target value 
end