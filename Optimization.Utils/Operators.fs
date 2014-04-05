namespace Optimization.Utils

[<AutoOpen>]
module FloatOperators = begin
  let precision = pown 0.1 3
  
  let equalPrec precision x y = abs(x - y) < precision 
  let equal = equalPrec precision 
  let notEqual x y = not (equal x y)    
  
  let cond op x y = op x y && notEqual x y
  let condOrEqual op x y = op x y || equal x y
  
  let bigger = cond (>)
  let biggerOrEqual = condOrEqual (>)
  let less = cond (<)
  let lessOrEqual = condOrEqual (<)
  
  let condSeq op (fst: float seq) (snd: float seq) = 
   (fst, snd) 
   ||> Seq.map2 op
   |> Seq.forall id

  let equalSeq fst snd = (fst, snd) ||> condSeq equal
  let biggerSeq fst snd = (fst, snd) ||> condSeq bigger
  let biggerOrEqualSeq fst snd = (fst, snd) ||> condSeq biggerOrEqual
  let lessSeq fst snd = (fst, snd) ||> condSeq less
  let lessOrEqualSeq fst snd = (fst, snd) ||> condSeq lessOrEqual
end

