// mathematic code here, so
// have much variables like `J'b` 
// and warning is rather annoying 
// while useless
#nowarn "0049"

namespace Optimization

open Optimization.Utils

[<AutoOpen>]
module Operators = begin
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
  
  let opSeq op (fst: float seq) (snd: float seq) = 
   (fst, snd) 
   ||> Seq.map2 (fun x y -> 
          (x, y) ||> op
       )
   |> Seq.forall id

  let equalSeq fst snd = (fst, snd) ||> opSeq equal
  let biggerSeq fst snd = (fst, snd) ||> opSeq bigger
  let biggerOrEqualSeq fst snd = (fst, snd) ||> opSeq biggerOrEqual
  let lessSeq fst snd = (fst, snd) ||> opSeq less
  let lessOrEqualSeq fst snd = (fst, snd) ||> opSeq lessOrEqual
end

(*
    Transportation theory task.
      n: int - count of product types
      m: int - count of resource types
      b: m vector - count of recources available
      c: n vector - profit from products
      A: (m,n) matrix - cost of product production
      J: n list - list of all indices 
      J'b: list - basis indices
      d'down: n vector - lower restrictions for x vector
      d'up: n vector - upper restriction for x vector
  
      c'x -> max
      Ax = b
      d'down <= x <= d'up
*)
[<AutoOpen>]
module DualSimplex = begin
  let private initJ'b (A:matrix) =
    let m, n = A.NumRows, A.NumCols

    let checkJ'b (J'b: int list) =
      let A'b = A |> Matrix.sliceCols J'b
      
      try 
        Matrix.inv A'b |> ignore
        true
      with
      | _ -> false

    let nextJ'b cur =
      let min = List.min >> ((+) 1) <| cur  
      let max = min + m
      
      [min..max - 1]
    
    let rec updateJ'b cur = 
      if List.max >> ((<) n) <| cur then
        failwith "Can't create initial basis" 
      else

      if checkJ'b cur then
        cur
      else
        cur 
        |> nextJ'b 
        |> updateJ'b
      
    updateJ'b [0..m - 1]

  let dualSimplex (A:matrix, b: vector, c, d, J) =
    let (d'down: float list, d'up: float list) = d
    let m, n = A.NumRows, A.NumCols
    let J'b = initJ'b A
    let J'n = J |> List.without J'b
    let A'b = A |> Matrix.sliceCols J'b
    let B = A'b |> Matrix.inv
    let c'b = c |> RowVector.items J'b
    
    let y = c'b * B
    let delta = J 
                |> List.map (fun j ->
                     let A'j = A.Column j
                     let c'j = c.[j]
                
                     y.Transpose 
                     |> Vector.dot A'j
                     |> (fun num -> num - c'j) 
                   ) 
    
    let ``J'n+``, ``J'n-`` = 
      J'n |> List.partition (fun j -> 
               let delta'j = delta.[j]
               biggerOrEqual delta'j 0.0
             )
    
    let rec dualSimplexImpl (delta: float list, J'b, ``J'n+``, ``J'n-``) =
      let J'n = J |> List.without J'b
      let A'b = A |> Matrix.sliceCols J'b
      let B = A'b |> Matrix.inv

      let getX () =
        let getByIndices indices index items =
          indices |> Seq.findIndex ((=) index)
                  |> (fun i -> Seq.nth i items )

        let x'n = J'n |> List.map (fun j ->
                           match j with
                           | In ``J'n+`` -> d'down.[j]
                           | In ``J'n-`` -> d'up.[j]
                           | _ -> failwith "No more indices expected."
                         )    
  
        let x'b = J'n
                  |> Seq.map (fun j ->
                       let A'j = A.Column j
                       let x'j = x'n |> getByIndices J'n j 
                     
                       A'j * x'j
                     ) 
                  |> Seq.reduce (+)
                  |> (fun sum -> B * (b - sum))
       
        J 
        |> Seq.map (fun j ->
             match j with
             | In J'b -> x'b |> getByIndices J'b j
             | In J'n -> x'n |> getByIndices J'n j
             | _ -> failwith "No more indices expected"
           )
        |> RowVector.ofSeq

      let x = getX ()
    
      let criteriaRes = 
        J |> List.map (fun j ->
               let x'j = x.[j] 
               let min = d'down.[j]
               let max = d'up.[j]
               let satisfiesDown = lessOrEqual min x'j
               let satisfiesUp = biggerOrEqual max x'j
    
               let satisfies = satisfiesDown && satisfiesUp
               let m'j = if satisfies then 0.0
                         else if satisfiesUp then 1.0
                         else -1.0
    
               j, satisfies, m'j
             ) 
    
      let isOptimal = criteriaRes 
                      |> Seq.map snd3 
                      |> Seq.reduce (&&)
    
      match isOptimal with
      | true -> Some(x)
      | false ->
          let (j'k, _, m'jk) = criteriaRes 
                               |> List.find (snd3 >> not) 
    
          let k = J'b |> List.findIndex ((=) j'k)
          let e'k = RowVector.E m k
          let delta'y = m'jk * (e'k * B)
    
          let M = J |> List.map (fun j -> 
                         match j with
                         | In J'b -> 0.0
                         | _ ->
                             let A'j = A.Column j
                             delta'y * A'j
                       )
          
          let steps = 
            J'n |> Seq.map (fun j -> 
                     let m'j = M.[j]
                     match (j, m'j) with
                       | (In ``J'n+``, Less 0.0) 
                       | (In ``J'n-``, Bigger 0.0) -> 
                          let delta'j = delta.[j]
                          j, -delta'j / m'j
                       | _ -> j, infinity
                   )
    
          let (j'0, step'0) = steps |> Seq.minBy snd
    
          match step'0 with
          | Equals infinity -> None // no plans
          | _ -> 
            let newDelta = J 
                           |> Seq.map (fun j -> 
                                match j with
                                | In J'n | Equals j'k -> 
                                    let delta'j = delta.[j]
                                    let m'j = M.[j]
                                    delta'j + step'0 * m'j
                                | In J'b -> 0.0
                                | _ -> failwith "No more indices expected"
                              )
                          |> List.ofSeq
           
            let newJ'b = J'b |> List.replace j'k j'0 
            let newJ'n = J |> List.without newJ'b
            let ``newJ'n+`` = match (m'jk, j'0) with
                              | ( 1.0, In ``J'n+``) -> ``J'n+`` |> List.replace j'0 j'k
                              | (-1.0, In ``J'n+``) -> ``J'n+`` |> List.without [j'0]
                              | ( 1.0, NotIn ``J'n+``) -> j'k :: ``J'n+`` 
                              | (-1.0, NotIn ``J'n+``) -> ``J'n+``
                              | _ -> failwith "No more variats expected."
    
            let ``newJ'n-`` = newJ'n |> List.without ``newJ'n+``
            
            dualSimplexImpl (newDelta, newJ'b, ``newJ'n+``, ``newJ'n-``)
        
    dualSimplexImpl (delta, J'b, ``J'n+``, ``J'n-``)
 end 
 
  
  
                  
    
  
  