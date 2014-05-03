module Optimization.DualSimplex

#nowarn "0049"

type private indices = int list

type private restrictions = 
  { down: float list;
    up: float list }

type private task =
  { A: matrix;
    b: vector;
    c: rowvec;
    d: restrictions }

let private initJb (A: matrix) =
  let m, n = A.NumRows, A.NumCols

  let checkJb (Jb: indices) =
    let Ab = A |> Matrix.sliceCols Jb
    
    try 
      Matrix.inv Ab |> ignore
      true
    with
    | _ -> false

  let nextJb cur =
    let min = Seq.min cur + 1  
    let max = min + m
    
    [min..max - 1]
  
  let rec updateJb cur = 
    if Seq.max cur >= n then
      failwith "Can't create initial basis" 
    else
      if checkJb cur then cur
      else
        nextJb cur 
        |> updateJb
    
  updateJb [0..m - 1]

let private dualSimplexImpl task =
  let { A = A; b = b; c = c; d = d}  = task 
  let { down = d'down; up = d'up } = d
  let m, n = A.NumRows, A.NumCols
  let J, Jb = [0.. n - 1], initJb A
  let Jn = J |> List.without Jb
  let B = 
    A 
    |> Matrix.sliceCols Jb 
    |> Matrix.inv
  
  let cb = c |> RowVector.items Jb
  let y = cb * B
  
  let delta = 
    J |> List.map (fun j ->
           let A'j = A.Column j
           let c'j = c.[j]
         
           y.Transpose 
           |> Vector.dot A'j
           |> (fun num -> num - c'j) 
         ) 
  
  let Jnplus, Jnminus = 
    Jn |> List.partition (fun j -> 
            biggerOrEqual delta.[j] 0.0
          )
  
  let rec loop (delta: float list, Jb, Jnplus, Jnminus) =
    let Jn = J |> List.without Jb
    let Ab = A |> Matrix.sliceCols Jb
    let B = Ab |> Matrix.inv

    let getX () =
      let getByIndices indices index items =
        indices |> Seq.findIndex ((=) index)
                |> (fun i -> Seq.nth i items )

      let x'n = 
        Jn |> Seq.map (fun j ->
                match j with
                | InSeq Jnplus -> d'down.[j]
                | InSeq Jnminus -> d'up.[j]
                | _ -> failwith "No more indices expected."
              )    

      let x'b = 
        Jn
        |> Seq.map (fun j ->
             let A'j = A.Column j
             let x'j = x'n |> getByIndices Jn j 
           
             A'j * x'j
           ) 
        |> Seq.reduce (+)
        |> (fun sum -> B * (b - sum))
     
      J 
      |> Seq.map (fun j ->
           match j with
           | InSeq Jb -> x'b |> getByIndices Jb j
           | InSeq Jn -> x'n |> getByIndices Jn j
           | _ -> failwith "No more indices expected"
         )
      |> RowVector.ofSeq

    let x = getX ()
  
    let criteriaRes = 
      J |> Seq.map (fun j ->
             let min, max = d'down.[j], d'up.[j]
             let satisfiesDown = lessOrEqual min x.[j]
             let satisfiesUp = biggerOrEqual max x.[j]
          
             let satisfies = satisfiesDown && satisfiesUp
             let m'j = if satisfies then 0.0
                       else if satisfiesUp then 1.0
                       else -1.0
          
             j, satisfies, m'j
           ) 
  
    let isOptimal = criteriaRes 
                    |> Seq.map snd3 
                    |> Seq.reduce (&&)
  
    if isOptimal then Some(Jb, x)
    else
      let (j'k, _, m'jk) = criteriaRes |> Seq.find (snd3 >> not) 
      let e'k = 
        Jb 
        |> List.findIndex ((=) j'k)
        |> RowVector.E m 
      
      let delta'y = m'jk * (e'k * B)
  
      let M = 
        J |> List.map (fun j -> 
               match j with
               | InSeq Jb -> 0.0
               | _ ->
                   let A'j = A.Column j
                   delta'y * A'j
             )
      
      let steps = 
        Jn 
        |> Seq.map (fun j -> 
             match (j, M.[j]) with
             | (InSeq Jnplus, Less 0.0) 
             | (InSeq Jnminus, Bigger 0.0) -> 
                j, -delta.[j] / M.[j]
             | _ -> j, infinity
           )
  
      let (j'0, step'0) = steps |> Seq.minBy snd
  
      if step'0 = infinity then None
      else
        let newDelta = 
          J 
          |> Seq.map (fun j -> 
               match j with
               | InSeq Jn | Equal j'k -> 
                   let delta'j = delta.[j]
                   let m'j = M.[j]
                   delta'j + step'0 * m'j
               | InSeq Jb -> 0.0
               | _ -> failwith "No more indices expected"
             )
          |> List.ofSeq
       
        let Jb' = Jb |> List.replace j'k j'0 
        let Jn' = J |> List.without Jb'
        let Jnplus' = 
          match (m'jk, j'0) with
          | ( 1.0, InSeq Jnplus) -> Jnplus |> List.replace j'0 j'k
          | (-1.0, InSeq Jnplus) -> Jnplus |> List.without [j'0]
          | ( 1.0, _) -> j'k :: Jnplus 
          | (-1.0, _) -> Jnplus
          | _ -> failwith "No more variants expected."
  
        let Jnminus' = Jn' |> List.without Jnplus'
        
        loop (newDelta, Jb', Jnplus', Jnminus')
      
  loop (delta, Jb, Jnplus, Jnminus)

let dualSimplex (A, b, c, d) =
  let d = { down = fst d; up = snd d }
  let task = { A = A; b = b; c = c; d = d }

  dualSimplexImpl task