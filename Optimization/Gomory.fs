module Optimization.Gomory
#nowarn "0049"

open RDotNet
open RProvider.linprog
open RProvider.``base``
open RProvider.RDotNetExtensions

type private indices = int list
type private plan = indices * rowvec  

type private task = 
  { A: matrix;
    b: vector;
    c: rowvec;
    I: indices }

let private isInt x = equal x (round x)

let private solve task =
  let namedParams = RProvider.Helpers.namedParams
  let rbind vector = R.rbind(paramArray = vector)
  let rvector items = items |> Array.ofSeq

  let rmatrix matrix =
    matrix
    |> Matrix.rows
    |> Array.ofSeq
    |> Array.map (Array.ofSeq >> box) 
    |> rbind 
 
  let { A = A; b = b; c = c; } = task
  let m, n = A.NumRows, A.NumRows

  let res = 
    namedParams [
      "cvec", box <| rvector c ;
      "bvec", box <| rvector b;
      "Amat", box <| rmatrix A;
      "lpSolve", box <| true;
      "maximum", box <| true;
      "const.dir", box <| Array.create m "==" ]
    |> R.solveLP

  let getJb plan = 
    let isJb J =
      try
        A 
        |> Matrix.sliceCols J
        |> Matrix.inv 
        |> ignore
        true
      with
      | _ -> false 
    
    let rec brutJb cur rest length =
      if Seq.length cur = length then
        if isJb cur then Seq.singleton cur
        else Seq.empty
      else
        seq { 
          for i in 0.. Seq.length rest - 1 do 
            let ind = rest |> Seq.nth i 
            let rest' = 
              rest 
              |> Seq.take i
              |> List.ofSeq

            yield! brutJb (ind::cur) rest' length
        }

    let zeroInd, nonZeroInd = 
     plan
     |> List.ofSeq
     |> List.mapi (fun i el -> i, el)
     |> List.partition (snd >> ((=) 0.0))
     |> Tuple.map (List.map fst)

    brutJb nonZeroInd zeroInd m
    |> Seq.head  

  try
    let x = 
      res.AsList().["solution"].AsNumeric()
      |> RowVector.ofSeq

    Some (getJb x, x) 
  with 
  | _ -> None

let private updateTask (plan: plan) task = 
  let { A = A; b = b; c = c; I = I } = task
  let m, n = A.NumRows, A.NumCols
  let J, (Jb, x) = [0..n - 1], plan
  let A'b = A |> Matrix.sliceCols Jb
  let B = Matrix.inv A'b 

  let i'0, xi'0 = 
    Jb 
    |> Seq.filter (fun j -> I |> Seq.exists ((=) j))
    |> Seq.map (fun j -> j, x.[j]) 
    |> Seq.find (not << isInt << snd)
  
  let y = B.Row i'0

  let Au, beta = 
    (y * A, y * b) 
    |> fun (fst, snd) -> 
         let getFract x = floor x - x
         let getRowFract = RowVector.map getFract
         (getRowFract fst, getFract snd)

  let m', n' = m + 1, n + 1
  let A' = 
    A
    |> Matrix.appendRow Au
    |> Matrix.appendCol (Vector.E m' (m' - 1))

  let b' = b |> Vector.append [beta]
  let c' = c |> RowVector.append [0.0]

  { A = A'; b = b'; c = c'; I = I }

let rec private gomoryImpl task =
  let { A = A; b = b; c = c; I = I } = task 
  let plan = solve task
  let isSolution = RowVector.items I >> Seq.forall isInt

  match plan with
  | None -> None
  | Some (_, x) when isSolution x -> Some (x)  
  | Some plan -> 
      task
      |> updateTask plan 
      |> gomoryImpl

let gomory (A, b, c, I) = 
  let task = { A = A; b = b; c = c; I = I}
  gomoryImpl task