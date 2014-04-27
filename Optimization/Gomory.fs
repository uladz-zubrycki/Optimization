module Optimization.Gomory
#nowarn "0049"

open RDotNet
open RProvider.linprog
open RProvider.``base``
open RProvider.RDotNetExtensions

type private indices = int list
type private task = matrix  *                 // A
                    vector  *                 // b
                    rowvec  *                 // c
                    indices *                 // J
                    indices                   // I

type private plan = indices *                 // Jb
                    rowvec                    // x

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
 
  let A, b, c, _, _ = task
  let cvec = rvector c 
  let bvec = rvector b 
  let Amat = rmatrix A
  let const_dir = Array.create bvec.Length "=="

  let res = 
    namedParams [
      "cvec", box cvec;
      "bvec", box bvec;
      "Amat", box Amat;
      "lpSolve", box true;
      "maximum", box true;
      "const.dir", box const_dir]
    |> R.solveLP

  try
    let x = 
      res.AsList().["solution"].AsNumeric()
      |> RowVector.ofSeq

    Some ([], x)
  with 
  | _ -> None

let private updateTask (plan: plan) (task: task) = 
  let A, b, c, J, I = task
  let m, n = A.NumRows, A.NumCols
  let Jb, x  = plan
  let Ab = A |> Matrix.sliceCols Jb
  let B = Matrix.inv Ab

  let i'0, xi'0 = 
    Jb 
    |> Seq.filter (fun j -> Seq.exists ((=) j) I)
    |> Seq.map (fun j -> j, x.[j]) 
    |> Seq.find (not << isInt << snd)
  
  let y = 
    List.findIndex ((=) i'0) Jb
    |> RowVector.E m
    |> (fun E -> E * B)

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
  let J' = J @ [n' - 1]
  let I' = I @ [n' - 1]

  (A', b', c', J', I')

let rec gomory (task: task) = 
  let A, b, c, J, I = task 
  let plan = solve task
  let isSolution = RowVector.items I >> Seq.forall isInt

  match plan with
  | None -> None
  | Some (_, x) when isSolution x -> Some(x)  
  | Some plan -> 
      task
      |> updateTask plan 
      |> gomory