module Optimization.BranchesAndBounds
open Optimization.DualSimplex

#nowarn "0049"

type private indices = int list

type private restrictions = 
  { down: float list; 
    up: float list }         

type private task = 
  { A: matrix;
    b: vector;
    c: rowvec;
    d: restrictions;
    I: indices }

type private tasks = task list
type private plan = rowvec * float    // plan and profit

type private solution = 
  | Found of plan
  | NotFound

type private iterCtx = tasks *        // unsolved tasks
                       solution       // best solution

let private solveTask task = 
  let { A = A; b = b; c = c; d = d } = task
  let { down = down; up = up } = d
  let profit = RowVector.dot c 

  dualSimplex (A, b, c, (down, up))
  |> Option.map (fun (_, x) -> (x, profit x))

let private splitTask i x task =
  let { d = d } = task
  let { up = up; down = down } = d

  let int = floor x
  let down' = List.set i (int + 1.0) down
  let up' = List.set i int up  
  let d1, d2 = { d with down = down' }, { d with up = up' }

  { task with d = d1 } :: {task with d = d2} :: []

let private branchesAndBoundsImpl task =
  let {A = A; b = b; c = c; d = d; I = I} = task 
  let m, n = A.NumRows, A.NumCols
  let isInt value = equal value (round value)
  let isSolution = RowVector.items I >> Seq.forall isInt
   
  let rec loop (ctx: iterCtx) = 
    match ctx with
    | [], NotFound -> None
    | [], Found(x, _) -> Some(x)  
    | (cur :: other, best) -> 
        match best, solveTask cur with
        | NotFound, Some (x, profit) 
          when isSolution x -> 
            (other, Found(x, profit))
        | Found (_, bestProfit), Some (_, profit) 
          when profit <= bestProfit -> 
            (other, best)
        | Found (_, bestProfit), Some (x, profit) 
          when isSolution x -> 
            (other, Found(x, profit))
        | _, Some (x, profit) ->
            let (j'0, xj'0) = 
              I
              |> Seq.map (fun j -> (j, x.[j]))
              |> Seq.find (not << isInt << snd)
            
            let tasks' = 
              cur
              |> splitTask j'0 xj'0
              |> (@) other
            
            (tasks', best)
        | _, None -> (other, best)
        |> loop 

  ([task], NotFound) |> loop 

let branchesAndBounds (A, b, c, d, I) =
  let d = { down = fst d; up = snd d }
  let task = { A = A; b = b; c = c; d = d; I = I }
  branchesAndBoundsImpl task