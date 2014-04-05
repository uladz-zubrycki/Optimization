namespace Optimization

open Optimization.Utils

[<AutoOpen>]
module BranchesAndBounds = begin
  [<AutoOpen>]
  module private Utils = begin
    type task = matrix *                      // A
                vector *                      // b
                rowvec *                      // c
                (float list * float list) *   // d'up * d'down
                int list *                    // J
                int list                      // I

    let isInt x = equal x (round x)
    let isIntSeq = Seq.forall isInt
  
    let (|AllInt|_|) x = 
      if isIntSeq x then Some ()
      else None
  end
  
  let private solveTask task = 
    let A, b, c, d, J, _ = task
    dualSimplex (A, b, c, d, J)

  let private splitTask index value task =
    let A, b, c, (d'down, d'up), J, I = task
    
    let int = floor value
    let d1 = (d'down |> List.set index (int + 1.0), d'up)
    let d2 = (d'down, d'up |> List.set index int)  
    let task1 = A, b, c, d1, J, I
    let task2 = A, b, c, d2, J, I
    
    [task1; task2] 

  let branchesAndBounds (task:task) =
    let A, b, c, d, J, I = task
    let m, n = A.NumRows, A.NumCols
    let profit = RowVector.dot c 

    let updateCtx ctx plan =
      let (tasks, r'0, m'0, M) = ctx
      let cur::other = tasks

      match (profit plan, plan) with
      | Lessf r'0, _ -> (other, r'0, m'0, M)
      | profit, AllInt -> (other, profit, 1, plan)
      | _, _ ->
        let (j'0, xj'0) = 
          I
          |> Seq.map (fun j -> (j, plan.[j]))
          |> Seq.find (not << isInt << snd)

        let newTasks = 
          cur
          |> splitTask j'0 xj'0
          |> (@) other
      
        (newTasks, r'0, m'0, M)

    let rec branchesAndBoundsImpl ctx = 
      let tasks, r'0, m'0, M = ctx 

      match (tasks, m'0) with
      | [], 0 -> None
      | [], 1 -> Some(M)  
      | cur :: other, _ ->
      
        cur
        |> solveTask
        |> function
           | None -> (other, r'0, m'0, M)
           | Some(plan) -> updateCtx ctx plan
        |> branchesAndBoundsImpl
      | _ -> failwith "Not expected"

    match I with
    | [] -> solveTask task
    | _ ->   
      let r'0 = -infinity
      let m'0 = 0
      let M = RowVector.zero n
      let tasks = [task]
       
      (tasks, r'0, m'0, M) |> branchesAndBoundsImpl 
end