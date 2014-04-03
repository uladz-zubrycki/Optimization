open Optimization
open Optimization.Utils

let task1 () =
  let A = matrix [[2.0; 1.0; -1.0; 0.0; 0.0; 1.0];
                  [1.0; 0.0;  1.0; 1.0; 0.0; 0.0;];
                  [0.0; 1.0;  0.0; 0.0; 1.0; 0.0;]]
  
  let b = vector [2.0; 
                  5.0; 
                  0.0]
  
  let c = rowvec [3.0; 2.0; 0.0; 3.0; -2.0; -4.0;]
  let d = ([0.0; -1.0; 2.0; 1.0; -1.0; 0.0],
           [2.0;  4.0; 4.0; 3.0;  3.0; 5.0])
  
  let J = [0..5]
  
  let plan = dualSimplex (A, b, c, d, J)
  match plan with
  | Some(x) -> 
      let xx = rowvec [1.5; 1.0; 2.0; 1.5; -1.0; 0.0]
      let profitxx = RowVector.dot c xx
      let profit = RowVector.dot c x
      ()
  | None ->
    ()

let task2 ()=
  let A = matrix [[1.0;  0.0; 3.0; 1.0; 0.0; 0.0];
                  [0.0; -1.0; 1.0; 1.0; 1.0; 2.0;];
                  [-2.0; 4.0; 2.0; 0.0; 0.0; 1.0;]]
  
  let b = vector [10.0; 
                  8.0; 
                  10.0]
  
  let c = rowvec [7.0; -2.0; 6.0; 0.0; 5.0; 2.0;]
  let d = ([0.0; 1.0; -1.0; 0.0; -2.0; 1.0],
           [3.0;  3.0; 6.0; 2.0;  4.0; 6.0])
  
  let J = [0..5]
  
  let plan = dualSimplex (A, b, c, d, J)
  match plan with
  | Some(x) -> 
      let profit = RowVector.dot c x
      let newB = A |> Matrix.rows 
                   |> Seq.map (fun row -> 
                        RowVector.dot row x
                      )
                   |> Vector.ofSeq

      ()
  | None ->
    ()

let task3 ()=
  let A = matrix [[1.0;  0.0; 3.0; 1.0; 0.0; 0.0];
                  [0.0; -1.0; 1.0; 1.0; 1.0; 2.0;];
                  [-2.0; 4.0; 2.0; 0.0; 0.0; 1.0;]]
  
  let b = vector [10.0; 
                  8.0; 
                  10.0]
  
  let c = rowvec [7.0; -2.0; 6.0; 0.0; 5.0; 2.0;]
  let d = ([0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0],
           [3.0; 5.0; 5.0; 1.0; 4.0; 5.0; 6.0; 3.0])
  
  let J = [0..5]
  
  let plan = dualSimplex (A, b, c, d, J)
  match plan with
  | Some(x) -> 
      let profit = RowVector.dot c x
      let newB = A |> Matrix.rows 
                   |> Seq.map (fun row -> 
                        RowVector.dot row x
                      )
                   |> Vector.ofSeq

      ()
  | None ->
    ()

task1 ()

