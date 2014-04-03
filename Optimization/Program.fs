open Optimization
open Optimization.Utils

let test (A: matrix, b: vector, c: rowvec, d: (float list * float list), J: int list, expected: rowvec option, result: rowvec option) = 
  match result, expected with
  | None, None -> ()
  | None, _ -> failwith "Expected plan, but not found"
  | Some(curX), None -> failwith "No plan expected but found"
  | Some(curX), Some(expX) -> 
    let expectedProfit = expX |> RowVector.dot c 
    let profit = curX |> RowVector.dot c 
  
    let newB = A |> Matrix.rows 
                 |> Seq.map (fun row -> 
                      RowVector.dot row curX
                    )
                 |> Vector.ofSeq

    let expNewB = A |> Matrix.rows 
                    |> Seq.map (fun row -> 
                         RowVector.dot row expX
                       )
                    |> Vector.ofSeq

    if not (equalSeq newB b) then
      failwith "Not a decision"

    if not (equalSeq expNewB b) then
      failwith "Methoda fail"

    if not (equalSeq curX expX) then
      failwith "Another answer"
    
    if bigger expectedProfit profit then
      failwith "Expected profit is bigger"

let solve (A, b, c, d, expected) = 
  let n = A |> Matrix.colCount
  let J = [0..n - 1]
  let result = dualSimplex (A, b, c, d, J)

  test (A, b, c, d, J, expected, result)


let task0 () =
  let A = matrix [[2.0; 1.0; -1.0; 0.0; 0.0; 1.0];
                  [1.0; 0.0;  1.0; 1.0; 0.0; 0.0;];
                  [0.0; 1.0;  0.0; 0.0; 1.0; 0.0;]]
  
  let b = vector [2.0; 
                  5.0; 
                  0.0]
  
  let c = rowvec [3.0; 2.0; 0.0; 3.0; -2.0; -4.0;]
  let d = ([0.0; -1.0; 2.0; 1.0; -1.0; 0.0],
           [2.0;  4.0; 4.0; 3.0;  3.0; 5.0])
  
  let expected = Some (rowvec [1.5; 1.0; 2.0; 1.5; -1.0; 0.0])

  (A, b, c, d, expected)

let task1 () =
  let A = matrix [[1.0; -5.0; 3.0; 1.0; 0.0; 0.0];
                  [4.0; -1.0; 1.0; 0.0; 1.0; 0.0;];
                  [2.0; 4.0;  2.0; 0.0; 0.0; 1.0;]]
  
  let b = vector [-7.0; 
                  22.0; 
                  30.0]
  
  let c = rowvec [7.0; -2.0; 6.0; 0.0; 5.0; 2.0;]
  let d = ([2.0; 1.0; 0.0; 0.0; 1.0; 1.0],
           [6.0;  6.0; 5.0; 2.0;  4.0; 6.0])
  
  let expected = Some (rowvec [5.0; 3.0; 1.0; 0.0; 4.0; 6.0;])

  (A, b, c, d, expected)
  

let task2 () =
  let A = matrix [[1.0; 0.0; 2.0;  2.0; -3.0; 3.0];
                  [0.0; 1.0; 0.0; -1.0;  0.0; 1.0;];
                  [1.0; 0.0; 1.0;  3.0;  2.0; 1.0;]]
  
  let b = vector [15.0; 
                  0.0; 
                  13.0]
  
  let c = rowvec [3.0; 0.5; 4.0; 4.0; 1.0; 5.0;]
  let d = ([0.0; 0.0; 0.0; 0.0; 0.0; 0.0],
           [3.0; 5.0; 4.0; 3.0; 3.0; 4.0;])
  
  let expected = Some (rowvec [3.0; 0.0; 4.0; 1.1818; 0.6364; 1.1818])
  (A, b, c, d, expected)

let tasks = [task0;
             task1; 
             task2]
tasks 
|> List.iter (fun task -> 
     solve <| task ()
   )


