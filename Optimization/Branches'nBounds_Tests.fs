module Optimization.Tests.BranchesAndBounds

open Optimization.BranchesAndBounds
open Optimization.FloatOperators

let case0 () = 
  let A = matrix [[ 1.0; 0.0;  1.0; 0.0; 0.0; 1.0;];
                  [ 1.0; 2.0; -1.0; 1.0; 1.0; 2.0;];
                  [-2.0; 4.0;  1.0; 0.0; 1.0; 0.0; ]]
  
  let b = vector [-3.0; 
                  3.0;
                  13.0]
  
  let c = rowvec [-3.0; 2.0; 0.0; -2.0; -5.0; 2.0;]
  let d = ([-2.0; -1.0; -2.0; 0.0; 1.0; -4.0;],
           [ 2.0;  3.0;  1.0; 5.0; 4.0; -1.0;])
  
  let I  = [0..A.NumCols - 1]

  let task = (A, b, c, d, I)
  let expected = None
  
  (task, expected)

let case1 () = 
  let A = matrix [[1.0; 0.0; 0.0; 12.0; 1.0; -3.0;  4.0; -1.0;];
                  [0.0; 1.0; 0.0; 11.0; 12.0; 3.0;  5.0;  3.0 ];
                  [0.0; 0.0; 1.0; 1.0; 0.0;  22.0; -2.0;  1.0; ]]
  
  let b = vector [40.0; 
                  107.0;
                  61.0]
  
  let c = rowvec [2.0; 1.0; -2.0; -1.0; 4.0; -5.0; 5.0; 5.0]
  let d = ([0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0;],
           [3.0; 5.0; 5.0; 3.0; 4.0; 5.0; 6.0; 3.0;])
  
  let I  = [0..A.NumCols - 1]
  
  let task = (A, b, c, d, I)
  let expected = None
  
  (task, expected)

let case2 () = 
  let A = matrix [[1.0; -3.0;  2.0; 0.0; 1.0; -1.0; 4.0; -1.0; 0.0];
                  [1.0; -1.0;  6.0; 1.0; 0.0; -2.0; 2.0;  2.0; 0.0];
                  [2.0;  2.0; -1.0; 1.0; 0.0; -3.0; 8.0; -1.0; 1.0];
                  [4.0;  1.0;  0.0; 0.0; 1.0; -1.0; 0.0; -1.0; 1.0];
                  [1.0;  1.0;  1.0; 1.0; 1.0;  1.0; 1.0;  1.0; 1.0];]
  
  let b = vector [3.0; 
                  9.0;
                  9.0;
                  5.0;
                  9.0]
  
  let c = rowvec [-1.0; 5.0; -2.0; 4.0; 3.0; 1.0; 2.0; 8.0; 3.0]
  let d = ([0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0],
           [5.0; 5.0; 5.0; 5.0; 5.0; 5.0; 5.0; 5.0; 5.0])
  
  let I  = [0..A.NumCols - 1]

  let task = (A, b, c, d, I)
  let expected = None
  
  (task, expected)

let case3 () = 
  let A = matrix [[1.0; 0.0; 0.0; 12.0; 1.0; -3.0;  4.0; -1.0; 2.5; 3.0];
                  [0.0; 1.0; 0.0; 11.0; 12.0; 3.0;  5.0;  3.0; 4.0; 5.1];
                  [0.0; 0.0; 1.0; 1.0;  0.0; 22.0; -2.0;  1.0; 6.1; 7.0];]
  
  let b = vector [43.5; 
                  107.3;
                  106.3;]
  
  let c = rowvec [2.0; 1.0; -2.0; -1.0; 4.0; -5.0; 5.0; 5.0; 1.0; 2.0]
  let d = ([0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0],
           [2.0; 4.0; 5.0; 3.0; 4.0; 5.0; 4.0; 4.0; 5.0; 6.0])
  
  let I  = [0..A.NumCols - 1]
  
  let task = (A, b, c, d, I)
  let expected = None
  
  (task, expected)

let case4 () = 
  let A = matrix [[4.0; 0.0; 0.0; 0.0;  0.0; -3.0;  4.0; -1.0; 2.0; 3.0];
                  [0.0; 1.0; 0.0; 0.0;  0.0; 3.0;   5.0;  3.0; 4.0; 5.1];
                  [0.0; 0.0; 1.0; 0.0;  0.0; 22.0; -2.0;  1.0; 6.0; 7.0];
                  [0.0; 0.0; 0.0; 1.0;  0.0; 6.0;  -2.0;  7.0; 5.0; 6.0];
                  [0.0; 0.0; 0.0; 0.0;  1.0; 5.0;  5.0;   1.0; 6.0; 7.0];
                 ]
  
  let b = vector [8.0; 
                  5.0;
                  4.0;
                  7.0;
                  8.0;]
  
  let c = rowvec [2.0; 1.0; -2.0; -1.0; 4.0; -5.0; 5.0; 5.0; 1.0; 2.0]
  let d = ([0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0],
           [10.0; 10.0; 10.0; 10.0; 10.0; 10.0; 10.0; 10.0; 10.0; 10.0])
  
  let I  = [0..A.NumCols - 1]
  
  let task = (A, b, c, d, I)
  let expected = None
  
  (task, expected)

let case5 () = 
  let A = matrix [[1.0; -5.0; 3.0; 1.0; 0.0; 0.0;]   
                  [4.0; -1.0; 1.0; 0.0; 1.0; 0.0;]   
                  [2.0;  4.0; 2.0; 0.0; 0.0; 1.0;]]   
  
  let b = vector [ -8.0;  
                   22.0; 
                   30.0;]
  
  let c = rowvec [7.0; -2.0; 6.0; 0.0; 5.0; 2.0;]
  let d = ([2.0; 1.0; 0.0; 0.0; 1.0; 1.0;],
           [6.0; 6.0; 5.0; 2.0; 4.0; 6.0;])
  
  let I  = [0..A.NumCols - 1]
  
  let task = (A, b, c, d, I)
  let expected = None
  
  (task, expected)

let case6 () = 
  let A = matrix [[1.0; 0.0; 0.0; 3.0;  1.0; -3.0;  4.0; -1.0;]  
                  [0.0; 1.0; 0.0; 4.0; -3.0;  3.0;  5.0;  3.0;]  
                  [0.0; 0.0; 1.0; 1.0;  0.0;  2.0; -2.0;  1.0;]]   
  
  let b = vector [ 30.0;  
                   78.0; 
                   18.0;]
  
  let c = rowvec [2.0; 1.0; -2.0; -1.0; 4.0; -5.0; 5.0; 5.0; ]
  let d = ([0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0;],
           [5.0; 5.0; 3.0; 4.0; 5.0; 6.0; 6.0; 8.0;])
  
  let I  = [0..A.NumCols - 1]
  
  let task = (A, b, c, d, I)
  let expected = None
  
  (task, expected)

let case7 () = 
  let A = matrix [[1.0; -3.0;  2.0; 0.0; 1.0; -1.0; 4.0; -1.0; 0.0;]  
                  [1.0; -1.0;  6.0; 1.0; 0.0; -2.0; 2.0;  2.0; 0.0;]  
                  [2.0;  2.0; -1.0; 1.0; 0.0; -3.0; 2.0; -1.0; 1.0;]  
                  [4.0;  1.0;  0.0; 0.0; 1.0; -1.0; 0.0; -1.0; 1.0;]  
                  [1.0;  1.0;  1.0; 1.0; 1.0;  1.0; 1.0;  1.0; 1.0;]]   
  
  let b = vector [18.0;  
                  18.0; 
                  30.0;
                  15.0;
                  18.0;]

  let c = rowvec [7.0; 5.0; -2.0; 4.0; 3.0; 1.0; 2.0; 8.0; 3.0; ]
  let d = ([0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0;],
           [8.0; 8.0; 8.0; 8.0; 8.0; 8.0; 8.0; 8.0; 8.0;])
  
  let I  = [0..A.NumCols - 1]
  
  let task = (A, b, c, d, I)
  let expected = None
  
  (task, expected)

let case8 () = 
  let A = matrix [
                   [1.0; 0.0; 1.0; 0.0;  4.0; 3.0;  4.0;]
                   [0.0; 1.0; 2.0; 0.0; 55.0; 3.5;  5.0;]
                   [0.0; 0.0; 3.0; 1.0;  6.0; 2.0; -2.5;]
                 ]   
  
  let b = vector [
                  26.0;  
                  185.0; 
                  32.5;
                 ]

  let c = rowvec [1.0; 2.0; 3.0; -1.0; 4.0; -5.0; 6.0; ]
  let d = ([0.0; 1.0; 0.0; 0.0; 0.0; 0.0; 0.0; ],
           [1.0; 2.0; 5.0; 7.0; 8.0; 4.0; 2.0; ])
  
  let I  = [0..A.NumCols - 1]
  
  let task = (A, b, c, d, I)
  let expected = None
  
  (task, expected)

let case9 () = 
  let A = matrix [
                   [2.0; 0.0; 1.0; 0.0; 0.0; 3.0;  5.0] 
                   [0.0; 2.0; 2.1; 0.0; 0.0; 3.5;  5.0]  
                   [0.0; 0.0; 3.0; 2.0; 0.0; 2.0;  1.1]  
                   [0.0; 0.0; 3.0; 0.0; 2.0; 2.0; -2.5]
                 ]   
  
  let b = vector [
                  58.0;
                  66.3;
                  36.7;
                  13.5;
                 ]

  let c = rowvec [1.0; 2.0; 3.0; 1.0; 2.0; 3.0; 4.0;]
  let d = ([1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; ],
           [2.0; 3.0; 4.0; 5.0; 8.0; 7.0; 7.0; ])
  
  let I  = [0..A.NumCols - 1]
  
  let task = (A, b, c, d, I)
  let expected = None
  
  (task, expected)

let case10 () = 
  let A = matrix [[1.0; 0.0; 0.0;  1.0; 1.0; -3.0;  4.0; -1.0;  3.0; 3.0;]  
                  [0.0; 1.0; 0.0; -2.0; 1.0;  1.0;  7.0;  3.0;  4.0; 5.0;] 
                  [0.0; 0.0; 1.0;  1.0; 0.0;  2.0; -2.0;  1.0; -4.0; 7.0;]]   
  
  let b = vector [27.0;
                  6.0; 
                  18.0;]

  let c = rowvec [-2.0; 1.0; -2.0; -1.0; 8.0; -5.0; 3.0; 5.0; 1.0; 2.0;]
  let d = ([0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0 ],
           [8.0; 7.0; 6.0; 7.0; 8.0; 5.0; 6.0; 7.0; 8.0; 5.0; ])
  
  let I  = [0..A.NumCols - 1]
  
  let task = (A, b, c, d, I)
  let expected = None
  
  (task, expected)

let test data = 
  let (i, task, expected) = data
  let result = task |> branchesAndBounds

  match expected, result with
  | (None, None) -> true
  | (Some _, None) -> false
  | (None, Some _) -> false
  | (Some expected, Some result) -> 
    
    if not (result |> equalSeq expected) then false
    else true

let cases = [case0;
   case1;
   case2;
   case3;
   case4;
   case5;
   case6;
   case7;
   case8;
   case9;
   case10;]

let results =
  cases
  |> Seq.map ((|>) ())
  |> Seq.mapi (fun i (task, expected) -> (i, task, expected))
  |> Seq.map test

let succeed = results |> Seq.forall id

if not succeed then
  let fails = 
    results
    |> Seq.mapi (fun i res -> i, res) 
    |> Seq.filter (id << not << snd)
    |> Seq.map fst 
    |> Seq.fold (fun st i -> sprintf "%s №%d," st i) ""

  failwith <| sprintf "Tests %s failed" fails