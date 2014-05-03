module Optimization.Tests.Bellman

open System.Collections.Generic
open Optimization.Bellman

module Dictionary = 
  let set (key, value) (dict: Dictionary<'a, 'b>) =
    dict.[key] <- value 
    dict

let createDict pairs =
  let createPair (key, value) =
    KeyValuePair (key, value)
  
  (Dictionary<'a, 'b> (), pairs)
  ||> Seq.fold (fun acc pair -> Dictionary.set pair acc)

let createF table  = 
  let dict = createDict table
  fun x -> dict.[x]

let case0 () = 
  let f1 = createF [(0, 0); (1, 3); (2, 4); (3, 5); (4, 8);  (5, 9);  (6, 10)]
  let f2 = createF [(0, 0); (1, 2); (2, 3); (3, 7); (4, 9);  (5, 12); (6, 13)]
  let f3 = createF [(0, 0); (1, 1); (2, 2); (3, 6); (4, 11); (5, 11); (6, 13)]

  let functions = [f1; f2; f3]
  let c = 6

  ([1; 1; 4], (functions, c))

let case1 () = 
  let f1 = createF [(0, 0); (1, 1); (2, 2); (3, 2); (4, 4); (5, 5); (6, 6)]
  let f2 = createF [(0, 0); (1, 2); (2, 3); (3, 5); (4, 7); (5, 7); (6, 8)]
  let f3 = createF [(0, 0); (1, 2); (2, 4); (3, 5); (4, 6); (5, 7); (6, 7)]

  let functions = [f1; f2; f3]
  let c = 6

  ([0; 4; 2], (functions, c))

let case2 () = 
  let f1 = createF [(0, 0); (1, 1); (2, 1); (3, 4); (4, 6); (5, 10); (6, 11)]
  let f2 = createF [(0, 0); (1, 2); (2, 3); (3, 5); (4, 6); (5, 7);  (6, 13)]
  let f3 = createF [(0, 0); (1, 1); (2, 4); (3, 4); (4, 7); (5, 8);  (6, 9)]

  let functions = [f1; f2; f3]
  let c = 6

  ([0; 6; 0], (functions, c))

let case3 () = 
  let f1 = createF [(0, 0); (1, 1); (2, 2); (3, 4); (4, 8); (5, 9); (6, 9);  (7, 23)]
  let f2 = createF [(0, 0); (1, 2); (2, 4); (3, 6); (4, 6); (5, 8); (6, 10); (7, 11)]
  let f3 = createF [(0, 0); (1, 3); (2, 4); (3, 7); (4, 7); (5, 8); (6, 8);  (7, 24)]

  let functions = [f1; f2; f3]
  let c = 7

  ([0; 0; 7], (functions, c))

let case4 () = 
  let f1 = createF [(0, 0); (1, 3); (2, 3); (3, 6); (4, 7); (5, 8); (6, 9);  (7, 14)]
  let f2 = createF [(0, 0); (1, 2); (2, 4); (3, 4); (4, 5); (5, 6); (6, 8);  (7, 13)]
  let f3 = createF [(0, 0); (1, 1); (2, 1); (3, 2); (4, 3); (5, 3); (6, 10); (7, 11)]

  let functions = [f1; f2; f3]
  let c = 7

  ([7; 0; 0], (functions, c))

let case5 () = 
  let f1 = createF [(0, 0); (1, 2); (2, 2); (3, 3); (4, 5); (5, 8);  (6, 8);  (7, 10); (8, 17)]
  let f2 = createF [(0, 0); (1, 1); (2, 2); (3, 5); (4, 8); (5, 10); (6, 11); (7, 13); (8, 15)]
  let f3 = createF [(0, 0); (1, 4); (2, 4); (3, 5); (4, 6); (5, 7);  (6, 13); (7, 14); (8, 14)]
  let f4 = createF [(0, 0); (1, 1); (2, 3); (3, 6); (4, 9); (5, 10); (6, 11); (7, 14); (8, 16)]

  let functions = [f1; f2; f3; f4]
  let c = 8

  ([0; 4; 1; 3], (functions, c))

let case6 () = 
  let f1 = createF [(0, 0); (1, 1); (2, 3); (3, 4); (4, 5); (5, 8); (6, 9);  (7, 9); (8, 11); (9, 12); (10, 12); (11, 14)]
  let f2 = createF [(0, 0); (1, 1); (2, 2); (3, 3); (4, 3); (5, 3); (6, 7);  (7, 12); (8, 13); (9, 14); (10, 17); (11, 19)]
  let f3 = createF [(0, 0); (1, 4); (2, 4); (3, 7); (4, 7); (5, 8); (6, 12); (7, 14); (8, 14); (9, 16); (10, 18); (11, 22)]
  let f4 = createF [(0, 0); (1, 5); (2, 5); (3, 5); (4, 7); (5, 9); (6, 13); (7, 13); (8, 15); (9, 15); (10, 19); (11, 24)]

  let functions = [f1; f2; f3; f4]
  let c = 11

  ([2; 7; 1; 1], (functions, c))

let case7 () = 
  let f1 = createF [(0, 0); (1, 4); (2, 4); (3, 6); (4, 9); (5, 12); (6, 12); (7, 15); (8, 16); (9, 19); (10, 19); (11, 19);]
  let f2 = createF [(0, 0); (1, 1); (2, 1); (3, 1); (4, 4); (5, 7);  (6, 8);  (7, 8);  (8, 13); (9, 13); (10, 19); (11, 20); ]
  let f3 = createF [(0, 0); (1, 2); (2, 5); (3, 6); (4, 7); (5, 8);  (6, 9);  (7, 11); (8, 11); (9, 13); (10, 13); (11, 18);]
  let f4 = createF [(0, 0); (1, 1); (2, 2); (3, 4); (4, 5); (5, 7);  (6, 8);  (7, 8);  (8, 9) ; (9, 9);  (10, 15); (11, 19); ]
  let f5 = createF [(0, 0); (1, 2); (2, 5); (3, 7); (4, 8); (5, 9);  (6, 10); (7, 10); (8, 11); (9, 14); (10, 17); (11, 21);]

  let functions = [f1; f2; f3; f4; f5]
  let c = 11

  ([7; 0; 2; 0; 2], (functions, c))

let case8 () = 
  let f1 = createF [(0, 0); (1, 1); (2, 2); (3, 2); (4, 2); (5, 3); (6, 5); (7, 8);  (8, 9);  (9, 13); (10, 14);]
  let f2 = createF [(0, 0); (1, 1); (2, 3); (3, 4); (4, 5); (5, 5); (6, 7); (7, 7);  (8, 10); (9, 12); (10, 12);]
  let f3 = createF [(0, 0); (1, 2); (2, 2); (3, 3); (4, 4); (5, 6); (6, 6); (7, 8);  (8, 9);  (9, 11); (10, 17);]
  let f4 = createF [(0, 0); (1, 1); (2, 1); (3, 1); (4, 2); (5, 3); (6, 9); (7, 9);  (8, 11); (9, 12); (10, 15);]
  let f5 = createF [(0, 0); (1, 2); (2, 7); (3, 7); (4, 7); (5, 9); (6, 9); (7, 10); (8, 11); (9, 12); (10, 13);]
  let f6 = createF [(0, 0); (1, 2); (2, 5); (3, 5); (4, 5); (5, 6); (6, 6); (7, 7);  (8, 12); (9, 18); (10, 22);]

  let functions = [f1; f2; f3; f4; f5; f6]
  let c = 10

  ([0; 0; 0; 0; 0; 10], (functions, c))

let test (expected, data) =
  let actual = data |> bellman
  actual |> expected.Equals

let cases =
  [
   case0;
   case1;
   case2;
   case3;
   case4;
   case5;
   case6;
   case7;
   case8;
  ]

let results =
  cases
  |> Seq.map ((|>) ())
  |> Seq.map test
  |> Seq.mapi (fun i res -> i, res)
  |> Seq.groupBy snd
  |> Seq.toList

for key, group in results do
  if not key then
    let fails = 
      group 
      |> Seq.map fst
      |> Seq.fold (fun acc cur -> sprintf "%s %d," acc cur) "Tests failed:"
    
    failwith fails
