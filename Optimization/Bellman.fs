module Optimization.Bellman

let rec private bellmanImpl functions y =
  match functions with
  | [] -> failwith "not expected"
  | f :: [] -> 
      [0..y]
      |> Seq.mapi (fun i z -> [i], f z)
      |> Seq.maxBy snd
  | f :: tail ->
      [0..y]
      |> Seq.mapi (fun i z -> 
           let prevIndices, prevMax = bellmanImpl tail (y - z)
           i :: prevIndices, f z + prevMax
         )
      |> Seq.maxBy snd

let bellman (functions, c) = 
  let functions' = functions |> List.rev
  
  bellmanImpl functions' c
  |> fst
  |> List.rev
