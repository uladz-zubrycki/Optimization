module Optimization.Bellman

open System
open System.Collections.Concurrent

#nowarn "40"

let private memoize f =
  let cache = new ConcurrentDictionary<'a, 'b> ()
  let accessor = Func<'a, 'b> (fun key -> f key)

  fun args -> cache.GetOrAdd (args, accessor)

let rec private bellmanImpl = memoize (fun (functions, y) ->
  match functions with
  | [] -> failwith "not expected"
  | f :: [] ->
      [0..y]
      |> Seq.map (fun z -> [z], f z)
      |> Seq.maxBy snd
  | f :: tail ->
      [0..y]
      |> Seq.map (fun z -> 
           let prevIndices, prevMax = bellmanImpl (tail, (y - z))
           z :: prevIndices, f z + prevMax
         )
      |> Seq.maxBy snd
)

let bellman (functions, c) = 
  let functions' = functions |> List.rev

  bellmanImpl (functions', c)
  |> fst
  |> List.rev
  