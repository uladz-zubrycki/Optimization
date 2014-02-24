#if INTERACTIVE
#r @"bin\Debug\FSharp.PowerPack.dll"
#load "EqualityRecognizers.fs" 
#load "Set.fs"
#load "Vector.fs" 
#load "RowVector.fs"
#load "List.fs"
#load "Matrix.fs" 
#endif

open Linear

(*
  Task:
    n: int - count of product types.
    m: int - count of resource types.
    b: vector - count of recources available.
    c: vector - profit from products. Length = n.
    a: matrix - cost of product production. Dimensions = (m,n).
    x: vector - plan. satisfies all conditions.
    J: indices - set of indeces for x vector. Length = n.

    Task canonical form:
    c * x -> max
    x >= 0
    A * x = b
*)

// Example   
//let A = matrix [[3.0;  1.0; 1.0; 0.0];
//                 [1.0; -2.0; 0.0; 1.0]]
//let b = vector [1.0; 1.0]
//let c = vector [1.0; 4.0; 1.0; -1.0]
//let J = [0..3]

//let A = matrix [[7.0; 4.0; 1.0;]]
//let b = vector [13.0;]
//let c = vector [21.0; 11.0; 0.0;]
//let J = [0..A.NumCols - 1]

//let A = matrix [[7.0;    4.0;      1.0;   1.0]
//                [0.0; -4.0/7.0; -1.0/7.0; 1.0]]
//let b = vector [13.0; -6.0/7.0]
//let c = vector [21.0; 11.0; 0.0; 0.0]
//let J = [0..A.NumCols - 1]

let A = matrix [[7.0;    4.0;      1.0;      0.0;   0.0;];
                [0.0; -4.0/7.0; -1.0/7.0;    1.0;   0.0;];
                [0.0;    0.0;   -1.0/4.0; -1.0/4.0; 1.0]]

let b = vector [13.0; -6.0/7.0; -1.0/2.0]
let c = vector [21.0;   11.0;     0.0;   0.0; 0.0]
let J = [0..A.NumCols - 1]

//let A = matrix [[1.0;  1.0;  0.0; 2.0;  0.0;];
//                [0.0; -1.0;  1.0; 0.0;  2.0;];
//                [1.0;  0.0; -1.0; 1.0; -2.0;]]
//let b = vector [3.0; 1.0; -1.0]
//let c = vector [1.0; 1.0; 1.0; 1.0; 1.0;]
//let J = [0..4]


let simplex A c (b:vector) J =
  let rec simplexImpl A c J plan = 
    let n, m = A |> Matrix.colCount, A |> Matrix.rowCount
    let (x: vector), J'b = plan
    let J'n = J |> List.without J'b
    let A'b = A |> Matrix.fromColumns J'b
    let c'b = c |> Vector.items J'b
    
    let B =  Matrix.inv A'b 
    let u = c'b.Transpose * B  
    
    let delta = J 
                |> List.map (fun j -> 
                        let A'j = A.Column j
                        u * A'j - c.[j]
                   )
                |> Vector.ofList
  
    match delta with
    | BiggerOrEqual 0.0 -> Some plan
    | _ ->
      let j'0 = J'n 
                |> List.map (fun j -> j, delta.[j])
                |> List.filter (snd >> ((>) 0.0))
                |> List.minBy fst
                |> fst

      let z = B * A.Column j'0
   
      match z with
      | LessOrEqual 0.0 -> None
      | _ ->
        let s, theta = [0..m-1]
                       |> Seq.filter (fun i -> z.[i] > 0.0)
                       |> Seq.map (fun i ->
                            let j = J'b.[i] 
                            i, x.[j] / z.[i]
                          )
                       |> Seq.minBy snd

        let newX = J 
                   |> List.map (fun j -> 
                        match j with
                        | Equals j'0 -> theta
                        | In J'n -> 0.0
                        | _ ->
                            let x'j = x.[j]
                            let i = J'b |> List.findIndex ((=) j) 
                            x'j - theta * z.[i]
                      )
                   |> Vector.ofList
        
        let newJ'b = J'b 
                     |> List.mapi (fun i j -> 
                          match i with
                          | Equals s -> j'0
                          | _ -> j
                        )

        simplexImpl A c J (newX, newJ'b) 

  let firstPhase A J (b: vector) = 
    let n, m = A |> Matrix.colCount, A |> Matrix.rowCount
    let J'bu = [n.. n + m - 1]
    let J'u = J @ J'bu
    let E = Matrix.identity m 
    let A'u = Matrix.augment A E
    let c'u = Vector.create (n + m) -1.0
    let x'u = b |> Vector.append (Vector.zero n)

    simplexImpl A'u c'u J'u (x'u, J'bu)

  let updateSigns A (b: vector) =
    let A = A
              |> Matrix.rows
              |> Seq.mapi (fun i row ->
                   let b'i = b.[i]
                   match b'i with
                   | Less 0.0 -> row * -1.0
                   | _ -> row
                 )
              |> Matrix.ofRows

    let b = b |> Vector.map (fun item -> 
                   match item with 
                   | Less 0.0 -> -1.0 * item 
                   | _ -> item
                 )
    (A, b)

  let A, b = updateSigns A b

  match firstPhase A J b with
  | None -> None
  | Some(x'u, J'bu) ->

    //TODO: some solution analisys here
    let x = x'u |> Vector.items J
    let J'b = J'bu

    simplexImpl A c J (x, J'b)
//  simplexImpl A c plan

match simplex A c b J with
| None -> ()
| Some(x, J'b) ->
    let t = 42
    ()

//let result = simplex A c b J 
//match result with
//| Some plan -> printf "plan is %A" plan
//| None -> printf "has no plan"
//
//System.Console.ReadLine |> ignore


