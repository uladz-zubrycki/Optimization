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
  Simplex method for transportation theory task.

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
let simplex A c b J =
  printfn "Resolving task:"
  printfn ""
  printfn "Matrix A:"
  printfn "%A" A
  printfn ""
  printfn "Indices J = %A" J
  printfn "Vector c = %A" c
  printfn "Vector b = %A" b
  printfn ""
  printfn ""

  /// Simplex method algorithm, when there is initial plan 
  let rec simplexImpl A c J plan = 
    printfn "\n================================================================================="
    printfn "Start iteration:"

    let n, m = A |> Matrix.colCount, A |> Matrix.rowCount
    let (x: vector), J'b = plan                         // initial 'x'-vector and basis indices
    let J'n = J |> List.without J'b                     // nonbasis indices
    let A'b = A |> Matrix.fromColumns J'b               // basis matrix
    let c'b = c |> Vector.items J'b                     // basis 'c' vector
    
    printfn ""
    printfn "n = %d" n
    printfn "m = %d" m
    printfn ""
    printfn "Indices J = %A" J
    printfn "Basis indices J'b = %A" J'b
    printfn "Nonbasis indices J'n = %A" J'n
    printfn ""
    printfn "Vector x = %A" x
    printfn "Vector b = %A" b
    printfn ""
    printfn "Matrix A:" 
    printfn "%A" A
    printfn ""
    printfn "Matrix A'b:" 
    printfn "%A" A'b
    printfn ""
    
    let B =  Matrix.inv A'b                             // inversed basis matrix
    let u = c'b.Transpose * B                           // vector of potential

   

    let delta = J |> List.map (fun j ->                 // deltas vector
                        let A'j = A.Column j
                        u * A'j - c.[j]
                     )
                  |> Vector.ofList
    
    printfn ""
    printfn "Inversed matrix B:" 
    printfn "%A" B
    printfn ""
    printfn "Potentials vector u = c'b * B" 
    printfn "%A" u
    printfn "Delta(j) = u * A(j) - c(j)"   
    printfn "%A" delta  
    printfn ""
  
    match delta with
    | BiggerOrEqual 0.0 ->   
        // no negative number in delta -> solution found                           
        Some plan                                         
    | _ ->  
        let j'0 = J'n                                     
                  |> List.map (fun j -> j, delta.[j])
                  |> List.filter (snd >> ((>) 0.0))
                  |> List.minBy fst
                  |> fst
        
        let z = B * A.Column j'0
                      
        printfn ""
        printfn "Delta negative index j'0 = %d" j'0
        printfn "Vector z = B * A(j'0)" 
        printfn "%A" z  
        printfn ""

        match z with
        | LessOrEqual 0.0 ->    
            printfn "Criterial funtion is unlimited"
            None                         
        | _ ->
            let s, theta = 
              [0..m-1]
              |> Seq.filter (fun i -> z.[i] > 0.0)
              |> Seq.map (fun i ->
                   let j = J'b.[i] 
                   i, x.[j] / z.[i]
                 )
              |> Seq.minBy snd

            printfn ""
            printfn "Creating new plan"
            printfn "j'0 = %d" j'0
            printfn "s = %d" s
            printfn "j's = %d" J'b.[s]
            printfn "theta = %f" theta
            
            // new plan 'x'-vector
            let newX = 
              J |> List.map (fun j ->              
                     match j with
                     | Equals j'0 -> 
                         printfn "New x(%d) = %f (theta as x(j'0))" j theta
                         theta
                     | In J'n -> 
                        printfn "New x(%d) = 0.0 (nonbasis)" j
                        0.0
                     | _ ->
                         let x'j = x.[j]
                         let i = J'b |> List.findIndex ((=) j) 
                         let newX = x'j - theta * z.[i]
                         printfn "New x(%d) = %f - %f * %f = %f (basis)" j x'j theta z.[i] newX
                         
                         newX
                   )
                |> Vector.ofList
            
            // new plan indices
            let newJ'b =                           
              J'b |> List.mapi (fun i j -> 
                       match i with
                       | Equals s -> j'0
                       | _ -> j
                     )

            printfn "New basis indices = (J'b \ j's) U j'0"
            printfn "%A" newJ'b
            printfn ""
            printfn ""

            simplexImpl A c J (newX, newJ'b) 

  /// Simplex method first phase
  let firstPhase A J b = 
    printfn "Simplex method first phase"
    let n, m = A |> Matrix.colCount, A |> Matrix.rowCount
    let J'bu = [n.. n + m - 1]              // synthetic basis indices
    let J'u = J @ J'bu                      // synthetic indices
    let E = Matrix.identity m   
    let A'u = Matrix.augment A E            // synthetic matrix
    let c'u = Vector.create (n + m) -1.0    // synthetic c vector
    let x'u = b |> Vector.append (Vector.zero n)  // synthetic initial plan

    simplexImpl A'u c'u J'u (x'u, J'bu)
  
  /// Change rows signs, to be all items of 'b' positive
  let updateSigns A b =
    printfn "Updating signs of 'A' matrix and 'b' vector"
    printfn "Matrix A:" 
    printfn "%A" A
    printfn ""
    printfn "Vector b = %A" b
    printfn ""
    
    let A = A |> Matrix.rows
              |> Seq.mapi (fun i row ->
                   let b'i = Vector.get b i
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
  | None -> 
      printfn "No solution for first phase => no solution"
      None
  | Some(x'u, J'bu) ->
      //TODO: some solution analisys here
      let x = x'u |> Vector.items J
      let J'b = J'bu
      
      printfn ""
      printfn "First phase solution analisys."
      printfn "Indices J'bu = %A" J'bu
      printfn "Vector x'u = %A" x'u
      printfn "Vector x = %A" x
      printfn ""
      
      printfn "Starting symplex method second phase."
      simplexImpl A c J (x, J'b)

// Example   
//let A = matrix [[3.0;  1.0; 1.0; 0.0];
//                [1.0; -2.0; 0.0; 1.0]]
//let b = vector [1.0; 1.0]
//let c = vector [1.0; 4.0; 1.0; -1.0]
//let J = [0..3]

let A = matrix [[7.0; 4.0; 1.0;]]
let b = vector [13.0;]
let c = vector [21.0; 11.0; 0.0;]
let J = [0..A.NumCols - 1]

//let A = matrix [[7.0;    4.0;      1.0;   1.0]
//                [0.0; -4.0/7.0; -1.0/7.0; 1.0]]
//let b = vector [13.0; -6.0/7.0]
//let c = vector [21.0; 11.0; 0.0; 0.0]
//let J = [0..A.NumCols - 1]

//let A = matrix [[7.0;    4.0;      1.0;      0.0;   0.0;];
//                [0.0; -4.0/7.0; -1.0/7.0;    1.0;   0.0;];
//                [0.0;    0.0;   -1.0/4.0; -1.0/4.0; 1.0]]
//
//let b = vector [13.0; -6.0/7.0; -1.0/2.0]
//let c = vector [21.0;   11.0;     0.0;   0.0; 0.0]
//let J = [0..A.NumCols - 1]

//let A = matrix [[1.0;  1.0;  0.0; 2.0;  0.0;];
//                [0.0; -1.0;  1.0; 0.0;  2.0;];
//                [1.0;  0.0; -1.0; 1.0; -2.0;]]
//let b = vector [3.0; 1.0; -1.0]
//let c = vector [1.0; 1.0; 1.0; 1.0; 1.0;]
//let J = [0..4]


match simplex A c b J with
| None -> 
    printfn "No solution"
    ()
| Some(x, J'b) ->
    printfn " Solution found:"
    printfn "  x = %A" (x)   
    printfn "  J = %A" (J'b)
    ()

System.Console.ReadLine |> ignore







