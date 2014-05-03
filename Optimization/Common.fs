namespace Optimization

[<AutoOpen>]
module Tuple = begin
  let snd3 (x, y, z) = y
  
  let map f (fst, snd) =
    (f fst, f snd)
end

[<AutoOpen>]
module FloatOperators = begin
  let precision = pown 0.1 3
  
  let equalPrec precision x y = abs(x - y) < precision 
  let equal = equalPrec precision 
  let notEqual x y = not (equal x y) 
  
  let cond op x y = op x y && notEqual x y
  let condOrEqual op x y = op x y || equal x y
  
  let condSeq op (fst: float seq) (snd: float seq) = 
   (fst, snd) 
   ||> Seq.map2 op
   |> Seq.forall id

  let bigger = cond (>)
  let biggerOrEqual = condOrEqual (>)
  let less = cond (<)
  let lessOrEqual = condOrEqual (<)
  let equalSeq x y = condSeq equal x y
  let biggerSeq x y = condSeq bigger
  let biggerOrEqualSeq x y = (x, y) ||> condSeq biggerOrEqual 
  let lessSeq x y = (x, y) ||> condSeq less
  let lessOrEqualSeq x y = (x, y) ||> condSeq lessOrEqual
end

[<AutoOpen>]
module Recognizers = begin
  let private matchBool = 
    function
    | true -> Some()
    | false -> None

  let private check (predicate: 'a -> 'b -> bool) x y =
    (x, y) 
    ||> predicate 
    |> matchBool

  let (|Equal|_|) value target = check (=) target value
  let (|Less|_|) value target = check (<) target value 
  let (|Bigger|_|) value target = check (>) target value  
  let (|Lessf|_|) value target = check less target value 
  let (|BiggerOrEqualf|_|) value target = check biggerOrEqual target value 

  let (|InSeq|_|) value target = 
    value 
    |> Seq.exists ((=) target) 
    |> matchBool
end

module List = begin
  let without (items: 'a list) (list: 'a list) =
    list
    |> List.filter (fun i ->
         not (items |> List.exists ((=) i))
       )   

  let replace target value list =
    list 
    |> List.map(fun i -> 
         if i = target then value 
         else i
       )

  let set index value list = 
    list 
    |> List.mapi (fun i x -> 
         if i = index then value
         else x
       )
end

[<RequireQualifiedAccess>]
module Vector = begin
  let concat (fst: vector) (snd: vector) = 
    [fst; snd]
    |> Seq.concat 
    |> Vector.ofSeq
  
  let items indices vector =
    indices
    |> Seq.map (Vector.get vector) 
    |> Vector.ofSeq

  let transpose (vector: vector) = vector.Transpose
  
  let append items vector = 
    Seq.append vector items
    |> Vector.ofSeq   

  let E length index = 
    Vector.init length (fun i ->
      if i = index then 1.0
      else 0.0
    )
end

[<RequireQualifiedAccess>]
module RowVector = begin
  let toList (row: rowvec) = row |> List.ofSeq

  let append items (row: rowvec) = 
    items
    |> Seq.append row
    |> RowVector.ofSeq

  let concat (first: rowvec) (second: rowvec) = 
    second
    |> Seq.append first 
    |> RowVector.ofSeq
  
  let map mapper (row: rowvec) =
    row
    |> Seq.map mapper
    |> RowVector.ofSeq
  
  let div num row = 
    if num = 0.0 then invalidArg "num" "Can't divide by zero."
    row |> map (fun el -> el / num)

  let items indices rowvec =
    indices
    |> Seq.map (RowVector.get rowvec) 
    |> RowVector.ofSeq

  let E length index = 
    RowVector.init length (fun i ->
      if i = index then 1.0 
      else 0.0 
    ) 

  let dot (row1: rowvec) (row2: rowvec) =
    (row1, row2)
    ||> Seq.map2 (*)
    |> Seq.sum
end

[<RequireQualifiedAccess>]
module Matrix = begin
  let ofRows (rows: rowvec seq) = Matrix.ofSeq rows 
  
  let ofColumns (columns: vector seq) = 
    columns 
    |> Matrix.ofSeq
    |> Matrix.transpose

  let colCount (matrix: matrix) = matrix.NumCols
  let rowCount (matrix: matrix) = matrix.NumRows

  let sliceCols indices (matrix: matrix) =
    indices 
    |> List.map (matrix.Column) 
    |> ofColumns

  let sliceRows indices (matrix: matrix) =
    indices 
    |> List.map (matrix.Row) 
    |> ofRows

  let rows (matrix: matrix) = 
    [0..matrix.NumRows - 1] 
    |> Seq.map matrix.Row

  let columns (matrix: matrix) =
    [0..matrix.NumCols - 1]
    |> Seq.map matrix.Column

  let isSquare (matrix: matrix) = matrix.NumCols = matrix.NumRows

  let exchangeRows first second (matrix:matrix) =
    if first < 0 || first > matrix.NumRows then
      invalidArg "first" "invalid index" 

    if second < 0 || first > matrix.NumRows then
      invalidArg "second" "invalid index" 
    
    if first = second then
      matrix
    else
      let getRow = function
        | Equal first -> matrix.Row second
        | Equal second -> matrix.Row first
        | x -> matrix.Row x

      [0..matrix.NumRows - 1]
      |> Seq.map getRow
      |> ofRows
   
  let augment (first:matrix) (second:matrix) = 
    if first.NumRows <> second.NumRows then
        failwith "Must have same count of rows."

    let concatRowPair pair = 
      pair ||> RowVector.concat 

    (rows first, rows second)
    ||> Seq.zip
    |> Seq.map concatRowPair
    |> ofRows

  let appendRow (row: rowvec) matrix = 
    Seq.append (rows matrix) [row]
    |> ofRows

  let appendCol (col: vector) matrix = 
    Seq.append (columns matrix) [col]
    |> ofColumns

  let inv (matrix: matrix) = 
    if not (isSquare matrix) then
      invalidArg "matrix" "must be square"

    let removeZero ind (matrix:matrix) =
      matrix.Column ind
      |> Seq.skip ind
      |> Seq.tryFindIndex ((<>) 0.0)
      |> function 
         | None -> failwith "Determinant is zero, cause matrix contains zero column"
         | Some(foundInd) when foundInd = 0 -> matrix
         | Some(foundInd) -> 
             matrix |> exchangeRows ind (foundInd + ind)
    
    let forward (matrix: matrix) = 
      [0..matrix.NumRows - 1]
      |> Seq.fold (fun (acc: matrix) ind -> 
           let rows = 
             acc
             |> removeZero ind
             |> rows
          
           let curRow = rows |> Seq.nth ind 
           let curRow = curRow |> RowVector.div curRow.[ind] 
          
           let top = rows |> Seq.take ind
           let bottom = 
             rows
             |> Seq.skip (ind + 1)
             |> Seq.map (fun row -> 
                   curRow * row.[ind] 
                   |> (-) row 
                )
          
           bottom
           |> Seq.append [curRow] 
           |> Seq.append top
           |> ofRows  
         ) 
         matrix 

    let backward (matrix: matrix) =
      [matrix.NumRows - 1..-1..0]
      |> Seq.fold (fun (acc: matrix) ind ->
           let rows = acc |> rows 
           let curRow = rows |> Seq.nth ind 
           
           let top = 
               rows 
               |> Seq.take ind
               |> Seq.map (fun row ->
                     curRow * row.[ind]
                     |> (-) row
                  )
           
           let bottom = Seq.skip ind rows 
           
           Seq.append top bottom |> ofRows
         )
         matrix

    let addIdentity matrix =
      augment matrix <|
        (matrix.NumCols
         |> Matrix.identity) 

    let dim = matrix.NumCols

    matrix
    |> addIdentity
    |> forward
    |> backward
    |> sliceCols [dim..dim * 2 - 1]
end
