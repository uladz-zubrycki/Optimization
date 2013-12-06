module Linear.Tests

open Linear
open Xunit
open FsUnit.Xunit

[<Fact>]
let ``some test`` () = 
  5 |> should equal 5

[<Fact>]
let ``some another test`` () = 
  5 |> should equal 6