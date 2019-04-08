module Tests

open Expecto
open TaxLib

[<Tests>]
let tests =
  testList "Tax Tests" [
    testCase "trivial" <| fun _ ->
      Expect.equal "a" "a" "a"
  ]
