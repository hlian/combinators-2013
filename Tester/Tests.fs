module Tester.Tests

open FParsec
open System
open NUnit.Framework

open Tester.Main

let go parser text =
    match run parser text with
        | Success (r, _, _) -> r
        | Failure (m, _, _) -> raise (Exception m)

[<Test>]
let testNumber() =
    Assert.AreEqual(go parser "123", JNumber 123.)

[<Test>]
let testArray() =
    Assert.AreEqual(go parser "[1,2,3]", JList [JNumber 1.; JNumber 2.; JNumber 3.])

[<Test>]
let testArrayWithSpaces() =
    Assert.AreEqual(go parser "[1, 2   ,   3    ]", JList [JNumber 1.; JNumber 2.; JNumber 3.])

[<Test>]
let testString() =
    Assert.AreEqual(go parser "\"hello\"", JString "hello")

[<Test>]
let testObject() =
    Assert.AreEqual(
        go parser "{\"1\": 2, \"abc\" : \"def\" }",
        Map.ofList [("1", JNumber 2.); ("abc", JString "def")] |> JObject)

[<EntryPoint>]
let main args = 0
