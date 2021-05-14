module ParseFetchTests

open Expecto
open NpgsqlFSharpParser

let testFetch inputQuery expected =
    test inputQuery {
        match Parser.parse inputQuery with
        | Ok (Expr.FetchQuery query) ->
            Expect.equal query expected "The query is parsed correctly"
        | Ok somethingElse ->
            failwithf "Unexpected fetch statement %A" somethingElse
        | Error errorMsg ->
            failwith errorMsg
    }

let ftestFetch inputQuery expected =
    ftest inputQuery {
        match Parser.parse inputQuery with
        | Ok (Expr.FetchQuery query) ->
            Expect.equal query expected "The query is parsed correctly"
        | Ok somethingElse ->
            failwithf "Unexpected fetch statement %A" somethingElse
        | Error errorMsg ->
            failwith errorMsg
    }

[<Tests>]
let fetchQueryTests = testList "Parse FETCH queries" [
    testFetch "FETCH 10 FROM c1" {
        FetchExpr.Default with
            CursorName = "c1"
            Direction = Direction.Forward 10
    }
]
