module ParseSetTests

open Expecto
open NpgsqlFSharpParser

let testSet inputQuery expected =
    test inputQuery {
        match Parser.parse inputQuery with
        | Ok (Expr.SetQuery query) ->
            Expect.equal query expected "The query is parsed correctly"
        | Ok somethingElse ->
            failwithf "Unexpected set statement %A" somethingElse
        | Error errorMsg ->
            failwith errorMsg
    }

let ftestSet inputQuery expected =
    ftest inputQuery {
        match Parser.parse inputQuery with
        | Ok (Expr.SetQuery query) ->
            Expect.equal query expected "The query is parsed correctly"
        | Ok somethingElse ->
            failwithf "Unexpected set statement %A" somethingElse
        | Error errorMsg ->
            failwith errorMsg
    }

[<Tests>]
let setQueryTests = testList "Parse SET tests" [

    testSet "SET DateStyle = ISO" {
        SetExpr.Default with
            Parameter = "DateStyle"
            Value = Some (Expr.Ident "ISO")
    }

    testSet "SET timezone TO 'Europe/Rome'" {
        SetExpr.Default with
            Parameter = "timezone"
            Value = Some (Expr.StringLiteral "Europe/Rome")
    }
]

