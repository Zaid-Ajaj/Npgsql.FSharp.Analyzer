module ParseDeclareTests

open Expecto
open NpgsqlFSharpParser

let testDeclare inputQuery expected =
    test inputQuery {
        match Parser.parse inputQuery with
        | Ok (Expr.DeclareQuery query) ->
            Expect.equal query expected "The query is parsed correctly"
        | Ok somethingElse ->
            failwithf "Unexpected declare statement %A" somethingElse
        | Error errorMsg ->
            failwith errorMsg
    }

let ftestDeclare inputQuery expected =
    ftest inputQuery {
        match Parser.parse inputQuery with
        | Ok (Expr.DeclareQuery query) ->
            Expect.equal query expected "The query is parsed correctly"
        | Ok somethingElse ->
            failwithf "Unexpected declare statement %A" somethingElse
        | Error errorMsg ->
            failwith errorMsg
    }

[<Tests>]
let declareQueryTests = testList "Parse DECLARE tests" [

    ftestDeclare "DECLARE c1 CURSOR FOR SELECT NOW();" ({
        Parameter = "c1"
        Query = Expr.SelectQuery { SelectExpr.Default with Columns = [Expr.Function("NOW", [])] }
    } |> Cursor)
]

