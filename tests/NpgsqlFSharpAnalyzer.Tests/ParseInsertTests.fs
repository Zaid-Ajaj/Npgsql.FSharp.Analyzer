module ParseInsertTests

open Expecto
open NpgsqlFSharpParser

let testInsert inputQuery expected =
    test inputQuery {
        match Parser.parse inputQuery with
        | Ok (Expr.InsertQuery query) ->
            Expect.equal query expected "The query is parsed correctly"
        | Ok somethingElse ->
            failwithf "Unexpected insert statement %A" somethingElse
        | Error errorMsg ->
            failwith errorMsg
    }

let ftestInsert inputQuery expected =
    ftest inputQuery {
        match Parser.parse inputQuery with
        | Ok (Expr.InsertQuery query) ->
            Expect.equal query expected "The query is parsed correctly"
        | Ok somethingElse ->
            failwithf "Unexpected insert statement %A" somethingElse
        | Error errorMsg ->
            failwith errorMsg
    }

[<Tests>]
let insertQueryTests = testList "Parse INSERT queries" [
    testInsert "INSERT INTO users (username, active) VALUES (@username, true)" {
        InsertExpr.Default with
            Table = "users"
            Columns = ["username"; "active"]
            Values = [ Expr.Parameter("@username"); Expr.Boolean true ]
    }

    testInsert "INSERT INTO users (username, active) VALUES (@username, true) RETURNING *" {
        InsertExpr.Default with
            Table = "users"
            Columns = ["username"; "active"]
            Values = [ Expr.Parameter("@username"); Expr.Boolean true ]
            Returning = [Expr.Star]
    }
]
