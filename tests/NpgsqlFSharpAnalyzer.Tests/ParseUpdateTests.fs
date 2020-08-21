module ParseUpdateTests

open Expecto
open NpgsqlFSharpParser

let testUpdate inputQuery expected =
    test inputQuery {
        match Parser.parse inputQuery with
        | Ok (Expr.UpdateQuery query) ->
            Expect.equal query expected "The query is parsed correctly"
        | Ok somethingElse ->
            failwithf "Unexpected update statement %A" somethingElse
        | Error errorMsg ->
            failwith errorMsg
    }

let ftestUpdate inputQuery expected =
    ftest inputQuery {
        match Parser.parse inputQuery with
        | Ok (Expr.UpdateQuery query) ->
            Expect.equal query expected "The query is parsed correctly"
        | Ok somethingElse ->
            failwithf "Unexpected update statement %A" somethingElse
        | Error errorMsg ->
            failwith errorMsg
    }

[<Tests>]
let updateQueryTests = testList "Parse UPDATE queries" [
    testUpdate "UPDATE users SET enabled = @enabled WHERE user_id = @user_id" {
        UpdateExpr.Default with
            Table = "users"
            Where = Some(Expr.Equals(Expr.Ident "user_id", Expr.Parameter "@user_id"))
            Assignments = [
                Expr.Equals(Expr.Ident "enabled", Expr.Parameter "@enabled")
            ]
    }

    testUpdate "UPDATE users SET enabled = @enabled, roles = @roles WHERE user_id = @user_id" {
        UpdateExpr.Default with
            Table = "users"
            Where = Some(Expr.Equals(Expr.Ident "user_id", Expr.Parameter "@user_id"))
            Assignments = [
                Expr.Equals(Expr.Ident "enabled", Expr.Parameter "@enabled")
                Expr.Equals(Expr.Ident "roles", Expr.Parameter "@roles")
            ]
    }

    testUpdate "UPDATE users SET enabled = @enabled, roles = @roles WHERE user_id = @user_id RETURNING *" {
        UpdateExpr.Default with
            Table = "users"
            
            Assignments = [
                Expr.Equals(Expr.Ident "enabled", Expr.Parameter "@enabled")
                Expr.Equals(Expr.Ident "roles", Expr.Parameter "@roles")
            ]

            Where = Some(Expr.Equals(Expr.Ident "user_id", Expr.Parameter "@user_id"))

            Returning = [Expr.Star]
    }
]
