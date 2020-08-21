module ParserDeleteTests

open Expecto
open NpgsqlFSharpParser

let testDelete inputQuery expected =
    test inputQuery {
        match Parser.parse inputQuery with
        | Ok (Expr.DeleteQuery query) ->
            Expect.equal query expected "The query is parsed correctly"
        | Ok somethingElse ->
            failwithf "Unexpected delete statement %A" somethingElse
        | Error errorMsg ->
            failwith errorMsg
    }

let ftestDelete inputQuery expected =
    ftest inputQuery {
        match Parser.parse inputQuery with
        | Ok (Expr.DeleteQuery query) ->
            Expect.equal query expected "The query is parsed correctly"
        | Ok somethingElse ->
            failwithf "Unexpected delete statement %A" somethingElse
        | Error errorMsg ->
            failwith errorMsg
    }

[<Tests>]
let deleteQueryTests = testList "Parse DELETE tests" [
    testDelete "DELETE FROM users WHERE last_login IS NULL" {
        DeleteExpr.Default with
            Table = "users"
            Where = Some (Expr.Equals(Expr.Null, Expr.Ident "last_login"))
    }

    testDelete "DELETE FROM users WHERE luck = 'bad' RETURNING *" {
        DeleteExpr.Default with
            Table = "users"
            Where = Some (Expr.Equals(Expr.Ident "luck", Expr.StringLiteral "bad"))
            Returning = [Expr.Star]
    }
]
