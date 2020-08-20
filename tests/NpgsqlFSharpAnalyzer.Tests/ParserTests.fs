module ParserTests

open Expecto
open NpgsqlFSharpParser

let testSelect query expected =
    test query {
        match Parser.parse query with
        | Ok (Expr.Query (TopLevelExpr.Select query)) ->
            Expect.equal query expected "The query is parsed correctly"
        | Ok somethingElse ->
            failwithf "Unexpected select statement %A" somethingElse
        | Error errorMsg ->
            failwith errorMsg
    }

let ftestSelect query expected =
    ftest query {
        match Parser.parse query with
        | Ok (Expr.Query (TopLevelExpr.Select query)) ->
            Expect.equal query expected "The query is parsed correctly"
        | Ok somethingElse ->
            failwithf "Unexpected select statement %A" somethingElse
        | Error errorMsg ->
            failwith errorMsg
    }

[<Tests>]
let parserTests = ftestList "Parser tests" [

    testSelect "SELECT NOW()" {
        SelectExpr.Default with
            Columns = [Expr.Function("NOW", [])]
    }

    testSelect "SELECT NOW();" {
        SelectExpr.Default with
            Columns = [Expr.Function("NOW", [])]
    }

    testSelect "SELECT username, user_id FROM users" {
        SelectExpr.Default with
            Columns = [Expr.Ident "username"; Expr.Ident "user_id"]
            From = Some (Expr.Ident "users") 
    }

    testSelect "SELECT DISTINCT username, user_id FROM users" {
        SelectExpr.Default with
            Columns = [Expr.Ident "username"; Expr.Ident "user_id"]
            From = Some (Expr.Ident "users") 
    }

    testSelect "SELECT DISTINCT ON (username, user_id) FROM users" {
        SelectExpr.Default with
            Columns = [Expr.Ident "username"; Expr.Ident "user_id"]
            From = Some (Expr.Ident "users") 
    }

    testSelect "SELECT COUNT(*) FROM users" {
        SelectExpr.Default with
            Columns = [Expr.Function("COUNT", [Expr.Star]) ]
            From = Some (Expr.Ident "users") 
    }

    testSelect "SELECT COUNT(*) FROM users WHERE last_login > @login_date" {
        SelectExpr.Default with
            Columns = [Expr.Function("COUNT", [Expr.Star]) ]
            From = Some (Expr.Ident "users")
            Where = Some (Expr.GreaterThan(Expr.Ident "last_login", Expr.Parameter "@login_date"))
    }

    testSelect "SELECT COUNT(*) FROM users WHERE last_login > NOW();" {
        SelectExpr.Default with
            Columns = [Expr.Function("COUNT", [Expr.Star]) ]
            From = Some (Expr.Ident "users")
            Where = Some (Expr.GreaterThan(Expr.Ident "last_login", Expr.Function("NOW", [ ])))
    }

    testSelect "SELECT * FROM users WHERE user_id = @user_id" {
        SelectExpr.Default with
            Columns = [Expr.Star]
            From = Some (Expr.Ident "users")
            Where = Some (Expr.Equals(Expr.Ident "user_id", Expr.Parameter "@user_id"))
    }

    testSelect """
        SELECT value, timestamp
        FROM meters
        WHERE timestamp >= @from AND timestamp < @to
    """ {
        SelectExpr.Default with
            Columns = [Expr.Ident "value"; Expr.Ident "timestamp"]
            From = Some (Expr.Ident "meters")
            Where = Some (Expr.And(Expr.GreaterThanOrEqual(Expr.Ident "timestamp", Expr.Parameter "@from"), Expr.LessThan(Expr.Ident "timestamp", Expr.Parameter "@to")))
    }

    testSelect """
        SELECT username, email
        FROM users
        WHERE user_id IN (SELECT id FROM user_ids WHERE id IS NOT NULL)
    """ {
        SelectExpr.Default with
            Columns = [Expr.Ident "username"; Expr.Ident "email"]
            From = Some (Expr.Ident "users")
            Where = Some (Expr.In(Expr.Ident "user_id", Expr.Query(TopLevelExpr.Select {
                SelectExpr.Default with
                    Columns = [Expr.Ident "id"]
                    From = Some (Expr.Ident "user_ids")
                    Where = Some(Expr.Not(Expr.Equals(Expr.Null, Expr.Ident "id")))
            })))
    }

    testSelect """
        SELECT username, email
        FROM users
        JOIN meters ON meters.user_id = users.user_id
        WHERE user_id IN (SELECT id FROM user_ids WHERE id IS NOT NULL)
    """ {
        SelectExpr.Default with
            Columns = [Expr.Ident "username"; Expr.Ident "email"]
            From = Some (Expr.Ident "users")
            Joins = [JoinExpr.InnerJoin("meters", Expr.Equals(Expr.Ident "meters.user_id", Expr.Ident "users.user_id"))]
            Where = Some (Expr.In(Expr.Ident "user_id", Expr.Query(TopLevelExpr.Select {
                SelectExpr.Default with
                    Columns = [Expr.Ident "id"]
                    From = Some (Expr.Ident "user_ids")
                    Where = Some(Expr.Not(Expr.Equals(Expr.Null, Expr.Ident "id")))
            })))
    }

    testSelect """
        SELECT username, email
        FROM users
        INNER JOIN meters ON meters.user_id = users.user_id
        WHERE user_id IN (SELECT id FROM user_ids WHERE id IS NOT NULL)
    """ {
        SelectExpr.Default with
            Columns = [Expr.Ident "username"; Expr.Ident "email"]
            From = Some (Expr.Ident "users")
            Joins = [JoinExpr.InnerJoin("meters", Expr.Equals(Expr.Ident "meters.user_id", Expr.Ident "users.user_id"))]
            Where = Some (Expr.In(Expr.Ident "user_id", Expr.Query(TopLevelExpr.Select {
                SelectExpr.Default with
                    Columns = [Expr.Ident "id"]
                    From = Some (Expr.Ident "user_ids")
                    Where = Some(Expr.Not(Expr.Equals(Expr.Null, Expr.Ident "id")))
            })))
    }

    testSelect """
        SELECT username, email
        FROM users
        JOIN meters ON meters.user_id = users.user_id
        LEFT JOIN utilities ON utilities.id = users.user_id
        WHERE user_id IN (SELECT id FROM user_ids WHERE id IS NOT NULL)
    """ {
        SelectExpr.Default with
            Columns = [Expr.Ident "username"; Expr.Ident "email"]
            From = Some (Expr.Ident "users")
            Joins = [
                JoinExpr.InnerJoin("meters", Expr.Equals(Expr.Ident "meters.user_id", Expr.Ident "users.user_id"))
                JoinExpr.LeftJoin("utilities", Expr.Equals(Expr.Ident "utilities.id", Expr.Ident "users.user_id"))
            ]
            Where = Some (Expr.In(Expr.Ident "user_id", Expr.Query(TopLevelExpr.Select {
                SelectExpr.Default with
                    Columns = [Expr.Ident "id"]
                    From = Some (Expr.Ident "user_ids")
                    Where = Some(Expr.Not(Expr.Equals(Expr.Null, Expr.Ident "id")))
            })))
    }

    testSelect """
        SELECT username, email
        FROM users
        INNER JOIN meters ON meters.user_id = users.user_id
        LEFT JOIN utilities ON utilities.id = users.user_id
        WHERE user_id IN (
            SELECT id FROM user_ids
            WHERE id IS NOT NULL
        )
    """ {
        SelectExpr.Default with
            Columns = [Expr.Ident "username"; Expr.Ident "email"]
            From = Some (Expr.Ident "users")
            Joins = [
                JoinExpr.InnerJoin("meters", Expr.Equals(Expr.Ident "meters.user_id", Expr.Ident "users.user_id"))
                JoinExpr.LeftJoin("utilities", Expr.Equals(Expr.Ident "utilities.id", Expr.Ident "users.user_id"))
            ]
            Where = Some (Expr.In(Expr.Ident "user_id", Expr.Query(TopLevelExpr.Select {
                SelectExpr.Default with
                    Columns = [Expr.Ident "id"]
                    From = Some (Expr.Ident "user_ids")
                    Where = Some(Expr.Not(Expr.Equals(Expr.Null, Expr.Ident "id")))
            })))
    }
]
