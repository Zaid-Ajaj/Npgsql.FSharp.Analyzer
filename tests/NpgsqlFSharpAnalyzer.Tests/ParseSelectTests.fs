module ParseSelectTests

open Expecto
open NpgsqlFSharpParser

let testSelect inputQuery expected =
    test inputQuery {
        match Parser.parse inputQuery with
        | Ok (Expr.SelectQuery query) ->
            Expect.equal query expected "The query is parsed correctly"
        | Ok somethingElse ->
            failwithf "Unexpected select statement %A" somethingElse
        | Error errorMsg ->
            failwith errorMsg
    }

let ftestSelect inputQuery expected =
    ftest inputQuery {
        match Parser.parse inputQuery with
        | Ok (Expr.SelectQuery query) ->
            Expect.equal query expected "The query is parsed correctly"
        | Ok somethingElse ->
            failwithf "Unexpected select statement %A" somethingElse
        | Error errorMsg ->
            failwith errorMsg
    }

[<Tests>]
let selectQueryTests = testList "Parse SELECT tests" [

    testSelect "SELECT NOW()" {
        SelectExpr.Default with
            Columns = [Expr.Function("NOW", [])]
    }

    testSelect "SELECT 1" {
        SelectExpr.Default with Columns = [Expr.Integer 1]
    }

    testSelect "SELECT ''" {
        SelectExpr.Default with Columns = [Expr.StringLiteral ""]
    }

    testSelect "SELECT 1::text" {
        SelectExpr.Default with Columns = [Expr.TypeCast(Expr.Integer 1, Expr.Ident "text")]
    }

    testSelect "SELECT @input::text, value::ltree" {
        SelectExpr.Default with
            Columns = [
                Expr.TypeCast(Expr.Parameter "@input", Expr.Ident "text")
                Expr.TypeCast(Expr.Ident "value", Expr.Ident "ltree")
            ]
    }

    testSelect "SELECT @input || 'hello', value::ltree" {
        SelectExpr.Default with
            Columns = [
                Expr.StringConcat(Expr.Parameter "@input", Expr.StringLiteral "hello")
                Expr.TypeCast(Expr.Ident "value", Expr.Ident "ltree")
            ]
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

    testSelect """SELECT * FROM "from" """ {
        SelectExpr.Default with
            Columns = [Expr.Star]
            From = Some (Expr.Ident "from")
    }

    testSelect "SELECT * FROM users" {
        SelectExpr.Default with
            Columns = [Expr.Star]
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

    testSelect "SELECT COUNT(*) AS user_count FROM users" {
        SelectExpr.Default with
            Columns = [Expr.As(Expr.Function("COUNT", [Expr.Star]), Expr.Ident("user_count")) ]
            From = Some (Expr.Ident "users")
    }

    testSelect "SELECT ename || empno AS EmpDetails, COALESCE(comm,0) AS TOTALSAL FROM sales" {
        SelectExpr.Default with
            Columns = [
                Expr.As(Expr.StringConcat(Expr.Ident "ename", Expr.Ident "empno"), Expr.Ident "EmpDetails")
                Expr.As(Expr.Function("COALESCE", [ Expr.Ident "comm"; Expr.Integer 0 ]), Expr.Ident "TOTALSAL")
            ]

            From = Some (Expr.Ident "sales")
    }

    testSelect "SELECT COUNT(*) AS user_count FROM users as u" {
        SelectExpr.Default with
            Columns = [Expr.As(Expr.Function("COUNT", [Expr.Star]), Expr.Ident("user_count")) ]
            From = Some (Expr.As(Expr.Ident "users", Expr.Ident "u"))
    }

    testSelect "SELECT COUNT(*) AS user_count FROM users u" {
        SelectExpr.Default with
            Columns = [Expr.As(Expr.Function("COUNT", [Expr.Star]), Expr.Ident("user_count")) ]
            From = Some (Expr.As(Expr.Ident "users", Expr.Ident "u"))
    }

    testSelect "SELECT COUNT(*) FROM users LIMIT 10" {
        SelectExpr.Default with
            Columns = [Expr.Function("COUNT", [Expr.Star]) ]
            From = Some (Expr.Ident "users")
            Limit = Some(Expr.Integer 10)
    }

    testSelect "SELECT COUNT(*) FROM users LIMIT @numberOfRows" {
        SelectExpr.Default with
            Columns = [Expr.Function("COUNT", [Expr.Star]) ]
            From = Some (Expr.Ident "users")
            Limit = Some(Expr.Parameter "@numberOfRows")
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
            Where = Some (Expr.In(Expr.Ident "user_id", Expr.SelectQuery {
                SelectExpr.Default with
                    Columns = [Expr.Ident "id"]
                    From = Some (Expr.Ident "user_ids")
                    Where = Some(Expr.Not(Expr.Equals(Expr.Null, Expr.Ident "id")))
            }))
    }

    testSelect """
        SELECT username, email
        FROM users
        WHERE last_login IS NOT NULL
        ORDER BY last_login
    """ {
        SelectExpr.Default with
            Columns = [Expr.Ident "username"; Expr.Ident "email"]
            From = Some (Expr.Ident "users")
            Where = Some (Expr.Not(Expr.Equals(Expr.Null, Expr.Ident "last_login")))
            OrderBy = [ Ordering.Asc("last_login") ]
    }

    testSelect """
        SELECT DISTINCT timestamp, value FROM meters
        WHERE meter_id = @meter_id AND timestamp >= @from
        ORDER BY timestamp
    """ {
        SelectExpr.Default with
            Columns = [Expr.Ident "timestamp"; Expr.Ident "value"]
            From = Some (Expr.Ident "meters")
            Where = Some (Expr.And(Expr.Equals(Expr.Ident "meter_id", Expr.Parameter "@meter_id"), Expr.GreaterThanOrEqual(Expr.Ident "timestamp", Expr.Parameter "@from")))
            OrderBy = [ Ordering.Asc("timestamp") ]
    }

    testSelect """
        SELECT username, email FROM users
        ORDER BY last_login DESC, user_id
    """ {
        SelectExpr.Default with
            Columns = [Expr.Ident "username"; Expr.Ident "email"]
            From = Some (Expr.Ident "users")
            OrderBy = [ Ordering.Desc("last_login"); Ordering.Asc "user_id" ]
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
            Where = Some (Expr.In(Expr.Ident "user_id", Expr.SelectQuery {
                SelectExpr.Default with
                    Columns = [Expr.Ident "id"]
                    From = Some (Expr.Ident "user_ids")
                    Where = Some(Expr.Not(Expr.Equals(Expr.Null, Expr.Ident "id")))
            }))
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
            Where = Some (Expr.In(Expr.Ident "user_id", Expr.SelectQuery {
                SelectExpr.Default with
                    Columns = [Expr.Ident "id"]
                    From = Some (Expr.Ident "user_ids")
                    Where = Some(Expr.Not(Expr.Equals(Expr.Null, Expr.Ident "id")))
            }))
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
            Where = Some (Expr.In(Expr.Ident "user_id", Expr.SelectQuery {
                SelectExpr.Default with
                    Columns = [Expr.Ident "id"]
                    From = Some (Expr.Ident "user_ids")
                    Where = Some(Expr.Not(Expr.Equals(Expr.Null, Expr.Ident "id")))
            }))
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
            Where = Some (Expr.In(Expr.Ident "user_id", Expr.SelectQuery {
                SelectExpr.Default with
                    Columns = [Expr.Ident "id"]
                    From = Some (Expr.Ident "user_ids")
                    Where = Some(Expr.Not(Expr.Equals(Expr.Null, Expr.Ident "id")))
            }))
    }

    testSelect """
        SELECT username, email
        FROM users
        INNER JOIN meters ON meters.user_id = users.user_id
        LEFT JOIN utilities ON utilities.id = users.user_id
        WHERE user_id IN (SELECT id FROM user_ids WHERE id IS NOT NULL)
        GROUP BY user_id, username
    """ {
        SelectExpr.Default with
            Columns = [Expr.Ident "username"; Expr.Ident "email"]
            From = Some (Expr.Ident "users")
            Joins = [
                JoinExpr.InnerJoin("meters", Expr.Equals(Expr.Ident "meters.user_id", Expr.Ident "users.user_id"))
                JoinExpr.LeftJoin("utilities", Expr.Equals(Expr.Ident "utilities.id", Expr.Ident "users.user_id"))
            ]
            Where = Some (Expr.In(Expr.Ident "user_id", Expr.SelectQuery {
                SelectExpr.Default with
                    Columns = [Expr.Ident "id"]
                    From = Some (Expr.Ident "user_ids")
                    Where = Some(Expr.Not(Expr.Equals(Expr.Null, Expr.Ident "id")))
            }))

            GroupBy = [Expr.Ident "user_id"; Expr.Ident "username"]
    }

    testSelect """
        SELECT username, email
        FROM users
        INNER JOIN meters ON meters.user_id = users.user_id
        LEFT JOIN utilities ON utilities.id = users.user_id
        WHERE user_id IN (SELECT id FROM user_ids WHERE id IS NOT NULL)
        GROUP BY user_id, username
        HAVING SUM(amount) > users.salary
    """ {
        SelectExpr.Default with
            Columns = [Expr.Ident "username"; Expr.Ident "email"]
            From = Some (Expr.Ident "users")
            Joins = [
                JoinExpr.InnerJoin("meters", Expr.Equals(Expr.Ident "meters.user_id", Expr.Ident "users.user_id"))
                JoinExpr.LeftJoin("utilities", Expr.Equals(Expr.Ident "utilities.id", Expr.Ident "users.user_id"))
            ]
            Where = Some (Expr.In(Expr.Ident "user_id", Expr.SelectQuery {
                SelectExpr.Default with
                    Columns = [Expr.Ident "id"]
                    From = Some (Expr.Ident "user_ids")
                    Where = Some(Expr.Not(Expr.Equals(Expr.Null, Expr.Ident "id")))
            }))

            GroupBy = [Expr.Ident "user_id"; Expr.Ident "username"]

            Having = Some(Expr.GreaterThan(Expr.Function("SUM", [Expr.Ident "amount"]), Expr.Ident "users.salary"))
    }

    testSelect """
        SELECT username, email
        FROM users
        INNER JOIN meters ON meters.user_id = users.user_id
        LEFT JOIN utilities ON utilities.id = users.user_id
        WHERE user_id IN (SELECT id FROM user_ids WHERE id IS NOT NULL)
        GROUP BY user_id, username
        HAVING SUM(amount) > users.salary
        LIMIT 20
    """ {
        SelectExpr.Default with
            Columns = [Expr.Ident "username"; Expr.Ident "email"]
            From = Some (Expr.Ident "users")
            Joins = [
                JoinExpr.InnerJoin("meters", Expr.Equals(Expr.Ident "meters.user_id", Expr.Ident "users.user_id"))
                JoinExpr.LeftJoin("utilities", Expr.Equals(Expr.Ident "utilities.id", Expr.Ident "users.user_id"))
            ]
            Where = Some (Expr.In(Expr.Ident "user_id", Expr.SelectQuery {
                SelectExpr.Default with
                    Columns = [Expr.Ident "id"]
                    From = Some (Expr.Ident "user_ids")
                    Where = Some(Expr.Not(Expr.Equals(Expr.Null, Expr.Ident "id")))
            }))

            GroupBy = [Expr.Ident "user_id"; Expr.Ident "username"]

            Having = Some(Expr.GreaterThan(Expr.Function("SUM", [Expr.Ident "amount"]), Expr.Ident "users.salary"))

            Limit = Some (Expr.Integer 20)
    }

    testSelect """
        SELECT username, email
        FROM users
        INNER JOIN meters ON meters.user_id = users.user_id
        LEFT JOIN utilities ON utilities.id = users.user_id
        WHERE user_id IN (SELECT id FROM user_ids WHERE id IS NOT NULL)
        GROUP BY user_id, username
        HAVING SUM(amount) > users.salary
        LIMIT 20
        OFFSET 100
    """ {
        SelectExpr.Default with
            Columns = [Expr.Ident "username"; Expr.Ident "email"]
            From = Some (Expr.Ident "users")
            Joins = [
                JoinExpr.InnerJoin("meters", Expr.Equals(Expr.Ident "meters.user_id", Expr.Ident "users.user_id"))
                JoinExpr.LeftJoin("utilities", Expr.Equals(Expr.Ident "utilities.id", Expr.Ident "users.user_id"))
            ]
            Where = Some (Expr.In(Expr.Ident "user_id", Expr.SelectQuery {
                SelectExpr.Default with
                    Columns = [Expr.Ident "id"]
                    From = Some (Expr.Ident "user_ids")
                    Where = Some(Expr.Not(Expr.Equals(Expr.Null, Expr.Ident "id")))
            }))

            GroupBy = [Expr.Ident "user_id"; Expr.Ident "username"]

            Having = Some(Expr.GreaterThan(Expr.Function("SUM", [Expr.Ident "amount"]), Expr.Ident "users.salary"))

            Limit = Some (Expr.Integer 20)

            Offset = Some (Expr.Integer 100)
    }

    testSelect """
        SELECT *
        FROM (SELECT NOW()) AS time
        LIMIT 1
    """ {
        SelectExpr.Default with
            Columns = [Expr.Star]
            From = Some (Expr.As (Expr.SelectQuery {
                SelectExpr.Default with
                    Columns = [Expr.Function ("NOW", [])]
            }, Expr.Ident "time"))
            Limit = Some (Expr.Integer 1)
    }

    testSelect """
        SELECT *
        FROM
        (SELECT NOW()) time
        LIMIT 1
    """ {
        SelectExpr.Default with
            Columns = [Expr.Star]
            From = Some (Expr.As (Expr.SelectQuery {
                SelectExpr.Default with
                    Columns = [Expr.Function ("NOW", [])]
            }, Expr.Ident "time"))
            Limit = Some (Expr.Integer 1)
    }

    testSelect """
        SELECT "username" FROM "users"
    """ {
        SelectExpr.Default with
            Columns = [Expr.Ident "username"]
            From = Expr.Ident "users" |> Some
        }

    testSelect """
        SELECT "username" AS "name" FROM "users"
    """ {
        SelectExpr.Default with
            Columns = [ Expr.As(Expr.Ident "username", Expr.Ident "name") ]
            From = Expr.Ident "users" |> Some
        }

    testSelect """
        SELECT "$Table"."timestamp" FROM "$Table"
    """ {
        SelectExpr.Default with
            Columns = [ Expr.Ident("$Table.timestamp") ]
            From = Expr.Ident "$Table" |> Some
        }

    testSelect """
        SELECT "$Table".timestamp AS ts FROM "$Table"
    """ {
        SelectExpr.Default with
            Columns = [ Expr.As(Expr.Ident("$Table.timestamp"), Expr.Ident("ts")) ]
            From = Expr.Ident "$Table" |> Some
        }

    testSelect """
        SELECT public."$Table"."timestamp" FROM "$Table"
    """ {
        SelectExpr.Default with
            Columns = [ Expr.Ident("public.$Table.timestamp") ]
            From = Expr.Ident "$Table" |> Some
        }

    testSelect """
        SELECT public."$Table"."timestamp" FROM "$Table" AS tbl
    """ {
        SelectExpr.Default with
            Columns = [ Expr.Ident("public.$Table.timestamp") ]
            From = Expr.As(Expr.Ident "$Table", Expr.Ident "tbl") |> Some
        }

    testSelect """
        SELECT public."$Table"."timestamp" FROM "$Table" tbl LIMIT 100
    """ {
        SelectExpr.Default with
            Columns = [ Expr.Ident("public.$Table.timestamp") ]
            From = Expr.As(Expr.Ident "$Table", Expr.Ident "tbl") |> Some
            Limit = Some (Expr.Integer 100)
        }

    testSelect "SELECT 1 -- This is a comment" {
        SelectExpr.Default with Columns = [Expr.Integer 1]
    }

    testSelect """
        -- This is a comment
        SELECT 1""" {
        SelectExpr.Default with Columns = [Expr.Integer 1]
    }

    testSelect """
        /* Comment inserted first */
        SELECT 1""" {
        SelectExpr.Default with Columns = [Expr.Integer 1]
    }

    testSelect """
        SELECT 1
        /* Comment inserted last */
        """ {
        SelectExpr.Default with Columns = [Expr.Integer 1]
    }

    testSelect """
        SELECT 1;
        /* Comment inserted after semicolon */
        """ {
        SelectExpr.Default with Columns = [Expr.Integer 1]
    }

    testSelect """
        SELECT * /* Comment inserted here */
        FROM -- Ignore
        (SELECT NOW()) time /* Comment inserted here */
        LIMIT 1 -- Ignore
    """ {
        SelectExpr.Default with
            Columns = [Expr.Star]
            From = Some (Expr.As (Expr.SelectQuery {
                SelectExpr.Default with
                    Columns = [Expr.Function ("NOW", [])]
            }, Expr.Ident "time"))
            Limit = Some (Expr.Integer 1)
    }

    testSelect """
        SELECT
        COUNT(*)
        FROM users
        WHERE last_login > TIMESTAMP '2021-01-04 00:00:00'
    """ {
        SelectExpr.Default with
            Columns = [Expr.Function("COUNT", [Expr.Star]) ]
            From = Some (Expr.Ident "users")
            Where = Some (Expr.GreaterThan(Expr.Ident "last_login", Expr.Timestamp("2021-01-04 00:00:00")))
    }

    testSelect """
        SELECT
        COUNT(*)
        FROM users
        WHERE last_login > DATE '2021-01-04 00:00:00'
    """ {
        SelectExpr.Default with
            Columns = [Expr.Function("COUNT", [Expr.Star]) ]
            From = Some (Expr.Ident "users")
            Where = Some (Expr.GreaterThan(Expr.Ident "last_login", Expr.Date("2021-01-04 00:00:00")))
    }

    testSelect """
        select timestamp '2021-01-04 00:00:00'
    """ {
        SelectExpr.Default with
            Columns = [Expr.Timestamp("2021-01-04 00:00:00") ]
    }

    testSelect """
        select date '2021-01-04 00:00:00'
    """ {
        SelectExpr.Default with
            Columns = [Expr.Date("2021-01-04 00:00:00") ]
    }
]

