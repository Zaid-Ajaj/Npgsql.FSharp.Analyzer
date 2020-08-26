module LimitExpressionTest

open Npgsql.FSharp

let findUsers connection =
    connection
    |> Sql.query "SELECT * FROM users LIMIT @numberOfUsers"
    |> Sql.parameters [ "@numberOfUsers", Sql.int64OrNone (Some 42L) ]
    |> Sql.execute (fun read -> read.text "username")
