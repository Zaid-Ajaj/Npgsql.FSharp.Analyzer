module ErrorsInTransactions

open Npgsql.FSharp

let executeMultipleQueries =
    "connectionString"
    |> Sql.connect
    |> Sql.executeTransaction [
        "UPDATE users SET user_id = @user_id", [ ]
        "DELTE FROM meters", [ ]
    ]
