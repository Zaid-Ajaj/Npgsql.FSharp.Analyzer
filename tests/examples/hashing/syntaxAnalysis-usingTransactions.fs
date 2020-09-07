module SyntaxAnalysisUsingTransactions

open Npgsql.FSharp

let executeMultipleQueries =
    "connectionString"
    |> Sql.connect
    |> Sql.executeTransaction [
        "UPDATE users SET user_id = @user_id", [ ]
        "DELETE FROM users", [ ]
    ]

let executeMultipleQueriesAsync =
    "connectionString"
    |> Sql.connect
    |> Sql.executeTransactionAsync [
        "UPDATE users SET user_id = @user_id", [ ]
        "DELETE FROM users", [ ]
    ]
