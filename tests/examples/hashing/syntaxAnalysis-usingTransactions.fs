module SyntaxAnalysisUsingTransactions

open Npgsql.FSharp

let executeMultipleQueries =
    "connectionString"
    |> Sql.connect
    |> Sql.executeTransaction [
        "UPDATE users SET user_id = @user_id", [
            [ "@user_id", Sql.int 42 ]
            [ "@user_id", Sql.int 43 ]
            [ "@user_id", Sql.int 44 ]
            [ "@user_id", (Sql.int 44) ]
        ]

        "DELETE FROM users", [ ]
    ]

let executeMultipleQueriesAsync =
    "connectionString"
    |> Sql.connect
    |> Sql.executeTransactionAsync [
        "UPDATE users SET user_id = @user_id", [ ]
        "DELETE FROM users", [ ]
    ]
