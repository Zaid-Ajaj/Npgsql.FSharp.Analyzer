module DetectingDynamicListsInTransactions

open Npgsql.FSharp

let transaction connection values =
    connection
    |> Sql.connect
    |> Sql.executeTransaction [
        "DELETE FROM users WHERE user_id = @user_id", [
            for value in values -> [
                "@user_id", Sql.int value
            ]
        ]
    ]

let transactionWithYield connection values =
    connection
    |> Sql.connect
    |> Sql.executeTransaction [
        "DELETE FROM users WHERE user_id = @user_id", [
            for value in values do yield [
                "@user_id", Sql.int value
            ]
        ]
    ]
