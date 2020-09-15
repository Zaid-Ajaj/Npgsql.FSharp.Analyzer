module UsingLiteralQueriesWithTransactons

open Npgsql.FSharp

let [<Literal>] DeleteUsersFirst = "DELETE FROM users WHERE user_id = @user_id"

let [<Literal>] DeleteUserSecond = "DELETE FROM users WHERE user_id = @user_id"

let transaction connection values =
    connection
    |> Sql.connect
    |> Sql.executeTransaction [
        DeleteUsersFirst, [
            for value in values -> [
                "@user_id", Sql.int value
            ]
        ]
    ]

let transactionWithYield connection values =
    connection
    |> Sql.connect
    |> Sql.executeTransaction [
        DeleteUserSecond, [
            for value in values do yield [
                "@user_id", Sql.int value
            ]
        ]
    ]
