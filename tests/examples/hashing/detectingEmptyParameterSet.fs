module DetectingEmptyParameterSet

open Npgsql.FSharp

let transaction connection =
    connection
    |> Sql.connect
    |> Sql.executeTransaction [
        "SELECT * FROM users WHERE user_id = @user_id", [
            [ ]
        ]
    ]
