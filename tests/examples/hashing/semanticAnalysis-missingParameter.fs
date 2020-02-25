module semanticAnalysis_missingParameter

open Npgsql.FSharp

let connectionString = "Dummy connection string"

let findUsers() =
    connectionString
    |> Sql.connect
    |> Sql.query "SELECT * FROM users WHERE user_id = @user_id AND active = @active"
    |> Sql.parameters [ "user_id", Sql.int64 42L ]
    |> Sql.executeAsync (fun read ->
        let userId =  read.int "user_id"
        let username = read.text "username"
        let active = read.bool "active"
        (userId, username, active))
