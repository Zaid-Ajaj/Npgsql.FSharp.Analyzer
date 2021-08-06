module readAttemptIntegerTypeMismatch

open Npgsql.FSharp

let connectionString = "Dummy connection string"

let findUsers() =
    connectionString
    |> Sql.connect
    |> Sql.query "SELECT * FROM users"
    |> Sql.executeAsync (fun read ->
         let user_id = read.bool "user_id"
         let username = read.text "username"
         let active = read.bool "active"
         (user_id, username, active))
