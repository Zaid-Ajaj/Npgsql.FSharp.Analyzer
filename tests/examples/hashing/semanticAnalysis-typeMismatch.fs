module semanticAnalysis_typeMismatch

open Npgsql.FSharp

let connectionString = "Dummy connection string"

let findUsers() =
    connectionString
    |> Sql.connect
    |> Sql.query "SELECT * FROM users"
    |> Sql.executeAsync (fun read ->
         let user_id = read.int "user_id"
         let username = read.text "username"
         // Sql.readInt should be Sql.readBool
         let active = read.int "active"
         (user_id, username, active))
