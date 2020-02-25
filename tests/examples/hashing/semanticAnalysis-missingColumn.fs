module semanticAnalysis

open Npgsql.FSharp
open Npgsql.FSharp.OptionWorkflow

let connectionString = "Dummy connection string"

let findUsers() =
    connectionString
    |> Sql.connect
    |> Sql.query "SELECT non_existent FROM users"
    |> Sql.executeAsync (fun read ->
        {|
            userId = read.int "user_id"
            username = read.text "username"
            active = read.bool "active"
        |})
