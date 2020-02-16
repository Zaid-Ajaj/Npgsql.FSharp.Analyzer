module semanticAnalysis

open Npgsql.FSharp
open Npgsql.FSharp.OptionWorkflow

let connectionString = "Dummy connection string"

let findUsers() = 
    connectionString
    |> Sql.connect
    |> Sql.query "SELECT non_existent FROM users"
    |> Sql.executeReaderAsync (fun reader ->
        let row = Sql.readRow reader
        option {
            let! user_id = Sql.readLong "user_id" row
            let! username = Sql.readString "username" row
            let! active = Sql.readBool "active" row
            return (user_id, username, active)
        })
