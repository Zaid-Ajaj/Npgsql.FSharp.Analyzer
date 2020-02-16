module Postgres

open Npgsql.FSharp
open Npgsql.FSharp.OptionWorkflow

let connectionString = "Dummy connection string"

let findSingleUser(userId: int) = 
    connectionString
    |> Sql.connect
    |> Sql.query "SELECT * FROM users WHERE user_id = @user_id"
    |> Sql.parameters [ "@user_id", Sql.Value userId ]
    |> Sql.executeReaderAsync (fun reader ->
        let row = Sql.readRow reader
        option {
            let! username = Sql.readString "username" row
            return username
        })

let findUsers() = 
    Sql.host "localhost"
    |> Sql.connectFromConfig
    |> Sql.query "SELECT * FROM users"
    |> Sql.parameters [ "@whatever", Sql.Value false; "@another", Sql.Value false ]
    |> Sql.executeReaderAsync (fun reader ->
        let row = Sql.readRow reader
        option {
            let! username = Sql.readString "username" row
            let! user_id = Sql.readInt "user_id" row
            return (username, user_id)
        })

let findNumberOfUsers() = 
    Sql.host "localhost"
    |> Sql.connectFromConfig
    |> Sql.query "SELECT COUNT(*) FROM users"
    |> Sql.executeNonQuery
