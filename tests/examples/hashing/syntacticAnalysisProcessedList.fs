module semanticAnalysisProcessedList

open Npgsql.FSharp
open Npgsql.FSharp.OptionWorkflow

let connectionString = "Dummy connection string"

let findUsernames() =
    connectionString
    |> Sql.connect
    |> Sql.query "SELECT * FROM users"
    |> Sql.execute (fun read -> read.text "username")
    |> function
        | Error error -> None 
        | Ok users -> Some users
    |> Option.map List.isEmpty
