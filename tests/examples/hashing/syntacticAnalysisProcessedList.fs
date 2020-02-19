module semanticAnalysisProcessedList

open Npgsql.FSharp
open Npgsql.FSharp.OptionWorkflow

let connectionString = "Dummy connection string"

let findUsernames() =
    connectionString
    |> Sql.connect
    |> Sql.query "SELECT * FROM users"
    |> Sql.executeReader (Sql.readRow >> Sql.readString "username")
    |> String.concat ", "
    |> printfn "Usernames are %s"
