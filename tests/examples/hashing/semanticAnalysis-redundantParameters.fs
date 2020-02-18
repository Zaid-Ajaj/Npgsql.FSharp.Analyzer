module semanticAnalysis_redundantParameters

open Npgsql.FSharp
open Npgsql.FSharp.OptionWorkflow

let connectionString = "Dummy connection string"

let findUsers() =
    connectionString
    |> Sql.connect
    |> Sql.query "SELECT * FROM users"
    |> Sql.parameters [ "user_id", Sql.Value 42 ]
    |> Sql.executeReaderAsync (Sql.readRow >> Sql.readString "username")
