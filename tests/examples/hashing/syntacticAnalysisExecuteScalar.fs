module syntacticAnalysisExecuteScalar

open Npgsql.FSharp
open Npgsql.FSharp.OptionWorkflow

let connectionString = "Dummy connection string"

let findUsernames() =
    connectionString
    |> Sql.connect
    |> Sql.query "SELECT COUNT(*) as Count FROM users WHERE is_active = @is_active"
    |> Sql.parameters [ "is_active", Sql.bit true ]
    |> Sql.executeSingleRow (fun read -> read.int64 "count")
