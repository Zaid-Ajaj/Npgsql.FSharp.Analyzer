module syntacticAnalysisExecuteScalar

open Npgsql.FSharp
open Npgsql.FSharp.OptionWorkflow

let connectionString = "Dummy connection string"

let findUsernames() =
    connectionString
    |> Sql.connect
    |> Sql.query "SELECT COUNT(*) FROM users WHERE is_active = @is_active"
    |> Sql.parameters [ "is_active", Sql.Value true ]
    |> Sql.executeScalar
    |> Sql.toInt
