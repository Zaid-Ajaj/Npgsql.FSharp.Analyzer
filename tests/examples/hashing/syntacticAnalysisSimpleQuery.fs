module syntacticAnalysisSimpleQuery

open Npgsql.FSharp

let connectionString = "Dummy connection string"

let findUsernames() =
    connectionString
    |> Sql.connect
    |> Sql.query "SELECT COUNT(*) FROM users"
