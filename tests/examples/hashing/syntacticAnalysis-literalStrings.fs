module semanticAnalysis_literalStrings

open Npgsql.FSharp

let connectionString = "Dummy connection string"

let [<Literal>] query = "SELECT * FROM users"

let [<Literal>] maskedQuery = query

let findUsers() =
    connectionString
    |> Sql.connect 
    |> Sql.query maskedQuery
    |> Sql.executeReaderAsync (Sql.readRow >> Sql.readString "username")
