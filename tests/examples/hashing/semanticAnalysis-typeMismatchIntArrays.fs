module semanticAnalysis_typeMismatchIntArrays

open Npgsql.FSharp
open Npgsql.FSharp.OptionWorkflow

let connectionString = "Dummy connection string"

let findInts() =
    connectionString
    |> Sql.connect
    |> Sql.query "SELECT * FROM ints"
    |> Sql.executeAsync (fun read ->
         let id = read.uuid "id"
         // read.int should be read.intArray
         let ints = read.int "ints"
         id, ints
    )
