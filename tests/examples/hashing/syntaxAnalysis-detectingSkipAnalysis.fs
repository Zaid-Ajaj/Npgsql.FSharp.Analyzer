module SyntaxAnalysisDetectingSkipAnalysis

open Npgsql.FSharp

let badQuery connection =
    connection
    |> Sql.query "SELECT * FROM non_existing_table"
    |> Sql.skipAnalysis
    |> Sql.execute (fun read -> read.int64 "user_id") 
