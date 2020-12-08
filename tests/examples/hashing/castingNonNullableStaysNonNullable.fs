module castingNonNullableStaysNonNullable

open Npgsql.FSharp.Tasks

let userIds connection =
    connection
    |> Sql.connect
    |> Sql.query "SELECT user_id::text AS useridtext FROM users"
    |> Sql.execute (fun read -> read.text "useridtext")

let moreIds connection =
    connection
    |> Sql.connect
    |> Sql.query "SELECT user_id::text as useridtext FROM users"
    |> Sql.execute (fun read -> read.text "useridtext")

let withoutAlias connection =
    connection
    |> Sql.connect
    |> Sql.query "SELECT user_id::text FROM users"
    |> Sql.execute (fun read -> read.text "user_id")
