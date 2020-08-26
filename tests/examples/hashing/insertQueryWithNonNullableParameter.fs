module InsertQueryWithNonNullableParameter

open Npgsql.FSharp

let findUsers connection =
    connection
    |> Sql.query "INSERT INTO users (username, active) VALUES (@username, @active) RETURNING *"
    |> Sql.parameters [ "@username", Sql.dbnull; "@active", Sql.bool true ]
    |> Sql.execute (fun read -> read.text "username")
