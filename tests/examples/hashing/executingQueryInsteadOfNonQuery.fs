module ExecutingQueryInsteadOfNonQuery

open Npgsql.FSharp

let findRoles connection =
    connection
    |> Sql.query "INSERT INTO users (username, roles) VALUES (@username, @roles)"
    |> Sql.parameters [ "@username", Sql.string "Hello";  "@roles", Sql.stringArray [| |] ]
    |> Sql.execute (fun read -> read.stringArray "roles")
