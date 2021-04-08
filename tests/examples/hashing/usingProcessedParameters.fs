module UsingProcessedParameters

open Npgsql.FSharp

let add x y = x + y

let findRoles connection =
    connection
    |> Sql.query "SELECT * FROM users WHERE username = @username"
    |> Sql.parameters [
        "@username", "Hello" |> Sql.text
        "@user_id", 5 |> add 10 |> Sql.int
    ]
    |> Sql.execute (fun read -> read.int "user_id") 
