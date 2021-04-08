module parameterTypeMismatch

open Npgsql.FSharp

let connectionString = "Dummy connection string"
 
let findUsers() =
    connectionString
    |> Sql.connect
    |> Sql.query "SELECT * FROM users WHERE user_id = @user_id"
    |> Sql.parameters [ "user_id", Sql.bit false ]
    |> Sql.execute (fun read -> read.text "username")
