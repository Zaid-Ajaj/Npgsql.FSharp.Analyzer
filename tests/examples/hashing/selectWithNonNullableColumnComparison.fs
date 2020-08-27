module SelectWithNonNullableColumnComparison

open Npgsql.FSharp

let findUsernames connectionString =
    connectionString
    |> Sql.connect
    |> Sql.query "SELECT * FROM users WHERE user_id = @user_id AND username = @username"
    |> Sql.parameters [ "@user_id", Sql.intOrNone None; "@username", Sql.text "User" ]
    |> Sql.execute (fun read -> read.int "user_id")
