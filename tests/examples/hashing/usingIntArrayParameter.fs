module UsingIntArrayParameter

open Npgsql.FSharp

let findRoles connection =
    connection
    |> Sql.query "SELECT * FROM users WHERE user_id = ANY(@user_ids)"
    |> Sql.parameters [ "user_ids", Sql.text "Hello" ] // should use Sql.intArray, Sql.intArrayOrNone or Sql.dbnull
    |> Sql.execute (fun read -> read.int "user_id") 
