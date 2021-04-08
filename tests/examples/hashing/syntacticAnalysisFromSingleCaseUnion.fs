module SyntacticAnalysisFromSingleCaseUnion

open Npgsql.FSharp

type SqlAction = SqlAction of (string -> int list)

let getData =
    SqlAction (fun (connectionString: string) -> 
        connectionString
        |> Sql.connect
        |> Sql.query "SELECT * FROM users WHERE user_id = @user_id"
        |> Sql.parameters [ "@user_id", Sql.int 42 ]
        |> Sql.execute (fun read -> read.int "user_id"))
