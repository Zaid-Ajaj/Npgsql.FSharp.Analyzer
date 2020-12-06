module SyntacticAnalysisFromLambdaBody

open Npgsql.FSharp

let getData =
    fun (connectionSring: string) -> 
        connectionSring
        |> Sql.connect
        |> Sql.query "SELECT * FROM users WHERE user_id = @user_id"
        |> Sql.parameters [ "@user_id", Sql.int 42 ]
        |> Sql.execute (fun read -> read.int "user_id")
