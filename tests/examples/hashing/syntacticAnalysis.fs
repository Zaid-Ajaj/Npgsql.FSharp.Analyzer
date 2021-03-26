module Postgres

open Npgsql.FSharp
open Npgsql.FSharp.OptionWorkflow

let connectionString = "Dummy connection string"

let findSingleUser(userId: int) = 
    connectionString
    |> Sql.connect
    |> Sql.query "SELECT * FROM users WHERE user_id = @user_id"
    |> Sql.parameters [ "@user_id", Sql.int userId ]
    |> Sql.executeAsync (fun read ->
        option {
            let username = read.text "username"
            let user_id = read.int "user_id" 
            let active = read.bool "active"
            return (username, user_id, active)
        })

let findUsers() = 
    Sql.host "localhost"
    |> Sql.connectFromConfig
    |> Sql.query "SELECT * FROM users"
    |> Sql.parameters [ "@whatever", Sql.bit false; "@another", Sql.int 12 ]
    |> Sql.executeAsync (fun read ->
        option {
            let username = read.text "username"
            let user_id = read.int "user_id" 
            let active = read.bool "active"
            return (username, user_id, active)
        })

let findNumberOfUsers() = 
    Sql.host "localhost"
    |> Sql.connectFromConfig
    |> Sql.query "SELECT COUNT(*) as count FROM users"
    |> Sql.execute (fun read -> read.int64 "count")

let findNumberOfUsersAfterCallingPrintExpressions () =
    printfn "Non blocking expression"
    printf "Non blocking expression"
    Sql.host "localhost"
    |> Sql.connectFromConfig
    |> Sql.query "SELECT COUNT(*) as count FROM users"
    |> Sql.execute (fun read -> read.int64 "count")

let executeFunction() =
    Sql.host "localhost"
    |> Sql.connectFromConfig
    |> Sql.func "getNumberOfUsers"
    |> Sql.execute (fun read -> read.text "username")

type Whatever() =
    member this.Hello =
        let whateverSql() = 
            Sql.host "localhost"
            |> Sql.connectFromConfig
            |> Sql.query "SELECT COUNT(*) as count FROM users"
            |> Sql.execute (fun read -> read.int64 "count")

        ()

let doCoolStuff() = async {
    return Sql.host "localhost"
           |> Sql.connectFromConfig
           |> Sql.query "SELECT COUNT(*) as count FROM users"
           |> Sql.execute (fun read -> read.int64 "count")
}

let doCoolStuffAsync() = async {
    return! Sql.host "localhost"
           |> Sql.connectFromConfig
           |> Sql.query "SELECT COUNT(*) as count FROM users"
           |> Sql.executeAsync (fun read -> read.int64 "count")
}

let doCoolStuffAsyncWithLet() = async {
    let! x =  Sql.host "localhost"
           |> Sql.connectFromConfig
           |> Sql.query "SELECT COUNT(*) as count FROM users"
           |> Sql.executeAsync (fun read -> read.int64 "count")

    return x; 
}

module InsidePostgres =

    let nextedDeclaretion() = 
        Sql.host "localhost"
        |> Sql.connectFromConfig
        |> Sql.query "SELECT COUNT(*) as count FROM users"
        |> Sql.execute (fun read -> read.int64 "count")

    let nextedDeclaretionWithExplicitReturnType() : Result<int64 list, exn>= 
        Sql.host "localhost"
        |> Sql.connectFromConfig
        |> Sql.query "SELECT COUNT(*) as count FROM users"
        |> Sql.execute (fun read -> read.int64 "count")

    type NestedWhatever() =
        member this.Hello =
            let whateverSql() = 
                Sql.host "localhost"
                |> Sql.connectFromConfig
                |> Sql.query "SELECT COUNT(*) as count FROM users"
                |> Sql.execute (fun read -> read.int64 "count")

            ()
