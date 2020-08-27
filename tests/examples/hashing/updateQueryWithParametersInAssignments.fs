module UpdateQueryWithParametersInAssignments

open Npgsql.FSharp
open System

let [<Literal>] updateUsernameQuery = """
    UPDATE users
    SET username = @username, last_login = @last_login
    WHERE user_id = @user_id
    RETURNING *
"""

let usersAndTheirRoles connectionString =
    connectionString
    |> Sql.connect
    |> Sql.query updateUsernameQuery
    |> Sql.parameters [
        "@user_id", Sql.intOrNone (Some 10)
        "@username", Sql.textOrNone (Some "John")
        "@last_login", Sql.timestamptz DateTime.Now
    ]
    |> Sql.execute (fun read -> read.int "user_id")
