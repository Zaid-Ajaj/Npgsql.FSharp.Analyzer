module SelectWithInnerJoins

open Npgsql.FSharp

let [<Literal>] usersWithRoles = """
    SELECT * FROM users
    JOIN user_roles ON users.user_id = user_roles.user_id
    WHERE users.user_id = @user_id AND role_description = @role
"""

let usersAndTheirRoles connectionString =
    connectionString
    |> Sql.connect
    |> Sql.query usersWithRoles
    |> Sql.parameters [ "@user_id", Sql.intOrNone None; "@role", Sql.text "User" ]
    |> Sql.execute (fun read -> read.int "user_id")
