#r "nuget:Npgsql.FSharp"

module Domain =
    type Article = { 
            Id : int ; 
            Name: string; 
            Stock: int 
        } 

module Data =
    open Domain
    open Npgsql.FSharp

    let getConnectionString () =
        System.Environment.GetEnvironmentVariable "DATABASE_CONNECTION_STRING"

    let getArticles () =
        getConnectionString()
        |> Sql.connect
        |> Sql.query "SELECT x FROM test.articles"
        |> Sql.execute (fun read ->
            {
                Id = read.int "id"
                Name = read.text "art_name"
                Stock = read.int "stock"
            })
