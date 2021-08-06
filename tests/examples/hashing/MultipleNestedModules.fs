namespace App

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

    [<Literal>]
    let getArticlesQuery = "SELECT x FROM test.articles"

    let getArticles () =
        getConnectionString()
        |> Sql.connect
        |> Sql.query getArticlesQuery
        |> Sql.execute (fun read ->
            {
                Id = read.int "id"
                Name = read.text "art_name"
                Stock = read.int "stock"
            })
