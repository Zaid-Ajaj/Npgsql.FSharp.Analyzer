module Tests

open System
open Expecto
open Npgsql.FSharp.Analyzers
open Npgsql.FSharp
open ThrowawayDb.Postgres

let analyzers = [
    SqlAnalyzer.queryAnalyzer
]

let inline find file = IO.Path.Combine(__SOURCE_DIRECTORY__ , file)
let inline analyze file = AnalyzerBootstrap.runProject file analyzers
let inline context file = AnalyzerBootstrap.context file

let createTestDatabase() =
    // Travis CI uses an empty string for the password of the database
    let databasePassword =
        let runningTravis = Environment.GetEnvironmentVariable "TESTING_IN_TRAVISCI"
        if isNull runningTravis || String.IsNullOrWhiteSpace runningTravis
        then "postgres" // for local tests
        else "" // for Travis CI

    Sql.host "localhost"
    |> Sql.port 5432
    |> Sql.username "postgres"
    |> Sql.password databasePassword
    |> Sql.str
    |> ThrowawayDatabase.Create

[<Tests>]
let tests =
    testList "Postgres" [
        test "Syntactic Analysis: SQL blocks can be detected with their relavant information" {
            match context (find "../examples/hashing/syntacticAnalysis.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                let operationBlocks = SyntacticAnalysis.findSqlBlocks context 
                Expect.equal 3 (List.length operationBlocks) "Found two operation blocks"
        }

        test "SQL schema analysis" {
            use db = createTestDatabase()

            Sql.connect db.ConnectionString
            |> Sql.query "CREATE TABLE users (user_id bigserial primary key, username text not null, active bit not null)"
            |> Sql.executeNonQuery
            |> ignore
             
            let databaseMetadata = InformationSchema.getDbSchemaLookups db.ConnectionString

            let userColumns =
                databaseMetadata.Schemas.["public"].Tables
                |> Seq.tryFind (fun pair -> pair.Key.Name = "users")
                |> Option.map (fun pair -> pair.Value)
                |> Option.map List.ofSeq

            match userColumns with
            | None ->
                failwith "Expected to find columns for users table"
            | Some columns ->
                Expect.equal 3 (List.length columns) "There are three columns"
        }
    ]
