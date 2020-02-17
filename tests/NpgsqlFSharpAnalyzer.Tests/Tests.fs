module Tests

open System
open Expecto
open Npgsql.FSharp.Analyzers
open Npgsql.FSharp
open ThrowawayDb.Postgres
open FSharp.Analyzers.SDK

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
                Expect.equal 4 (List.length operationBlocks) "Found four operation blocks"
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

        test "SQL query analysis" {
            use db = createTestDatabase()

            Sql.connect db.ConnectionString
            |> Sql.query "CREATE TABLE users (user_id bigserial primary key, username text not null, active bit not null)"
            |> Sql.executeNonQuery
            |> ignore

            let databaseMetadata = InformationSchema.getDbSchemaLookups db.ConnectionString

            let query = "SELECT * FROM users"
            let parameters, outputColumns, enums = InformationSchema.extractParametersAndOutputColumns(db.ConnectionString, query, false, databaseMetadata)
            Expect.isEmpty parameters "Query contains no parameters"
            Expect.equal 3 (List.length outputColumns) "There are 3 columns in users table"
        }

        test "SQL scalar query analysis" {
            use db = createTestDatabase()
            
            Sql.connect db.ConnectionString
            |> Sql.query "CREATE TABLE users (user_id bigserial primary key, username text not null, active bit not null)"
            |> Sql.executeNonQuery
            |> ignore

            let databaseMetadata = InformationSchema.getDbSchemaLookups db.ConnectionString
            let query = "SELECT COUNT(*) FROM users"
            let resultSetMetadata = SqlAnalysis.extractParametersAndOutputColumns(db.ConnectionString, query, databaseMetadata)
            match resultSetMetadata with
            | Result.Error errorMsg ->
                failwithf "Could not analyse result set metadata %s" errorMsg
            | Result.Ok (parameters, outputColumns) ->
                Expect.isEmpty parameters "Query shouldn't contain any parameters"
                Expect.equal 1 outputColumns.Length "There is one column returned"
        }

        test "SQL function analysis" {
            use db = createTestDatabase()

            let createFuncQuery = """
            CREATE FUNCTION Increment(val integer) RETURNS integer AS $$
            BEGIN
            RETURN val + 1;
            END; $$
            LANGUAGE PLPGSQL;
            """
            
            Sql.connect db.ConnectionString
            |> Sql.query createFuncQuery
            |> Sql.executeNonQuery
            |> ignore

            let databaseMetadata = InformationSchema.getDbSchemaLookups db.ConnectionString
            let query = "SELECT Increment(@Input)"
            let resultSetMetadata = SqlAnalysis.extractParametersAndOutputColumns(db.ConnectionString, query, databaseMetadata)
            match resultSetMetadata with
            | Result.Error errorMsg ->
                failwithf "Could not analyse result set metadata %s" errorMsg
            | Result.Ok (parameters, outputColumns) ->
                Expect.equal 1 parameters.Length "Query has one parameter"
                Expect.equal "integer" parameters.[0].DataType.Name "The parameter is int4"
                Expect.equal 1 outputColumns.Length "There is one column returned"
                Expect.equal "integer" outputColumns.[0].DataType.Name "The output type is int4"
        }

        test "SQL query semantic analysis: missing column" {
            use db = createTestDatabase()

            Sql.connect db.ConnectionString
            |> Sql.query "CREATE TABLE users (user_id bigserial primary key, username text not null, active bit not null)"
            |> Sql.executeNonQuery
            |> ignore

            match context (find "../examples/hashing/semanticAnalysis-missingColumn.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                let block = List.exactlyOne (SyntacticAnalysis.findSqlBlocks context)
                let messages = SqlAnalysis.analyzeBlock block db.ConnectionString
                match messages with
                | [ message ] ->
                    Expect.equal Severity.Warning message.Severity "The message is an warning"
                    Expect.stringContains message.Message "non_existent" "Message should contain the missing column name"
                | _ ->
                    failwith "Expected only one error message"
        }

        test "SQL query semantic analysis: missing parameter" {
            use db = createTestDatabase()

            Sql.connect db.ConnectionString
            |> Sql.query "CREATE TABLE users (user_id bigserial primary key, username text not null, active bit not null)"
            |> Sql.executeNonQuery
            |> ignore

            match context (find "../examples/hashing/semanticAnalysis-missingParameter.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                let block = List.exactlyOne (SyntacticAnalysis.findSqlBlocks context)
                let messages = SqlAnalysis.analyzeBlock block db.ConnectionString
                match messages with
                | [ message ] ->
                    Expect.equal Severity.Warning message.Severity "The message is a warning"
                    Expect.stringContains message.Message "Missing parameter 'active'"  "Error should say which parameter is not provided"
                | _ ->
                    failwith "Expected only one error message"
        }
    ]
