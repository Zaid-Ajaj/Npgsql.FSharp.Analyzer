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
    Sql.host "localhost"
    |> Sql.port 5432
    |> Sql.username "postgres"
    |> Sql.password "postgres"
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

        test "Syntactic Analysis: reading queries with [<Literal>] query" {
            match context (find "../examples/hashing/syntacticAnalysis-literalStrings.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                match SyntacticAnalysis.findSqlBlocks context with
                | [ operation ] ->
                    match SqlAnalysis.findQuery operation with
                    | Some(query, range) -> Expect.equal query "SELECT * FROM users" "Literal string should be read correctly"
                    | None -> failwith "Should have found the correct query"
                | _ ->
                    failwith "Should not happen"
        }

        test "Syntactic Analysis: simple queries can be read" {
            match context (find "../examples/hashing/syntacticAnalysisSimpleQuery.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                match SyntacticAnalysis.findSqlBlocks context with
                | [ operation ] ->
                    match SqlAnalysis.findQuery operation with
                    | Some(query, range) -> Expect.equal query "SELECT COUNT(*) FROM users" "Literal string should be read correctly"
                    | None -> failwith "Should have found the correct query"
                | _ ->
                    failwith "Should not happen"
        }

        test "Syntactic Analysis: combination with Sql.executeScalar can be detected" {
            match context (find "../examples/hashing/syntacticAnalysisExecuteScalar.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                match SyntacticAnalysis.findSqlBlocks context with
                | [ operation ] ->
                    match SqlAnalysis.findQuery operation with
                    | Some(query, range) ->
                        Expect.equal query "SELECT COUNT(*) FROM users WHERE is_active = @is_active" "Literal string should be read correctly"
                        match SqlAnalysis.findParameters operation with
                        | Some ([ parameter ], range) ->
                            Expect.equal "is_active" parameter.parameter "Parameter is correct"
                        | otherwise ->
                            failwith "There should have been a parameter"
                    | None ->
                        failwith "Should have found the correct query"
                | _ ->
                    failwith "Should not happen"
        }

        test "Syntactic Analysis: reading queries with extra processing after Sql.executeReader" {
            match context (find "../examples/hashing/syntacticAnalysisProcessedList.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                match SyntacticAnalysis.findSqlBlocks context with
                | [ operation ] ->
                    match SqlAnalysis.findQuery operation with
                    | Some(query, range) ->
                        Expect.equal query "SELECT * FROM users" "Query should be read correctly"
                        match SqlAnalysis.findColumnReadAttempts operation with
                        | Some [ attempt ] ->
                            Expect.equal attempt.funcName "Sql.readString" "Function name is correct"
                            Expect.equal attempt.columnName "username" "Column name is read correctly"
                        | otherwise ->
                            failwith "Should have found one column read attempt"
                    | None ->
                        failwith "Should have found the correct query"
                | otherwise ->
                    failwith "Should not happen"
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

        test "SQL query semantic analysis: type mismatch" {
            use db = createTestDatabase()

            Sql.connect db.ConnectionString
            |> Sql.query "CREATE TABLE users (user_id bigserial primary key, username text not null, active bit not null)"
            |> Sql.executeNonQuery
            |> ignore

            match context (find "../examples/hashing/semanticAnalysis-typeMismatch.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                let block = List.exactlyOne (SyntacticAnalysis.findSqlBlocks context)
                let messages = SqlAnalysis.analyzeBlock block db.ConnectionString
                match messages with
                | [ message ] ->
                    Expect.stringContains message.Message "Please use Sql.readBool instead" "Message contains suggestion to use Sql.readBool"
                | _ ->
                    failwith "Expected only one error message"
        }

        test "SQL query semantic analysis: redundant parameters" {
            use db = createTestDatabase()

            Sql.connect db.ConnectionString
            |> Sql.query "CREATE TABLE users (user_id bigserial primary key, username text not null, active bit not null)"
            |> Sql.executeNonQuery
            |> ignore

            match context (find "../examples/hashing/semanticAnalysis-redundantParameters.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                let block = List.exactlyOne (SyntacticAnalysis.findSqlBlocks context)
                let messages = SqlAnalysis.analyzeBlock block db.ConnectionString
                match messages with
                | [ message ] ->
                    Expect.stringContains message.Message "Provided parameters are redundant" "Message contains suggestion to remove Sql.parameters"
                | _ ->
                    failwith "Expected only one error message"
        }
    ]
