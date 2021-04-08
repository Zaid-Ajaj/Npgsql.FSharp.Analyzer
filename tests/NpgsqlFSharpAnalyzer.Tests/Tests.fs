module Tests

open System
open Expecto
open Npgsql.FSharp.Analyzers
open Npgsql.FSharp.Analyzers.Core
open Npgsql.FSharp
open ThrowawayDb.Postgres

let analyzers = [
    SqlAnalyzer.queryAnalyzer
]

let inline find file = IO.Path.Combine(__SOURCE_DIRECTORY__ , file)
let project = IO.Path.Combine(__SOURCE_DIRECTORY__, "../examples/hashing/examples.fsproj")

let raiseWhenFailed = function
    | Result.Ok _ -> ()
    | Result.Error error -> raise error

let inline context file =
    AnalyzerBootstrap.context file
    |> Option.map SqlAnalyzer.sqlAnalyzerContext

let createTestDatabase() =
    Sql.host "localhost"
    |> Sql.port 5432
    |> Sql.username "postgres"
    |> Sql.password "postgres"
    |> Sql.formatConnectionString
    |> ThrowawayDatabase.Create

[<Tests>]
let tests =
    testList "Postgres" [
        test "Syntactic Analysis: SQL blocks can be detected with their relavant information" {
            match context (find "../examples/hashing/syntacticAnalysis.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                let operationBlocks = SyntacticAnalysis.findSqlOperations context
                Expect.equal (List.length operationBlocks) 15 "Found 15 operation blocks"
        }

        test "Syntactic analysis: no SQL blocks should be found using sprintf" {
            match context (find "../examples/hashing/SprintfBlock.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                let operations = SyntacticAnalysis.findSqlOperations context
                Expect.isEmpty operations "There should be no syntactic blocks"
        }

        test "Syntactic Analysis: finding processed parameters" {
            match context (find "../examples/hashing/usingProcessedParameters.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                let operationBlocks = SyntacticAnalysis.findSqlOperations context
                Expect.equal (List.length operationBlocks) 1 "Found 1 operation"
                let parameters = 
                    [
                        for operation in operationBlocks do
                        for block in operation.blocks do
                            match block with
                            | SqlAnalyzerBlock.Parameters (parameters, _) ->
                                yield! parameters
                            | _ ->
                                ()
                    ]

                Expect.equal 2 parameters.Length "There are 2 parameters"
        }

        test "Syntactic analysis: SQL block found from top-level expression in module" {
            match context (find "../examples/hashing/topLevelExpressionIsDetected.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                let operations = SyntacticAnalysis.findSqlOperations context
                Expect.equal 1 operations.Length "There should be one syntactic block found"
        }

        test "Syntactic analysis: SQL block found from lambda body" {
            match context (find "../examples/hashing/syntacticAnalysisFromLambdaBody.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                let operations = SyntacticAnalysis.findSqlOperations context
                Expect.equal 1 operations.Length "There should be one syntactic block found"
        }

        test "Syntactic analysis: SQL block found from lambda body wrapped in single case union" {
            match context (find "../examples/hashing/syntacticAnalysisFromSingleCaseUnion.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                let operations = SyntacticAnalysis.findSqlOperations context
                Expect.equal 1 operations.Length "There should be one syntactic block found"
        }

        test "Syntactic Analysis: reading queries with [<Literal>] query" {
            match context (find "../examples/hashing/syntacticAnalysis-literalStrings.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                match SyntacticAnalysis.findSqlOperations context with
                | [ operation ] ->
                    match SqlAnalysis.findQuery operation with
                    | Some(query, range) -> Expect.equal query "SELECT * FROM users" "Literal string should be read correctly"
                    | None -> failwith "Should have found the correct query"
                | _ ->
                    failwith "Should not happen"
        }

        test "Syntactic Analysis: reading queries from transactions" {
            match context (find "../examples/hashing/syntaxAnalysis-usingTransactions.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                let operations = SyntacticAnalysis.findSqlOperations context
                Expect.equal operations.Length 2 "There should be two operations"
        }

        test "Syntactic Analysis: simple queries can be read" {
            match context (find "../examples/hashing/syntacticAnalysisSimpleQuery.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                match SyntacticAnalysis.findSqlOperations context with
                | [ operation ] ->
                    match SqlAnalysis.findQuery operation with
                    | Some(query, range) -> Expect.equal query "SELECT COUNT(*) FROM users" "Literal string should be read correctly"
                    | None -> failwith "Should have found the correct query"
                | _ ->
                    failwith "Should not happen"
        }

        test "Syntactic Analysis: combination with Sql functions can be detected" {
            match context (find "../examples/hashing/syntacticAnalysisExecuteScalar.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                match SyntacticAnalysis.findSqlOperations context with
                | [ operation ] ->
                    match SqlAnalysis.findQuery operation with
                    | Some(query, range) ->
                        Expect.equal query "SELECT COUNT(*) as Count FROM users WHERE is_active = @is_active" "Literal string should be read correctly"
                        match SqlAnalysis.findParameters operation with
                        | Some ([ parameter ], range) ->
                            Expect.equal "is_active" parameter.name "Parameter is correct"
                        | otherwise ->
                            failwith "There should have been a parameter"
                    | None ->
                        failwith "Should have found the correct query"
                | _ ->
                    failwith "Should not happen"
        }

        test "Syntactic Analysis: skip analysis can be detected" {
            match context (find "../examples/hashing/syntaxAnalysis-detectingSkipAnalysis.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                match SyntacticAnalysis.findSqlOperations context with
                | [ operation ] ->
                    operation.blocks
                    |> List.exists (fun block -> block = SqlAnalyzerBlock.SkipAnalysis)
                    |> fun found -> Expect.isTrue found "Skip analysis block found"
                | _ ->
                    failwith "Should not happen"
        }

        test "Syntactic Analysis: empty parameter sets can be analyzed" {
            match context (find "../examples/hashing/detectingEmptyParameterSet.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                match SyntacticAnalysis.findSqlOperations context with
                | [ operation ] ->
                    let transactionQueries = 
                        operation.blocks
                        |> List.tryPick (fun block ->
                            match block with
                            | SqlAnalyzerBlock.Transaction queries -> Some queries
                            | _ -> None)

                    match transactionQueries with
                    | None -> failwith "Expected transaction to be found"
                    | Some [ ] -> failwith "Expected transaction to have one query"
                    | Some (query :: _) ->
                        Expect.equal 1 query.parameterSets.Length "There is one parameter set"
                        Expect.isEmpty query.parameterSets.[0].parameters "There are no parameters provided"

                | _ ->
                    failwith "Should not happen"
        }

        test "Syntactic Analysis: detecting dynamic arrays" {
            match context (find "../examples/hashing/detectingDynamicListsInTransactions.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                match SyntacticAnalysis.findSqlOperations context with
                | [ operation; secondOperation ] ->
                    let transactionQueries = 
                        operation.blocks
                        |> List.tryPick (fun block ->
                            match block with
                            | SqlAnalyzerBlock.Transaction queries -> Some queries
                            | _ -> None)

                    match transactionQueries with
                    | None -> failwith "Expected transaction to be found"
                    | Some [ ] -> failwith "Expected transaction to have one query"
                    | Some (query :: _) ->
                        Expect.equal 1 query.parameterSets.Length "There is one parameter set"
                        Expect.equal 1 query.parameterSets.[0].parameters.Length "There are no parameters provided"

                    let secondTransactionQueries = 
                        secondOperation.blocks
                        |> List.tryPick (fun block ->
                            match block with
                            | SqlAnalyzerBlock.Transaction queries -> Some queries
                            | _ -> None)

                    match secondTransactionQueries with
                    | None -> failwith "Expected transaction to be found"
                    | Some [ ] -> failwith "Expected transaction to have one query"
                    | Some (query :: _) ->
                        Expect.equal 1 query.parameterSets.Length "There is one parameter set"
                        Expect.equal 1 query.parameterSets.[0].parameters.Length "There are no parameters provided"
                | _ ->
                    failwith "Should not happen"
        }

        test "Syntactic Analysis: detecting dynamic arrays and using query literals" {
            use db = createTestDatabase()

            Sql.connect db.ConnectionString
            |> Sql.query "CREATE TABLE users (user_id bigserial primary key, username text not null)"
            |> Sql.executeNonQuery
            |> raiseWhenFailed

            match context (find "../examples/hashing/usingLiteralQueriesWithTransactions.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                match SyntacticAnalysis.findSqlOperations context with
                | [ operation; secondOperation ] ->

                    let transactionQueries = 
                        operation.blocks
                        |> List.tryPick (fun block ->
                            match block with
                            | SqlAnalyzerBlock.Transaction queries -> Some queries
                            | _ -> None)

                    match transactionQueries with
                    | None -> failwith "Expected transaction to be found"
                    | Some [ ] -> failwith "Expected transaction to have one query"
                    | Some (query :: _) ->
                        Expect.equal 1 query.parameterSets.Length "There is one parameter set"
                        Expect.equal 1 query.parameterSets.[0].parameters.Length "There are no parameters provided"

                    let secondTransactionQueries = 
                        secondOperation.blocks
                        |> List.tryPick (fun block ->
                            match block with
                            | SqlAnalyzerBlock.Transaction queries -> Some queries
                            | _ -> None)

                    match secondTransactionQueries with
                    | None -> failwith "Expected transaction to be found"
                    | Some [ ] -> failwith "Expected transaction to have one query"
                    | Some (query :: _) ->
                        Expect.equal 1 query.parameterSets.Length "There is one parameter set"
                        Expect.equal 1 query.parameterSets.[0].parameters.Length "There are no parameters provided"

                    match SqlAnalysis.databaseSchema db.ConnectionString with
                    | Result.Error connectionError ->
                        failwith connectionError
                    | Result.Ok schema ->
                        let firstMessages = SqlAnalysis.analyzeOperation operation db.ConnectionString schema
                        let secondMessages = SqlAnalysis.analyzeOperation secondOperation db.ConnectionString schema
                        Expect.isEmpty firstMessages "No errors returned"
                        Expect.isEmpty secondMessages "No errors returned"
                | _ ->
                    failwith "Should not happen"
        }

        test "Semantic analysis: skip analysis doesn't give any errors" {
            use db = createTestDatabase()
            
            match context (find "../examples/hashing/syntaxAnalysis-detectingSkipAnalysis.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                match SqlAnalysis.databaseSchema db.ConnectionString with
                | Result.Error connectionError ->
                    failwith connectionError
                | Result.Ok schema ->
                    let block = List.exactlyOne (SyntacticAnalysis.findSqlOperations context)
                    let messages = SqlAnalysis.analyzeOperation block db.ConnectionString schema
                    Expect.isEmpty messages "No errors returned"
        }

        test "Semantic analysis: casting non-nullable columns stays non-nullable" {
            use db = createTestDatabase()

            Sql.connect db.ConnectionString
            |> Sql.query "CREATE TABLE users (user_id bigserial primary key, username text not null)"
            |> Sql.executeNonQuery
            |> raiseWhenFailed

            match context (find "../examples/hashing/castingNonNullableStaysNonNullable.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                match SqlAnalysis.databaseSchema db.ConnectionString with
                | Result.Error connectionError ->
                    failwith connectionError
                | Result.Ok schema ->
                    let blocks = SyntacticAnalysis.findSqlOperations context
                    let messages = blocks |> List.collect (fun block -> SqlAnalysis.analyzeOperation block db.ConnectionString schema)
                    Expect.isEmpty messages "No errors returned"
        }

        test "Semantic analysis: incorrect queries in executeTransaction are detected" {
            use db = createTestDatabase()

            Sql.connect db.ConnectionString
            |> Sql.query "CREATE TABLE users (user_id bigserial primary key, username text not null)"
            |> Sql.executeNonQuery
            |> raiseWhenFailed

            match context (find "../examples/hashing/errorsInTransactions.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                match SqlAnalysis.databaseSchema db.ConnectionString with
                | Result.Error connectionError ->
                    failwith connectionError
                | Result.Ok schema ->
                    let block = List.exactlyOne (SyntacticAnalysis.findSqlOperations context)
                    let messages = SqlAnalysis.analyzeOperation block db.ConnectionString schema
                    Expect.equal 3 messages.Length "Three errors returned"
        }

        test "Semantic Analysis: empty parameter sets with missing parameters give error" {
            use db = createTestDatabase()

            Sql.connect db.ConnectionString
            |> Sql.query "CREATE TABLE users (user_id bigserial primary key, username text not null, active bit not null)"
            |> Sql.executeNonQuery
            |> raiseWhenFailed

            match context (find "../examples/hashing/detectingEmptyParameterSet.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                match SqlAnalysis.databaseSchema db.ConnectionString with
                | Result.Error connectionError ->
                    failwith connectionError
                | Result.Ok schema ->
                    let block = List.exactlyOne (SyntacticAnalysis.findSqlOperations context)
                    let messages = SqlAnalysis.analyzeOperation block db.ConnectionString schema
                    match messages with
                    | [ message ] ->
                        Expect.isTrue (message.IsWarning()) "The message is an warning"
                    | _ ->
                        failwith "Expected only one error message"
        }

        test "Semantic Analysis: parameter type mismatch" {
            use db = createTestDatabase()

            Sql.connect db.ConnectionString
            |> Sql.query "CREATE TABLE users (user_id bigserial primary key, username text not null, active bit not null)"
            |> Sql.executeNonQuery
            |> ignore

            match context (find "../examples/hashing/parameterTypeMismatch.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                match SqlAnalysis.databaseSchema db.ConnectionString with
                | Result.Error connectionError ->
                    failwith connectionError
                | Result.Ok schema ->
                    let block = List.exactlyOne (SyntacticAnalysis.findSqlOperations context)
                    let messages = SqlAnalysis.analyzeOperation block db.ConnectionString schema
                    match messages with
                    | [ message ] ->
                        Expect.isTrue (message.IsWarning()) "The message is an warning"
                        Expect.stringContains message.Message "Sql.int64" "Message should contain the missing column name"
                    | _ ->
                        failwith "Expected only one error message"
        }

        test "Syntactic Analysis: reading queries with extra processing after Sql.executeReader" {
            match context (find "../examples/hashing/syntacticAnalysisProcessedList.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                match SyntacticAnalysis.findSqlOperations context with
                | [ operation ] ->
                    match SqlAnalysis.findQuery operation with
                    | Some(query, range) ->
                        Expect.equal query "SELECT * FROM users" "Query should be read correctly"
                        match SqlAnalysis.findColumnReadAttempts operation with
                        | Some [ attempt ] ->
                            Expect.equal attempt.funcName "read.text" "Function name is correct"
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

        test "SQL schema analysis with arrays" {
            use db = createTestDatabase()

            Sql.connect db.ConnectionString
            |> Sql.query "CREATE TABLE users (user_id bigserial primary key, roles text[] not null)"
            |> Sql.executeNonQuery
            |> raiseWhenFailed

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
                Expect.equal 2 (List.length columns) "There are three columns"
                let rolesColumn = columns |> List.find (fun column -> column.Name = "roles")
                Expect.equal rolesColumn.DataType.Name "text" "The data type is text"
                Expect.isTrue rolesColumn.DataType.IsArray "The data type is an array"
                Expect.isFalse rolesColumn.Nullable "The column is not nullable"
        }

        test "SQL schema analysis with user defined arrays" {
            use db = createTestDatabase ()
  
            Sql.connect db.ConnectionString
            |> Sql.executeTransaction [
                "CREATE TYPE role AS ENUM ('admin')", []
                "CREATE TABLE users (roles role[])", [] ]
            |> raiseWhenFailed
  
            let databaseMetadata =
                InformationSchema.getDbSchemaLookups db.ConnectionString
  
            let userColumns =
                databaseMetadata.Schemas.["public"].Tables
                |> Seq.tryFind (fun pair -> pair.Key.Name = "users")
                |> Option.map (fun pair -> pair.Value)
                |> Option.map List.ofSeq
  
            match userColumns with
            | None -> failwith "Expected to find columns for users table"
            | Some columns ->
                Expect.equal 1 (List.length columns) "There is one column"
                let rolesColumn = columns |> List.find (fun column -> column.Name = "roles")
                Expect.equal rolesColumn.DataType.Name "role" "The data type is role"
                Expect.isTrue rolesColumn.DataType.IsArray "The data type is an array"
        }

        test "SQL query analysis" {
            use db = createTestDatabase()

            Sql.connect db.ConnectionString
            |> Sql.query "CREATE TABLE users (user_id bigserial primary key, username text not null, active bit not null)"
            |> Sql.executeNonQuery
            |> raiseWhenFailed

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
            |> raiseWhenFailed

            let databaseMetadata = InformationSchema.getDbSchemaLookups db.ConnectionString
            let query = "SELECT COUNT(*) FROM users"
            let resultSetMetadata = SqlAnalysis.extractParametersAndOutputColumns(db.ConnectionString, query, databaseMetadata)
            match resultSetMetadata with
            | Result.Error errorMsg ->
                failwithf "Could not analyse result set metadata %s" errorMsg
            | Result.Ok (parameters, outputColumns, _) ->
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
            |> raiseWhenFailed

            let databaseMetadata = InformationSchema.getDbSchemaLookups db.ConnectionString
            let query = "SELECT Increment(@Input)"
            let resultSetMetadata = SqlAnalysis.extractParametersAndOutputColumns(db.ConnectionString, query, databaseMetadata)
            match resultSetMetadata with
            | Result.Error errorMsg ->
                failwithf "Could not analyse result set metadata %s" errorMsg
            | Result.Ok (parameters, outputColumns, _) ->
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
            |> raiseWhenFailed

            match context (find "../examples/hashing/semanticAnalysis-missingColumn.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                match SqlAnalysis.databaseSchema db.ConnectionString with
                | Result.Error connectionError ->
                    failwith connectionError
                | Result.Ok schema ->
                    let block = List.exactlyOne (SyntacticAnalysis.findSqlOperations context)
                    let messages = SqlAnalysis.analyzeOperation block db.ConnectionString schema
                    match messages with
                    | [ message ] ->
                        Expect.isTrue (message.IsWarning()) "The message is an warning"
                        Expect.stringContains message.Message "non_existent" "Message should contain the missing column name"
                    | _ ->
                        failwith "Expected only one error message"
        }

        test "SQL query semantic analysis: missing parameter" {
            use db = createTestDatabase()

            Sql.connect db.ConnectionString
            |> Sql.query "CREATE TABLE users (user_id bigserial primary key, username text not null, active bit not null)"
            |> Sql.executeNonQuery
            |> raiseWhenFailed

            match context (find "../examples/hashing/semanticAnalysis-missingParameter.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                let block = List.exactlyOne (SyntacticAnalysis.findSqlOperations context)
                match SqlAnalysis.databaseSchema db.ConnectionString with
                | Result.Error connectionError ->
                    failwith connectionError
                | Result.Ok schema ->
                    let messages = SqlAnalysis.analyzeOperation block db.ConnectionString schema
                    match messages with
                    | [ message ] ->
                        Expect.isTrue (message.IsWarning()) "The message is a warning"
                        Expect.stringContains message.Message "Missing parameter 'active'"  "Error should say which parameter is not provided"
                    | _ ->
                        failwith "Expected only one error message"
        }

        test "SQL query semantic analysis: using dynamically referenced query doesn't give errors" {
            use db = createTestDatabase()

            Sql.connect db.ConnectionString
            |> Sql.query "CREATE TABLE users (user_id bigserial primary key, username text not null, active bit not null)"
            |> Sql.executeNonQuery
            |> raiseWhenFailed

            match context (find "../examples/hashing/syntaxAnalysisReferencingQueryDoesNotGiveError.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                let block = List.exactlyOne (SyntacticAnalysis.findSqlOperations context)
                match SqlAnalysis.databaseSchema db.ConnectionString with
                | Result.Error connectionError ->
                    failwith connectionError
                | Result.Ok schema ->
                    let messages = SqlAnalysis.analyzeOperation block db.ConnectionString schema
                    Expect.isEmpty messages "There should be no errors"
        }

        test "SQL query semantic analysis: type mismatch" {
            use db = createTestDatabase()

            Sql.connect db.ConnectionString
            |> Sql.query "CREATE TABLE users (user_id bigserial primary key, username text not null, active bit not null)"
            |> Sql.executeNonQuery
            |> raiseWhenFailed

            match context (find "../examples/hashing/semanticAnalysis-typeMismatch.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                match SqlAnalysis.databaseSchema db.ConnectionString with
                | Result.Error connectionError ->
                    failwith connectionError
                | Result.Ok schema ->
                    let operation = List.exactlyOne (SyntacticAnalysis.findSqlOperations context)
                    let messages = SqlAnalysis.analyzeOperation operation db.ConnectionString schema
                    match messages with
                    | [ message ] ->
                        Expect.stringContains message.Message "Please use read.bool instead" "Message contains suggestion to use Sql.readBool"
                    | _ ->
                        failwith "Expected only one error message"
        }

        test "SQL query semantic analysis: type mismatch when using text[]" {
            use db = createTestDatabase()

            Sql.connect db.ConnectionString
            |> Sql.query "CREATE TABLE users (user_id bigserial primary key, roles text[] not null)"
            |> Sql.executeNonQuery
            |> raiseWhenFailed

            match context (find "../examples/hashing/readingTextArray.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                match SqlAnalysis.databaseSchema db.ConnectionString with
                | Result.Error connectionError ->
                    failwith connectionError
                | Result.Ok schema ->
                    let operation = List.exactlyOne (SyntacticAnalysis.findSqlOperations context)
                    let messages = SqlAnalysis.analyzeOperation operation db.ConnectionString schema
                    match messages with
                    | [ message ] ->
                        Expect.stringContains message.Message "Please use read.stringArray instead" "Message contains suggestion to use Sql.stringArray"
                    | _ ->
                        failwith "Expected only one error message"
        }

        test "SQL query semantic analysis: type mismatch when using uuid[]" {
            use db = createTestDatabase()

            Sql.connect db.ConnectionString
            |> Sql.query "CREATE TABLE users (user_id bigserial primary key, codes uuid[] not null)"
            |> Sql.executeNonQuery
            |> raiseWhenFailed

            match context (find "../examples/hashing/readingUuidArray.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                match SqlAnalysis.databaseSchema db.ConnectionString with
                | Result.Error connectionError ->
                    failwith connectionError
                | Result.Ok schema ->
                    let operation = List.exactlyOne (SyntacticAnalysis.findSqlOperations context)
                    let messages = SqlAnalysis.analyzeOperation operation db.ConnectionString schema
                    match messages with
                    | [ message ] ->
                        Expect.stringContains message.Message "Please use read.uuidArray instead" "Message contains suggestion to use Sql.stringArray"
                    | _ ->
                        failwith "Expected only one error message"
        }

        test "SQL query semantic analysis: type mismatch when using int[] as parameter" {
            use db = createTestDatabase()

            Sql.connect db.ConnectionString
            |> Sql.query "CREATE TABLE users (user_id bigserial primary key, username text)"
            |> Sql.executeNonQuery
            |> raiseWhenFailed

            match context (find "../examples/hashing/usingIntArrayParameter.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                match SqlAnalysis.databaseSchema db.ConnectionString with
                | Result.Error connectionError ->
                    failwith connectionError
                | Result.Ok schema ->
                    let operation = List.exactlyOne (SyntacticAnalysis.findSqlOperations context)
                    let messages = SqlAnalysis.analyzeOperation operation db.ConnectionString schema
                    match messages with
                    | [ message ] ->
                        Expect.stringContains message.Message "Sql.intArray" "Message contains suggestion to use Sql.stringArray"
                    | _ ->
                        failwith "Expected only one error message"
        }

        test "SQL query semantic analysis: detect incorrectly used Sql.execute where as Sql.executeNonQuery was needed" {
            use db = createTestDatabase()

            Sql.connect db.ConnectionString
            |> Sql.query "CREATE TABLE users (user_id bigserial primary key, username text, roles text[] not null)"
            |> Sql.executeNonQuery
            |> ignore

            match context (find "../examples/hashing/executingQueryInsteadOfNonQuery.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                match SqlAnalysis.databaseSchema db.ConnectionString with
                | Result.Error connectionError ->
                    failwith connectionError
                | Result.Ok schema ->
                    let operation = List.exactlyOne (SyntacticAnalysis.findSqlOperations context)
                    let messages = SqlAnalysis.analyzeOperation operation db.ConnectionString schema
                    match messages with
                    | [ message ] ->
                        Expect.stringContains message.Message "Sql.executeNonQuery" "Message contains suggestion to use Sql.executeNonQuery"
                    | _ ->
                        failwith "Expected only one error message"
        }

        test "SQL query semantic analysis: detect incorrectly used nullable parameter on non-nullable column insert" {
            use db = createTestDatabase()

            Sql.connect db.ConnectionString
            |> Sql.query "CREATE TABLE users (user_id bigserial primary key, username text not null, active boolean not null)"
            |> Sql.executeNonQuery
            |> ignore

            match context (find "../examples/hashing/insertQueryWithNonNullableParameter.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                match SqlAnalysis.databaseSchema db.ConnectionString with
                | Result.Error connectionError ->
                    failwith connectionError
                | Result.Ok schema ->
                    let operation = List.exactlyOne (SyntacticAnalysis.findSqlOperations context)
                    let messages = SqlAnalysis.analyzeOperation operation db.ConnectionString schema
                    match messages with
                    | [ message ] ->
                        Expect.stringContains message.Message "Sql.text or Sql.string" "Message contains suggestion to use non-nullable text functions"
                    | _ ->
                        failwith "Expected only one error message"
        }

        test "SQL query semantic analysis: type mismatch with integer/serial" {
            use db = createTestDatabase()

            Sql.connect db.ConnectionString
            |> Sql.query "CREATE TABLE users (user_id serial primary key, username text not null, active bit not null)"
            |> Sql.executeNonQuery
            |> raiseWhenFailed

            match context (find "../examples/hashing/readAttemptIntegerTypeMismatch.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                match SqlAnalysis.databaseSchema db.ConnectionString with
                | Result.Error connectionError ->
                    failwith connectionError
                | Result.Ok schema ->
                    let operation = List.exactlyOne (SyntacticAnalysis.findSqlOperations context)
                    let messages = SqlAnalysis.analyzeOperation operation db.ConnectionString schema
                    match messages with
                    | [ message ] ->
                        Expect.stringContains message.Message "read.int" "Message contains suggestion to use Sql.readBool"
                    | _ ->
                        failwith "Expected only one error message"
        }

        test "SQL query semantic analysis: type mismatch with comparing against non-nullable column during SELECT" {
            use db = createTestDatabase()

            Sql.connect db.ConnectionString
            |> Sql.query "CREATE TABLE users (user_id serial primary key, username text not null)"
            |> Sql.executeNonQuery
            |> raiseWhenFailed

            match context (find "../examples/hashing/selectWithNonNullableColumnComparison.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                match SqlAnalysis.databaseSchema db.ConnectionString with
                | Result.Error connectionError ->
                    failwith connectionError
                | Result.Ok schema ->
                    let operation = List.exactlyOne (SyntacticAnalysis.findSqlOperations context)
                    let messages = SqlAnalysis.analyzeOperation operation db.ConnectionString schema
                    match messages with
                    | [ message ] ->
                        Expect.stringContains message.Message "Sql.int instead" "Message contains suggestion to use Sql.readBool"
                    | _ ->
                        failwith "Expected only one error message"
        }


        test "SQL query semantic analysis: type mismatch with comparing against non-nullable column during SELECT with JOINS" {
            use db = createTestDatabase()

            Sql.connect db.ConnectionString
            |> Sql.query "CREATE TABLE users (user_id serial primary key, username text not null)"
            |> Sql.executeNonQuery
            |> raiseWhenFailed

            Sql.connect db.ConnectionString
            |> Sql.query "CREATE TABLE user_roles (user_role_id serial primary key, role_description text not null, user_id int not null references users(user_id))"
            |> Sql.executeNonQuery
            |> raiseWhenFailed

            match context (find "../examples/hashing/selectWithInnerJoins.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                match SqlAnalysis.databaseSchema db.ConnectionString with
                | Result.Error connectionError ->
                    failwith connectionError
                | Result.Ok schema ->
                    let operation = List.exactlyOne (SyntacticAnalysis.findSqlOperations context)
                    let messages = SqlAnalysis.analyzeOperation operation db.ConnectionString schema
                    match messages with
                    | [ message ] ->
                        Expect.stringContains message.Message "Sql.int instead" "Message contains suggestion to use Sql.readBool"
                    | _ ->
                        failwith "Expected only one error message"
        }


        test "SQL query semantic analysis: type mismatch with comparing against non-nullable column during UPDATE with assignments" {
            use db = createTestDatabase()

            Sql.connect db.ConnectionString
            |> Sql.query "CREATE TABLE users (user_id serial primary key, username text not null, last_login timestamptz)"
            |> Sql.executeNonQuery
            |> raiseWhenFailed

            match context (find "../examples/hashing/updateQueryWithParametersInAssignments.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                match SqlAnalysis.databaseSchema db.ConnectionString with
                | Result.Error connectionError ->
                    failwith connectionError
                | Result.Ok schema ->
                    let operation = List.exactlyOne (SyntacticAnalysis.findSqlOperations context)
                    let messages = SqlAnalysis.analyzeOperation operation db.ConnectionString schema
                    match messages with
                    | [ firstMessage; secondMessage ] ->
                        Expect.stringContains firstMessage.Message "Sql.int instead" "Suggest to use non-nullable Sql.int"
                        Expect.stringContains secondMessage.Message "Sql.text or Sql.string" "Suggest to use non-nullable text"
                    | _ ->
                        failwith "Expected only one error message"
        }

        test "SQL query semantic analysis: redundant parameters" {
            use db = createTestDatabase()

            Sql.connect db.ConnectionString
            |> Sql.query "CREATE TABLE users (user_id bigserial primary key, username text not null, active bit not null)"
            |> Sql.executeNonQuery
            |> raiseWhenFailed

            match context (find "../examples/hashing/semanticAnalysis-redundantParameters.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                match SqlAnalysis.databaseSchema db.ConnectionString with
                | Result.Error connectionError ->
                    failwith connectionError
                | Result.Ok schema ->
                    let block = List.exactlyOne (SyntacticAnalysis.findSqlOperations context)
                    let messages = SqlAnalysis.analyzeOperation block db.ConnectionString schema
                    match messages with
                    | [ message ] ->
                        Expect.stringContains message.Message "Provided parameters are redundant" "Message contains suggestion to remove Sql.parameters"
                    | _ ->
                        failwith "Expected only one error message"
        }
    ]
