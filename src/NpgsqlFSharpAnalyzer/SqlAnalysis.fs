namespace Npgsql.FSharp.Analyzers

open FSharp.Analyzers.SDK
open FSharp.Compiler.Range
open F23.StringSimilarity

module SqlAnalysis =
    let extractParametersAndOutputColumns(connectionString, commandText, dbSchemaLookups) =
        try
            let parameters, output, enums = InformationSchema.extractParametersAndOutputColumns(connectionString, commandText, false, dbSchemaLookups)
            Result.Ok (parameters, output)
        with
        | ex ->
            Result.Error ex.Message

    let createWarning (message: string) (range: range) : Message =
        { Message = message;
          Type = "SQL Analysis";
          Code = "SQL0001";
          Severity = Warning;
          Range = range;
          Fixes = [ ] }

    let createError (message: string) (range: range) : Message =
        { Message = message;
          Type = "SQL Analysis";
          Code = "SQL0002";
          Severity = Error;
          Range = range;
          Fixes = [ ] }

    let findQuery (operation: SqlOperation) =
        operation.blocks
        |> List.tryFind (function | SqlAnalyzerBlock.Query(query, range) -> true | _ -> false)
        |> Option.map(function | SqlAnalyzerBlock.Query(query, range) -> (query, range) | _ -> failwith "should not happen")

    let findParameters (operation: SqlOperation) =
        operation.blocks
        |> List.tryFind (function | SqlAnalyzerBlock.Parameters(parameters, range) -> true | _ -> false)
        |> Option.map(function | SqlAnalyzerBlock.Parameters(parameters, range) -> (parameters, range) | _ -> failwith "should not happen")

    let findColumnReadAttempts (operation: SqlOperation) =
        operation.blocks
        |> List.tryFind (function | SqlAnalyzerBlock.ReadingColumns(attempts) -> true | _ -> false)
        |> Option.map(function | SqlAnalyzerBlock.ReadingColumns(attempts) -> attempts | _ -> failwith "should not happen")

    let analyzeParameters (operation: SqlOperation) (requiredParameters: InformationSchema.Parameter list) =
        match findParameters operation with
        | None ->
            if not (List.isEmpty requiredParameters) then
                let missingParameters =
                    requiredParameters
                    |> List.map (fun p -> sprintf "%s:%s" p.Name p.DataType.Name)
                    |> String.concat ", "
                    |> sprintf "Missing parameters [%s]. Please use Sql.parameters to provide them."
                [ createWarning missingParameters operation.range ]
            else
                [ ]

        | Some (queryParams, queryParamsRange) ->
            if List.isEmpty requiredParameters then
                [ createWarning "Provided parameters are redundant. Sql query is not parameterized" operation.range ]
            else 
                let missingParameters = [
                    for requiredParameter in requiredParameters do
                        if not (queryParams |> List.exists (fun p -> p.parameter.TrimStart('@') = requiredParameter.Name))
                        then
                            let message = sprintf "Missing parameter '%s' of type %s" requiredParameter.Name requiredParameter.DataType.Name
                            yield createWarning message queryParamsRange

                    for providedParam in queryParams do
                        if not (requiredParameters |> List.exists (fun p -> p.Name = providedParam.parameter.TrimStart('@')))
                        then
                            let levenshtein = new NormalizedLevenshtein()
                            let closestAlternative =
                                requiredParameters
                                |> List.minBy (fun parameter -> levenshtein.Distance(parameter.Name, providedParam.parameter))
                                |> fun parameter -> parameter.Name
                                 
                            let expectedParameters =
                                requiredParameters
                                |> List.map (fun p -> sprintf "%s:%s" p.Name p.DataType.Name)
                                |> String.concat ", "
                                |> sprintf "Required parameters are [%s]."
                            yield createWarning (sprintf "Unexpected parameter '%s' is provided. Did you mean '%s'? %s" providedParam.parameter closestAlternative expectedParameters) providedParam.range
                ]

                missingParameters

    let findColumn (name: string) (availableColumns: InformationSchema.Column list) =
        availableColumns
        |> List.tryFind (fun column -> column.Name = name)

    let formatColumns (availableColumns: InformationSchema.Column list) =
        availableColumns
        |> List.map (fun column -> sprintf "| -- %s of type %s" column.Name column.DataType.Name)
        |> String.concat "\n"

    let analyzeColumnReadingAttempts (columnReadAttempts: ColumnReadAttempt list) (availableColumns: InformationSchema.Column list) =
        [
            for attempt in columnReadAttempts do
                match findColumn attempt.columnName availableColumns with
                | None ->
                    let levenshtein = new NormalizedLevenshtein()
                    let closestAlternative =
                        availableColumns
                        |> List.minBy (fun column -> levenshtein.Distance(attempt.columnName, column.Name))
                        |> fun column -> column.Name

                    let warningMsg = sprintf "Attempting to read column named '%s' that was not returned by the result set. Did you mean to read '%s'?\nAvailable columns from the result set are:\n%s" attempt.columnName closestAlternative (formatColumns availableColumns)
                    yield createWarning warningMsg attempt.funcCallRange

                | Some column ->
                    let typeMismatchMessage (shouldUse: string) =
                        sprintf "Type mismatch: attempting to read column '%s' of type '%s' using %s. Please use %s instead."
                            column.Name column.DataType.Name attempt.funcName shouldUse

                    let typeMismatch (shouldUse: string) =
                        { createWarning (typeMismatchMessage shouldUse) attempt.funcCallRange with
                            Fixes = [
                                {  FromRange = attempt.funcCallRange
                                   FromText = sprintf "%s \"%s\"" attempt.funcName attempt.columnName
                                   ToText = sprintf "%s \"%s\"" shouldUse attempt.columnName }
                            ]
                        }

                    match column.DataType.Name with
                    | ("bit"|"boolean") when attempt.funcName <> "Sql.readBool" ->
                        yield typeMismatch "Sql.readBool"
                    | ("text"|"json"|"xml"|"jsonb") when attempt.funcName <> "Sql.readString" ->
                        yield typeMismatch "Sql.readString"
                    | ("character varying"|"character"|"char"|"varchar"|"citext") when attempt.funcName <> "Sql.readString" ->
                        yield typeMismatch "Sql.readString"
                    | ("int" | "int2" | "int4" | "smallint" | "integer") when attempt.funcName <> "Sql.readInt" ->
                        yield typeMismatch "Sql.readInt"
                    | ("int8" | "bigint") when attempt.funcName <> "Sql.readLong" ->
                        yield typeMismatch "Sql.readLong"
                    | ("real" | "float4" | "double precision" | "float8") when attempt.funcName <> "Sql.readNumber" ->
                        yield typeMismatch "Sql.readNumber"
                    | ("numeric" | "decimal" | "money") when attempt.funcName <> "Sql.readDecimal" || attempt.funcName <> "Sql.readMoney" ->
                        yield typeMismatch "Sql.readDecimal"
                    | "bytea" when attempt.funcName <> "Sql.readBytea" ->
                        yield typeMismatch "Sql.readBytea"
                    | "uuid" when attempt.funcName <> "Sql.readUuid" ->
                        yield typeMismatch "Sql.readUuid"
                    | "date" when attempt.funcName <> "Sql.readDate" ->
                        yield typeMismatch "Sql.readDate"
                    | ("timestamp"|"timestamp without time zone") when attempt.funcName <> "Sql.readTimestamp" ->
                        yield typeMismatch "Sql.readTimestamp"
                    | ("timestamptz"|"timestamp with time zone") when attempt.funcName <> "Sql.readTimestampTz" ->
                        yield typeMismatch "Sql.readTimestampTz"
                    | ("interval" | "time without time zone" | "time") when attempt.funcName <> "Sql.readTime" ->
                        yield typeMismatch "Sql.readTime"
                    | _ ->
                        ()
        ]

    /// Tries to read the database schema from the connection string
    let databaseSchema connectionString =
        try Result.Ok (InformationSchema.getDbSchemaLookups(connectionString))
        with | ex -> Result.Error ex.Message

    let analyzeBlock (operation: SqlOperation) (connectionString: string) : Message list =
        match findQuery operation with
        | None ->
            [ ]
        | Some (query, queryRange) ->
            match databaseSchema connectionString with
            | Result.Error connectionError ->
                [ createWarning connectionError queryRange ]
            | Result.Ok schema ->
                let queryAnalysis = extractParametersAndOutputColumns(connectionString, query, schema)
                match queryAnalysis with
                | Result.Error queryError ->
                    [ createWarning queryError queryRange ]
                | Result.Ok (parameters, outputColunms) ->
                    let readingAttempts = defaultArg (findColumnReadAttempts operation) [ ]
                    [
                        yield! analyzeParameters operation parameters
                        yield! analyzeColumnReadingAttempts readingAttempts outputColunms
                    ]
