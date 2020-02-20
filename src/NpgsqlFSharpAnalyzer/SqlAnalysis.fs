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
                match requiredParameters, queryParams with
                | [ requiredParam ], [ providedParam ] ->
                    // match simple case when there is one parameter provided with mismatched name
                    [
                        if requiredParam.Name <> providedParam.name
                        then
                            let message = sprintf "Unexpected provided paramter '%s'. Did you mean '%s'?" providedParam.name requiredParam.Name
                            let fix =
                              { FromRange = providedParam.range
                                FromText = providedParam.name
                                ToText = sprintf "\"%s\"" requiredParam.Name }

                            yield  { createWarning message providedParam.range with Fixes = [fix] }
                    ]
                | _ ->
                    [
                        /// None of the required parameters have the name of this provided parameter
                        let isUnknown (parameter: UsedParameter) =
                            requiredParameters
                            |> List.forall (fun requiredParam -> parameter.name <> requiredParam.Name)

                        let isRedundant (parameter: UsedParameter) =
                            // every required parameter has a corresponding provided query parameter
                            let requirementSatisfied =
                                requiredParameters
                                |> List.forall (fun requiredParam -> queryParams |> List.exists (fun queryParam -> queryParam.name = requiredParam.Name))

                            requirementSatisfied && isUnknown parameter

                        for requiredParameter in requiredParameters do
                            if not (queryParams |> List.exists (fun providedParam -> providedParam.name = requiredParameter.Name))
                            then
                                let message = sprintf "Missing parameter '%s' of type %s" requiredParameter.Name requiredParameter.DataType.Name
                                yield createWarning message queryParamsRange

                        for providedParam in queryParams do
                            if isRedundant providedParam then
                                yield createWarning (sprintf "Provided parameter '%s' is redundant. The query does not require such parameter" providedParam.name) providedParam.range

                            else if isUnknown providedParam then

                                // parameters that haven't been provided yet
                                let remainingParameters =
                                    requiredParameters
                                    |> List.filter (fun requiredParam -> not (queryParams |> List.exists (fun queryParam -> queryParam.name = requiredParam.Name)))

                                let levenshtein = new NormalizedLevenshtein()
                                let closestAlternative =
                                    remainingParameters
                                    |> List.minBy (fun parameter -> levenshtein.Distance(parameter.Name, providedParam.name))
                                    |> fun parameter -> parameter.Name

                                let expectedParameters =
                                    remainingParameters
                                    |> List.map (fun p -> sprintf "%s:%s" p.Name p.DataType.Name)
                                    |> String.concat ", "
                                    |> sprintf "Required parameters are [%s]."

                                let codeFixes =
                                    remainingParameters
                                    |> List.map (fun p ->
                                        { FromRange = providedParam.range
                                          FromText = providedParam.name
                                          ToText = sprintf "\"%s\"" p.Name })

                                let warning = createWarning (sprintf "Unexpected parameter '%s' is provided. Did you mean '%s'? %s" providedParam.name closestAlternative expectedParameters) providedParam.range

                                yield { warning with Fixes = codeFixes }
                    ]

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
                    let warning = createWarning warningMsg attempt.columnNameRange
                    let codeFixes =
                        availableColumns
                        |> List.map (fun column ->
                            { FromRange = attempt.columnNameRange
                              FromText = attempt.columnName
                              ToText = sprintf "\"%s\"" column.Name })
                    
                    yield { warning with Fixes = codeFixes }

                | Some column ->
                    let typeMismatchMessage (shouldUse: string) =
                        sprintf "Type mismatch: attempting to read column '%s' of type '%s' using %s. Please use %s instead."
                            column.Name column.DataType.Name attempt.funcName shouldUse

                    let typeMismatch (shouldUse: string) =
                        { createWarning (typeMismatchMessage shouldUse) attempt.funcCallRange with
                            Fixes = [
                                {  FromRange = attempt.funcCallRange
                                   FromText = attempt.funcName
                                   ToText = shouldUse }
                            ]
                        }

                    match column.DataType.Name with
                    | ("bit"|"bool"|"boolean") when attempt.funcName <> "Sql.readBool" ->
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

    /// Uses database schema that is retrieved once during initialization
    /// and re-used when analyzing the rest of the Sql operation blocks
    let analyzeOperation (operation: SqlOperation) (connectionString: string) (schema: InformationSchema.DbSchemaLookups) =
        match findQuery operation with
        | None ->
            [ ]
        | Some (query, queryRange) ->
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
