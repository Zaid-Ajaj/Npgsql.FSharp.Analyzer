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
                        else
                            // do parameter type-checking
                            let matchingColumnType = requiredParameters |> List.tryFind(fun p -> p.Name = providedParam.name)
                            match matchingColumnType with
                            | None -> ()
                            | Some requiredParam ->
                                let typeMismatch (shouldUse: string list) =
                                    let formattedSuggestions =
                                        shouldUse
                                        |> String.concat ", "
                                        |> sprintf "[%s]"

                                    let warning =
                                      sprintf "Attempting to provide parameter '%s' of type '%s' using function %s. Please use one of %s instead."
                                        providedParam.name
                                        requiredParam.DataType.Name
                                        providedParam.paramFunc
                                        formattedSuggestions

                                    let codeFixs : Fix list =
                                        shouldUse
                                        |> List.map (fun func ->
                                            {
                                                FromText = providedParam.name
                                                FromRange = providedParam.paramFuncRange
                                                ToText = func
                                            })

                                    { createWarning warning providedParam.paramFuncRange with Fixes = codeFixs }

                                match requiredParam.DataType.Name with
                                | "bit" ->
                                    if providedParam.paramFunc <> "Sql.bit" &&  providedParam.paramFunc <> "Sql.bitOrNull" && providedParam.paramFunc <> "Sql.dbnull"
                                    then yield typeMismatch [ "Sql.bit"; "Sql.bitOrNull";"Sql.dbnull"]
                                | ("bool" | "boolean") ->
                                    if providedParam.paramFunc <> "Sql.bool" &&  providedParam.paramFunc <> "Sql.boolOrNull" && providedParam.paramFunc <> "Sql.dbnull"
                                    then yield typeMismatch [ "Sql.bool"; "Sql.boolOrNull"; "Sql.dbnull" ]
                                | ("int" | "int32" | "integer" | "serial") ->
                                    if providedParam.paramFunc <> "Sql.int" &&  providedParam.paramFunc <> "Sql.intOrNull" && providedParam.paramFunc <> "Sql.dbnull"
                                    then yield typeMismatch [ "Sql.int"; "Sql.intOrNull"; "Sql.dbnull" ]
                                | ("smallint" | "int16") ->
                                    if providedParam.paramFunc <> "Sql.int16" &&  providedParam.paramFunc <> "Sql.int16OrNull" && providedParam.paramFunc <> "Sql.dbnull"
                                    then yield typeMismatch [ "Sql.int16"; "Sql.int16OrNull"; "Sql.dbnull" ]
                                | ("int64" | "bigint" |"bigserial") ->
                                    if providedParam.paramFunc <> "Sql.int64" &&  providedParam.paramFunc <> "Sql.int64OrNull" && providedParam.paramFunc <> "Sql.dbnull"
                                    then yield typeMismatch [ "Sql.int64"; "Sql.int64OrNull"; "Sql.dbnull" ]
                                | ("numeric" | "decimal" | "money") ->
                                    if providedParam.paramFunc <> "Sql.decimal" &&  providedParam.paramFunc <> "Sql.decimalOrNull" && providedParam.paramFunc <> "Sql.dbnull"
                                    then yield typeMismatch [ "Sql.decimal"; "Sql.decimalOrNull"; "Sql.dbnull" ]
                                | "double precision" ->
                                    if providedParam.paramFunc <> "Sql.double" &&  providedParam.paramFunc <> "Sql.doubleOrNull" && providedParam.paramFunc <> "Sql.dbnull"
                                    then yield typeMismatch [ "Sql.double"; "Sql.doubleOrNull"; "Sql.dbnull" ]
                                | "bytea" ->
                                    if providedParam.paramFunc <> "Sql.bytea" &&  providedParam.paramFunc <> "Sql.byteaOrNull" && providedParam.paramFunc <> "Sql.dbnull"
                                    then yield typeMismatch [ "Sql.bytea"; "Sql.byteaOrNull"; "Sql.dbnull" ]
                                | "uuid" ->
                                    if providedParam.paramFunc <> "Sql.uuid" &&  providedParam.paramFunc <> "Sql.uuidOrNull" && providedParam.paramFunc <> "Sql.dbnull"
                                    then yield typeMismatch [ "Sql.uuid"; "Sql.uuidOrNull"; "Sql.dbnull" ]
                                | "date" ->
                                    if providedParam.paramFunc <> "Sql.date" &&  providedParam.paramFunc <> "Sql.dateOrNull" && providedParam.paramFunc <> "Sql.dbnull"
                                    then yield typeMismatch [ "Sql.date"; "Sql.dateOrNull"; "Sql.dbnull" ]
                                | ("timestamp"|"timestamp without time zone") ->
                                    if providedParam.paramFunc <> "Sql.timestamp" &&  providedParam.paramFunc <> "Sql.timestampOrNull" && providedParam.paramFunc <> "Sql.dbnull"
                                    then yield typeMismatch [ "Sql.timestamp"; "Sql.timestampOrNull"; "Sql.dbnull" ]
                                | ("timestamptz" | "timestamp with time zone") ->
                                    if providedParam.paramFunc <> "Sql.timestamptz" &&  providedParam.paramFunc <> "Sql.timestamptzOrNull" && providedParam.paramFunc <> "Sql.dbnull"
                                    then yield typeMismatch [ "Sql.timestamptz"; "Sql.timestamptzOrNull"; "Sql.dbnull" ]
                                | ("text"|"json"|"xml"|"jsonb") ->
                                    if providedParam.paramFunc <> "Sql.text" && providedParam.paramFunc <> "Sql.textOrNull" && providedParam.paramFunc <> "Sql.string" && providedParam.paramFunc <> "Sql.textOrNull" && providedParam.paramFunc <> "Sql.dbnull"
                                    then yield typeMismatch [ "Sql.text"; "Sql.string"; "Sql.textOrNull"; "Sql.stringOrNull"; "Sql.dbnull" ]
                                | _ ->
                                    ()
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
                    let typeMismatchMessage (shouldUse: string list) =
                        let formattedFunctions =
                            shouldUse
                            |> String.concat ", "
                            |> sprintf "[%s]"

                        sprintf "Type mismatch: attempting to read column '%s' of type '%s' (nullable = %b) using %s. Please use one of %s instead."
                            column.Name column.DataType.Name column.Nullable  attempt.funcName formattedFunctions

                    let typeMismatch (shouldUse: string list) =
                        let warningMessage = typeMismatchMessage shouldUse
                        let fixes =
                            shouldUse
                            |> List.map (fun func -> {
                                FromRange = attempt.funcCallRange
                                FromText = attempt.funcName
                                ToText = func
                            })

                        { createWarning warningMessage attempt.funcCallRange with Fixes = fixes }

                    if attempt.funcName.StartsWith "Sql." then
                        match column.DataType.Name with
                        | ("bit"|"bool"|"boolean") when attempt.funcName <> "Sql.readBool" ->
                            yield typeMismatch [ "Sql.readBool" ]
                        | ("text"|"json"|"xml"|"jsonb") when attempt.funcName <> "Sql.readString" ->
                            yield typeMismatch [ "Sql.readString" ]
                        | ("character varying"|"character"|"char"|"varchar"|"citext") when attempt.funcName <> "Sql.readString" ->
                            yield typeMismatch [ "Sql.readString" ]
                        | ("int" | "int2" | "int4" | "smallint" | "integer") when attempt.funcName <> "Sql.readInt" ->
                            yield typeMismatch [ "Sql.readInt" ]
                        | ("int8" | "bigint") when attempt.funcName <> "Sql.readLong" ->
                            yield typeMismatch [ "Sql.readLong" ]
                        | ("real" | "float4" | "double precision" | "float8") when attempt.funcName <> "Sql.readNumber" ->
                            yield typeMismatch ["Sql.readNumber"]
                        | ("numeric" | "decimal" | "money") when attempt.funcName <> "Sql.readDecimal" || attempt.funcName <> "Sql.readMoney" ->
                            yield typeMismatch ["Sql.readDecimal"]
                        | "bytea" when attempt.funcName <> "Sql.readBytea" ->
                            yield typeMismatch ["Sql.readBytea"]
                        | "uuid" when attempt.funcName <> "Sql.readUuid" ->
                            yield typeMismatch [ "Sql.readUuid" ]
                        | "date" when attempt.funcName <> "Sql.readDate" ->
                            yield typeMismatch [ "Sql.readDate" ]
                        | ("timestamp"|"timestamp without time zone") when attempt.funcName <> "Sql.readTimestamp" ->
                            yield typeMismatch [ "Sql.readTimestamp" ]
                        | ("timestamptz"|"timestamp with time zone") when attempt.funcName <> "Sql.readTimestampTz" ->
                            yield typeMismatch [ "Sql.readTimestampTz" ]
                        | ("interval" | "time without time zone" | "time") when attempt.funcName <> "Sql.readTime" ->
                            yield typeMismatch [ "Sql.readTime" ]
                        | _ ->
                            ()
                    else
                        let replace (name: string) =
                            match attempt.funcName.Split '.' with
                            | [| reader; funcName |] -> sprintf "%s.%s" reader name
                            | _ -> attempt.funcName

                        let using (name: string) = attempt.funcName.EndsWith (sprintf ".%s" name)
                        let notUsing = using >> not

                        match column.DataType.Name with
                        ("bit"|"bool"|"boolean") ->
                            if column.Nullable && notUsing "boolOrNull"
                            then yield typeMismatch [ replace "boolOrNull" ]
                            // else if not column.Nullable && using "boolOrNull"
                            // then yield typeMismatch [ replace "bool" ]
                            else if notUsing "boolOrNull" && notUsing "bool"
                            then
                                if column.Nullable
                                then yield typeMismatch [ replace "boolOrNull" ]
                                else yield typeMismatch [ replace "bool" ]
                            else ()

                        | ("int8" | "tinyint") ->
                            if column.Nullable && List.forall notUsing [ "int8orNull"; "int16OrNull"; "intOrNull"; "int64OrNull" ]
                            then yield typeMismatch [ replace "int8OrNull"; replace "int16OrNull"; replace "intOrNull"; replace "int64OrNull" ]
                            // else if not column.Nullable && using "int8OrNull"
                            // then yield typeMismatch [ replace "int8"; replace "int16"; replace "int"; replace "int64" ]
                            // else if not column.Nullable && using "int16OrNull"
                            // then yield typeMismatch [ replace "int8"; replace "int16"; replace "int"; replace "int64" ]
                            // else if not column.Nullable && using "intdOrNull"
                            // then yield typeMismatch [ replace "int8"; replace "int16"; replace "int"; replace "int64" ]
                            // else if not column.Nullable && using "int64OrNull"
                            // then yield typeMismatch [ replace "int8"; replace "int16"; replace "int"; replace "int64" ]
                            else if List.forall notUsing [ "int8OrNull"; "int16OrNull"; "intOrNull"; "int64OrNull"; "int8"; "int16"; "int"; "int64" ]
                            then
                                if column.Nullable
                                then yield typeMismatch [ replace "int8OrNull"; replace "int16OrNull"; replace "intOrNull"; replace "int64OrNull" ]
                                else yield typeMismatch [ replace "int8"; replace "int16"; replace "int"; replace "int64" ]

                        | ("int16"| "smallint") ->
                            if column.Nullable && List.forall notUsing [ "int16OrNull"; "intOrNull"; "int64OrNull" ]
                            then yield typeMismatch [ replace "int16OrNull"; replace "intOrNull"; replace "int64OrNull" ]
                            //else if not column.Nullable && using "int16OrNull"
                            //then yield typeMismatch [ replace "int16"; replace "int"; replace "int64" ]
                            //else if not column.Nullable && using "intOrNull"
                            //then yield typeMismatch [ replace "int16"; replace "int"; replace "int64" ]
                            //else if not column.Nullable && using "int64OrNull"
                            //then yield typeMismatch [ replace "int16"; replace "int"; replace "int64" ]
                            else if List.forall notUsing [ "int16OrNull"; "intOrNull"; "int64OrNull"; "int16"; "int"; "int64" ]
                            then
                                if column.Nullable
                                then yield typeMismatch [ replace "int16OrNull"; replace "intOrNull"; replace "int64OrNull" ]
                                else yield typeMismatch [ replace "int16"; replace "int"; replace "int64" ]

                        | ("int"|"integer"|"int32"|"serial"|"int4") ->
                            if column.Nullable && List.forall notUsing [ "intOrNull"; "int64OrNull" ]
                            then yield typeMismatch [ replace "intOrNull"; replace "int64OrNull" ]
                            // else if not column.Nullable && using "intOrNull"
                            // then yield typeMismatch [ replace "int"; replace "int64" ]
                            // else if not column.Nullable && using "int64OrNull"
                            // then yield typeMismatch [ replace "int"; replace "int64" ]
                            else if List.forall notUsing [ "intOrNull"; "int64OrNull"; "int"; "int64" ]
                            then
                                if column.Nullable
                                then yield typeMismatch [ replace "intOrNull"; replace "int64OrNull" ]
                                else yield typeMismatch [ replace "int"; replace "int64" ]

                        | ("int64"|"bigint"|"bigserial") ->
                            if column.Nullable && notUsing "int64OrNull"
                            then yield typeMismatch [ replace "int64OrNull" ]
                            // else if not column.Nullable && using "int64OrNull"
                            // then yield typeMismatch [ replace "int64" ]
                            else if notUsing "int64OrNull" && notUsing "int64"
                            then
                                if column.Nullable
                                then yield typeMismatch [ replace "int64OrNull" ]
                                else yield typeMismatch [ replace "int64" ]
                            else ()

                        | ("numeric"|"decimal"|"money") ->
                            if column.Nullable && notUsing "decimalOrNull"
                            then yield typeMismatch [ replace "decimalOrNull" ]
                            // else if not column.Nullable && using "decimalOrNull"
                            // then yield typeMismatch [ replace "decimal" ]
                            else if notUsing "decimalOrNull" && notUsing "decimal"
                            then
                                if column.Nullable
                                then yield typeMismatch [ replace "decimalOrNull" ]
                                else yield typeMismatch [ replace "decimal" ]
                            else ()

                        | "double precision" ->
                            if column.Nullable && notUsing "doubleOrNull"
                            then yield typeMismatch [ replace "doubleOrNull" ]
                            // else if not column.Nullable && using "doubleOrNull"
                            // then yield typeMismatch [ replace "double" ]
                            else if notUsing "doubleOrNull" && notUsing "double"
                            then
                                if column.Nullable
                                then yield typeMismatch [ replace "doubleOrNull" ]
                                else yield typeMismatch [ replace "double" ]
                            else ()

                        | ("text"|"json"|"xml"|"jsonb") ->
                            if column.Nullable && notUsing "textOrNull" && notUsing "stringOrNull"
                            then yield typeMismatch [ replace "textOrNull"; replace "stringOrNull" ]
                            //else if not column.Nullable && (using "textOrNull" || using "stringOrNull")
                            //then yield typeMismatch [ replace "text"; replace "string" ]
                            else if notUsing "textOrNull" && notUsing "text" && notUsing "string" && notUsing "stringOrNull"
                            then
                                if column.Nullable
                                then yield typeMismatch [ replace "textOrNull"; replace "stringOrText" ]
                                else yield typeMismatch [ replace "text"; replace "string" ]
                            else ()
                             
                        | "date" ->
                            if column.Nullable && notUsing "dateOrNull"
                            then yield typeMismatch [ replace "dateOrNull" ]
                            //else if not column.Nullable && using "dateOrNull"
                            //then yield typeMismatch [ replace "date" ]
                            else if notUsing "dateOrNull" && notUsing "date"
                            then
                                if column.Nullable
                                then yield typeMismatch [ replace "dateOrNull" ]
                                else yield typeMismatch [ replace "date" ]
                            else ()

                        | ("timestamp"|"timestamp without time zone") ->
                            if column.Nullable && notUsing "timestampOrNull" && notUsing "dateTimeOrNull"
                            then yield typeMismatch [ replace "dateTimeOrNull"; replace "timestampOrNull" ]
                            //else if not column.Nullable && (using "timestampOrNull" || using "dateTimeOrNull")
                            //then yield typeMismatch [ replace "dateTime"; replace "timestamp" ]
                            else if notUsing "timestampOrNull" && notUsing "timestamp" && notUsing "dateTimeOrNull" && notUsing "dateTime"
                            then
                                if column.Nullable
                                then yield typeMismatch [ replace "dateTimeOrNull"; replace "timestampOrNull" ]
                                else yield typeMismatch [ replace "dateTime"; replace "timestamp" ]
                            else ()

                        | ("timestamptz"|"timestamp with time zone") ->
                            if column.Nullable && notUsing "timestamptzOrNull" && notUsing "dateTimeOrNull"
                            then yield typeMismatch [ replace "dateTimeOrNull"; replace "timestamptzOrNull" ]
                            //else if not column.Nullable && (using "timestamptzOrNull" || using "dateTimeOrNull")
                            //then yield typeMismatch [ replace "dateTime"; replace "timestamptz" ]
                            else if notUsing "timestamptzOrNull" && notUsing "timestamptz" && notUsing "dateTimeOrNull" && notUsing "dateTime"
                            then
                                if column.Nullable
                                then yield typeMismatch [ replace "dateTimeOrNull"; replace "timestamptzOrNull" ]
                                else yield typeMismatch [ replace "dateTime"; replace "timestamptz" ]
                            else ()

                        | "bytea" ->
                            if column.Nullable && notUsing "byteaOrNull"
                            then yield typeMismatch [ replace "byteaOrNull" ]
                            // else if not column.Nullable && using "byteaOrNull"
                            // then yield typeMismatch [ replace "bytea" ]
                            else if notUsing "byteaOrNull" && notUsing "bytea"
                            then
                                if column.Nullable
                                then yield typeMismatch [ replace "byteaOrNull" ]
                                else yield typeMismatch [ replace "bytea" ]
                            else ()
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
