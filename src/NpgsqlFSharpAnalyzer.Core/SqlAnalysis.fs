namespace Npgsql.FSharp.Analyzers.Core

open System
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
                                    if providedParam.paramFunc <> "Sql.bit" &&  providedParam.paramFunc <> "Sql.bitOrNone" && providedParam.paramFunc <> "Sql.dbnull"
                                    then yield typeMismatch [ "Sql.bit"; "Sql.bitOrNone";"Sql.dbnull"]
                                | ("bool" | "boolean") ->
                                    if providedParam.paramFunc <> "Sql.bool" &&  providedParam.paramFunc <> "Sql.boolOrNone" && providedParam.paramFunc <> "Sql.dbnull"
                                    then yield typeMismatch [ "Sql.bool"; "Sql.boolOrNone"; "Sql.dbnull" ]
                                | ("int" | "int32" | "integer" | "serial") ->
                                    if providedParam.paramFunc <> "Sql.int" &&  providedParam.paramFunc <> "Sql.intOrNone" && providedParam.paramFunc <> "Sql.dbnull"
                                    then yield typeMismatch [ "Sql.int"; "Sql.intOrNone"; "Sql.dbnull" ]
                                | ("smallint" | "int16") ->
                                    if providedParam.paramFunc <> "Sql.int16" &&  providedParam.paramFunc <> "Sql.int16OrNone" && providedParam.paramFunc <> "Sql.dbnull"
                                    then yield typeMismatch [ "Sql.int16"; "Sql.int16OrNone"; "Sql.dbnull" ]
                                | ("int64" | "bigint" |"bigserial") ->
                                    if providedParam.paramFunc <> "Sql.int64" &&  providedParam.paramFunc <> "Sql.int64OrNone" && providedParam.paramFunc <> "Sql.dbnull"
                                    then yield typeMismatch [ "Sql.int64"; "Sql.int64OrNone"; "Sql.dbnull" ]
                                | ("numeric" | "decimal" | "money") ->
                                    if providedParam.paramFunc <> "Sql.decimal" &&  providedParam.paramFunc <> "Sql.decimalOrNone" && providedParam.paramFunc <> "Sql.dbnull"
                                    then yield typeMismatch [ "Sql.decimal"; "Sql.decimalOrNone"; "Sql.dbnull" ]
                                | "double precision" ->
                                    if providedParam.paramFunc <> "Sql.double" &&  providedParam.paramFunc <> "Sql.doubleOrNone" && providedParam.paramFunc <> "Sql.dbnull"
                                    then yield typeMismatch [ "Sql.double"; "Sql.doubleOrNone"; "Sql.dbnull" ]
                                | "bytea" ->
                                    if providedParam.paramFunc <> "Sql.bytea" &&  providedParam.paramFunc <> "Sql.byteaOrNone" && providedParam.paramFunc <> "Sql.dbnull"
                                    then yield typeMismatch [ "Sql.bytea"; "Sql.byteaOrNone"; "Sql.dbnull" ]
                                | "uuid" ->
                                    if providedParam.paramFunc <> "Sql.uuid" &&  providedParam.paramFunc <> "Sql.uuidOrNone" && providedParam.paramFunc <> "Sql.dbnull"
                                    then yield typeMismatch [ "Sql.uuid"; "Sql.uuidOrNone"; "Sql.dbnull" ]
                                | "date" ->
                                    if providedParam.paramFunc <> "Sql.date" &&  providedParam.paramFunc <> "Sql.dateOrNone" && providedParam.paramFunc <> "Sql.dbnull"
                                    then yield typeMismatch [ "Sql.date"; "Sql.dateOrNone"; "Sql.dbnull" ]
                                | ("timestamp"|"timestamp without time zone") ->
                                    if providedParam.paramFunc <> "Sql.timestamp" &&  providedParam.paramFunc <> "Sql.timestampOrNone" && providedParam.paramFunc <> "Sql.dbnull"
                                    then yield typeMismatch [ "Sql.timestamp"; "Sql.timestampOrNone"; "Sql.dbnull" ]
                                | ("timestamptz" | "timestamp with time zone") ->
                                    if providedParam.paramFunc <> "Sql.timestamptz" &&  providedParam.paramFunc <> "Sql.timestamptzOrNone" && providedParam.paramFunc <> "Sql.dbnull"
                                    then yield typeMismatch [ "Sql.timestamptz"; "Sql.timestamptzOrNone"; "Sql.dbnull" ]

                                | "jsonb" ->
                                    if providedParam.paramFunc <> "Sql.jsonb" && providedParam.paramFunc <> "Sql.jsonbOrNone" && providedParam.paramFunc <> "Sql.dbnull"
                                    then yield typeMismatch [ "Sql.jsonb"; "Sql.jsonbOrNone"; "Sql.dbnull" ]

                                | ("text"|"json"|"xml") ->
                                    if providedParam.paramFunc <> "Sql.text" && providedParam.paramFunc <> "Sql.textOrNone" && providedParam.paramFunc <> "Sql.string" && providedParam.paramFunc <> "Sql.textOrNone" && providedParam.paramFunc <> "Sql.dbnull"
                                    then yield typeMismatch [ "Sql.text"; "Sql.string"; "Sql.textOrNone"; "Sql.stringOrNone"; "Sql.dbnull" ]
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

                    let warningMsg =
                        if String.IsNullOrWhiteSpace attempt.columnName then
                            sprintf "Attempting to read a column without specifying a name. Available columns returned from the result set are:\n%s" (formatColumns availableColumns)
                        else
                            sprintf "Attempting to read column named '%s' that was not returned by the result set. Did you mean to read '%s'?\nAvailable columns from the result set are:\n%s" attempt.columnName closestAlternative (formatColumns availableColumns)

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
                            if column.Nullable && notUsing "boolOrNone"
                            then yield typeMismatch [ replace "boolOrNone" ]
                            else if notUsing "boolOrNone" && notUsing "bool"
                            then
                                if column.Nullable
                                then yield typeMismatch [ replace "boolOrNone" ]
                                else yield typeMismatch [ replace "bool" ]
                            else ()

                        | ("int8" | "tinyint") ->
                            if column.Nullable && List.forall notUsing [ "int8OrNone"; "int16OrNone"; "intOrNone"; "int64OrNone" ]
                            then yield typeMismatch [ replace "int8OrNone"; replace "int16OrNone"; replace "intOrNone"; replace "int64OrNone" ]
                            else if List.forall notUsing [ "int8OrNone"; "int16OrNone"; "intOrNone"; "int64OrNone"; "int8"; "int16"; "int"; "int64" ]
                            then
                                if column.Nullable
                                then yield typeMismatch [ replace "int8OrNone"; replace "int16OrNone"; replace "intOrNone"; replace "int64OrNone" ]
                                else yield typeMismatch [ replace "int8"; replace "int16"; replace "int"; replace "int64" ]

                        | ("int16"| "smallint") ->
                            if column.Nullable && List.forall notUsing [ "int16OrNone"; "intOrNone"; "int64OrNone" ]
                            then yield typeMismatch [ replace "int16OrNone"; replace "intOrNone"; replace "int64OrNone" ]
                            else if List.forall notUsing [ "int16OrNone"; "intOrNone"; "int64OrNone"; "int16"; "int"; "int64" ]
                            then
                                if column.Nullable
                                then yield typeMismatch [ replace "int16OrNone"; replace "intOrNone"; replace "int64OrNone" ]
                                else yield typeMismatch [ replace "int16"; replace "int"; replace "int64" ]

                        | ("int"|"integer"|"int32"|"serial"|"int4"|"int2") ->
                            if column.Nullable && List.forall notUsing [ "intOrNone"; "int64OrNone" ]
                            then yield typeMismatch [ replace "intOrNone"; replace "int64OrNone" ]
                            else if List.forall notUsing [ "intOrNone"; "int64OrNone"; "int"; "int64" ]
                            then
                                if column.Nullable
                                then yield typeMismatch [ replace "intOrNone"; replace "int64OrNone" ]
                                else yield typeMismatch [ replace "int"; replace "int64" ]

                        | ("int64"|"bigint"|"bigserial") ->
                            if column.Nullable && notUsing "int64OrNone"
                            then yield typeMismatch [ replace "int64OrNone" ]
                            else if notUsing "int64OrNone" && notUsing "int64"
                            then
                                if column.Nullable
                                then yield typeMismatch [ replace "int64OrNone" ]
                                else yield typeMismatch [ replace "int64" ]
                            else ()

                        | ("numeric"|"decimal"|"money") ->
                            if column.Nullable && notUsing "decimalOrNone"
                            then yield typeMismatch [ replace "decimalOrNone" ]
                            else if notUsing "decimalOrNone" && notUsing "decimal"
                            then
                                if column.Nullable
                                then yield typeMismatch [ replace "decimalOrNone" ]
                                else yield typeMismatch [ replace "decimal" ]
                            else ()

                        | "double precision" ->
                            if column.Nullable && notUsing "doubleOrNone"
                            then yield typeMismatch [ replace "doubleOrNone" ]
                            else if notUsing "doubleOrNone" && notUsing "double"
                            then
                                if column.Nullable
                                then yield typeMismatch [ replace "doubleOrNone" ]
                                else yield typeMismatch [ replace "double" ]
                            else ()

                        | ("text"|"json"|"xml"|"jsonb"|"varchar") ->
                            if column.Nullable && notUsing "textOrNone" && notUsing "stringOrNone"
                            then yield typeMismatch [ replace "textOrNone"; replace "stringOrNone" ]
                            else if notUsing "textOrNone" && notUsing "text" && notUsing "string" && notUsing "stringOrNone"
                            then
                                if column.Nullable
                                then yield typeMismatch [ replace "textOrNone"; replace "stringOrText" ]
                                else yield typeMismatch [ replace "text"; replace "string" ]
                            else ()

                        | "date" ->
                            if column.Nullable && notUsing "dateOrNone"
                            then yield typeMismatch [ replace "dateOrNone" ]
                            else if notUsing "dateOrNone" && notUsing "date"
                            then
                                if column.Nullable
                                then yield typeMismatch [ replace "dateOrNone" ]
                                else yield typeMismatch [ replace "date" ]
                            else ()

                        | ("timestamp"|"timestamp without time zone") ->
                            if column.Nullable && notUsing "timestampOrNone" && notUsing "dateTimeOrNone"
                            then yield typeMismatch [ replace "dateTimeOrNone"; replace "timestampOrNone" ]
                            else if notUsing "timestampOrNone" && notUsing "timestamp" && notUsing "dateTimeOrNone" && notUsing "dateTime"
                            then
                                if column.Nullable
                                then yield typeMismatch [ replace "dateTimeOrNone"; replace "timestampOrNone" ]
                                else yield typeMismatch [ replace "dateTime"; replace "timestamp" ]
                            else ()

                        | ("timestamptz"|"timestamp with time zone") ->
                            if column.Nullable && notUsing "timestamptzOrNone" && notUsing "dateTimeOrNone"
                            then yield typeMismatch [ replace "dateTimeOrNone"; replace "timestamptzOrNone" ]
                            //else if not column.Nullable && (using "timestamptzOrNone" || using "dateTimeOrNone")
                            //then yield typeMismatch [ replace "dateTime"; replace "timestamptz" ]
                            else if notUsing "timestamptzOrNone" && notUsing "timestamptz" && notUsing "dateTimeOrNone" && notUsing "dateTime"
                            then
                                if column.Nullable
                                then yield typeMismatch [ replace "dateTimeOrNone"; replace "timestamptzOrNone" ]
                                else yield typeMismatch [ replace "dateTime"; replace "timestamptz" ]
                            else ()

                        | "bytea" ->
                            if column.Nullable && notUsing "byteaOrNone"
                            then yield typeMismatch [ replace "byteaOrNone" ]
                            // else if not column.Nullable && using "byteaOrNone"
                            // then yield typeMismatch [ replace "bytea" ]
                            else if notUsing "byteaOrNone" && notUsing "bytea"
                            then
                                if column.Nullable
                                then yield typeMismatch [ replace "byteaOrNone" ]
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
