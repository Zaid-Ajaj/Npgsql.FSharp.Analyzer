namespace Npgsql.FSharp.Analyzers.Core

open System
open FSharp.Compiler.Range
open F23.StringSimilarity
open NpgsqlFSharpParser
open InformationSchema
open Npgsql

module SqlAnalysis =

    type ComparisonParameter = {
        parameterName : string
        column : string
        table : string option 
    }

    let rec trimParams = function
        | Expr.Parameter(name) -> Expr.Parameter(name.TrimStart '@')

        | Expr.Function(funcName, arguments) ->
            let modifiedArguments = List.map trimParams arguments
            Expr.Function(funcName, modifiedArguments)

        | Expr.SelectQuery query ->
            Expr.SelectQuery {
                query with
                    Columns = List.map trimParams query.Columns
                    Where = Option.map trimParams query.Where
                    Limit = Option.map trimParams query.Limit
                    GroupBy = List.map trimParams query.GroupBy
                    Having = Option.map trimParams query.Having
                    Offset = Option.map trimParams query.Offset
            }

        | Expr.InsertQuery query ->
            Expr.InsertQuery {
                query with
                    Values = List.map trimParams query.Values
                    Returning = List.map trimParams query.Returning
                    ConflictResolution =
                        query.ConflictResolution
                        |> List.map (fun (column, expr) -> column, trimParams expr) 
            }

        | Expr.DeleteQuery query ->
            Expr.DeleteQuery {
                query with
                    Where = Option.map trimParams query.Where
                    Returning = List.map trimParams query.Returning
            }

        | Expr.UpdateQuery query ->
            Expr.UpdateQuery {
                query with
                    Where = Option.map trimParams query.Where
                    Assignments = List.map trimParams query.Assignments
                    ConflictResolution = List.map trimParams query.ConflictResolution
                    Returning = List.map trimParams query.Returning
            }

        | Expr.And(left, right) -> Expr.And(trimParams left, trimParams right)

        | Expr.Or(left, right) -> Expr.Or(trimParams left, trimParams right)

        | Expr.Equals(left, right) -> Expr.Equals(trimParams left, trimParams right)

        | Expr.Not(expr) -> Expr.Not(trimParams expr)

        | Expr.In(left, right) -> Expr.In(trimParams left, trimParams right)

        | Expr.LessThan(left, right) -> Expr.LessThan(trimParams left, trimParams right)

        | Expr.LessThanOrEqual(left, right) -> Expr.LessThanOrEqual(trimParams left, trimParams right)

        | Expr.GreaterThan(left, right) -> Expr.GreaterThan(trimParams left, trimParams right)

        | Expr.GreaterThanOrEqual(left, right) -> Expr.GreaterThanOrEqual(trimParams left, trimParams right)

        | expr -> expr

    let parseQueryTrimmed query =
        match Parser.parse query with
        | Result.Error error -> Result.Error error
        | Result.Ok expr -> Result.Ok (trimParams expr)

    let columnParameterTable (parameterName: string) (columnName: string) =
        if columnName.Contains "." then
            let parts = columnName.Split '.'
            { parameterName = parameterName; column = parts.[1]; table = Some parts.[0] }
        else
            { parameterName = parameterName; column = columnName; table = None }

    let rec findComparisonParameters = function
        | Expr.Equals(Expr.Ident columnName, Expr.Parameter parameterName) ->
            [ columnParameterTable parameterName columnName ]

        | Expr.Equals(Expr.Parameter parameterName, Expr.Ident columnName) ->
            [ columnParameterTable parameterName columnName ]

        | Expr.LessThan(Expr.Ident columnName, Expr.Parameter parameterName) ->
            [ columnParameterTable parameterName columnName ]

        | Expr.LessThan(Expr.Parameter parameterName, Expr.Ident columnName) ->
            [ columnParameterTable parameterName columnName ]

        | Expr.LessThanOrEqual(Expr.Ident columnName, Expr.Parameter parameterName) ->
            [ columnParameterTable parameterName columnName ]

        | Expr.LessThanOrEqual(Expr.Parameter parameterName, Expr.Ident columnName) ->
            [ columnParameterTable parameterName columnName ]

        | Expr.GreaterThan(Expr.Ident columnName, Expr.Parameter parameterName) ->
            [ columnParameterTable parameterName columnName ]

        | Expr.GreaterThan(Expr.Parameter parameterName, Expr.Ident columnName) ->
            [ columnParameterTable parameterName columnName ]

        | Expr.GreaterThanOrEqual(Expr.Ident columnName, Expr.Parameter parameterName) ->
            [ columnParameterTable parameterName columnName ]

        | Expr.GreaterThanOrEqual(Expr.Parameter parameterName, Expr.Ident columnName) ->
            [ columnParameterTable parameterName columnName ]

        | Expr.Not(expression) -> findComparisonParameters expression

        | Expr.And(leftExpression, rightExpression) ->
            [
                yield! findComparisonParameters leftExpression
                yield! findComparisonParameters rightExpression
            ]

        | Expr.Or(leftExpression, rightExpression) ->
            [
                yield! findComparisonParameters leftExpression
                yield! findComparisonParameters rightExpression
            ]

        | expression -> [ ]

    let containsNonInnerJoins (query: SelectExpr) =
        query.Joins
        |> List.exists (function
            | JoinExpr.FullJoin _ -> true
            | JoinExpr.LeftJoin _ -> true
            | JoinExpr.RightJoin _ -> true
            | JoinExpr.InnerJoin _ -> false
        )


    let determineParameterNullability (parameters: Parameter list) (schema: DbSchemaLookups) query =
        match parseQueryTrimmed query with
        | Result.Error error -> parameters
        | Result.Ok (Expr.InsertQuery query) ->

            let findParameter (expr, column) =
                match expr with
                | Expr.Parameter parameterName ->
                    Some (parameterName, column)
                | _ ->
                    None

            let insertParameters =
                query.Columns
                |> List.zip query.Values
                |> List.choose findParameter

            let columnNonNullable columnName =
                schema.Columns
                |> Seq.exists (fun column -> column.Value.Name = columnName && not column.Value.Nullable)

            parameters
            |> List.map (fun requiredParameter ->
                insertParameters
                |> List.tryFind (fun (parameter, column) -> parameter = requiredParameter.Name && columnNonNullable column)
                |> function
                    | None -> requiredParameter
                    | Some _ -> { requiredParameter with IsNullable = false }
            )

        | Result.Ok (Expr.UpdateQuery query) ->
            let columnNonNullable columnName (table: Option<string>) =
                schema.Columns
                |> Seq.exists (fun column ->
                    column.Value.Name = columnName
                    && not column.Value.Nullable
                    && table = Some column.Value.BaseTableName
                )

            let comparisonParameters =
                match query.Where with
                | None -> [ ]
                | Some whereExpr ->
                    findComparisonParameters whereExpr
                    |> List.map (fun comparison ->
                        match comparison.table with
                        | None -> { comparison with table = Some query.Table }
                        | _ -> comparison
                    )

            let updateAssignments =
                query.Assignments
                |> List.choose (function
                    | Expr.Equals(Expr.Ident columnName, Expr.Parameter parameter) ->
                        Some (parameter, columnName)
                    | _ ->
                        None
                )

            parameters
            // determine nullability on comparison operators
            |> List.map (fun requiredParameter ->
                comparisonParameters
                |> List.tryFind (fun comparison ->
                    comparison.parameterName = requiredParameter.Name
                    && columnNonNullable comparison.column comparison.table)
                |> function
                    | None -> requiredParameter
                    | Some _ -> { requiredParameter with IsNullable = false }
            )
            // determine nullability on update assignments
            |> List.map (fun requiredParameter ->
                updateAssignments
                |> List.tryFind (fun (parameter, column) -> parameter = requiredParameter.Name && columnNonNullable column (Some query.Table))
                |> function
                    | None -> requiredParameter
                    | Some _ -> { requiredParameter with IsNullable = false }
            )

        | Result.Ok (Expr.DeleteQuery query) ->
            let columnNonNullable columnName (table: Option<string>) =
                schema.Columns
                |> Seq.exists (fun column ->
                    column.Value.Name = columnName
                    && not column.Value.Nullable
                    && table = Some column.Value.BaseTableName
                )

            let comparisonParameters =
                match query.Where with
                | None -> [ ]
                | Some whereExpr ->
                    findComparisonParameters whereExpr
                    |> List.map (fun comparison ->
                        match comparison.table with
                        | None -> { comparison with table = Some query.Table }
                        | _ -> comparison
                    )

            parameters
            |> List.map (fun requiredParameter ->
                comparisonParameters
                |> List.tryFind (fun comparison ->
                    comparison.parameterName = requiredParameter.Name
                    && columnNonNullable comparison.column comparison.table)
                |> function
                    | None -> requiredParameter
                    | Some _ -> { requiredParameter with IsNullable = false }
            )


        | Result.Ok (Expr.SelectQuery query) ->
            if containsNonInnerJoins query || query.From.IsNone then
                // When query includes non-INNER JOINS, things get complicated
                // Implement later...
                parameters

            else

                let columnNonNullable columnName (table: Option<string>) =
                    schema.Columns
                    |> Seq.exists (fun column ->
                        column.Value.Name = columnName
                        && not column.Value.Nullable
                        && table = Some column.Value.BaseTableName
                    )

                let comparisonParameters =
                    match query.Where with
                    | None -> [ ]
                    | Some whereExpr ->
                        findComparisonParameters whereExpr
                        |> List.map (fun comparison ->
                            match comparison.table, query.From with
                            | None, Some (Expr.Ident table) ->
                                { comparison with table = Some table }
                            | None, Some (Expr.As(Expr.Ident table, alias)) ->
                                { comparison with table = Some table }
                            | _ ->
                                comparison
                        )

                parameters
                |> List.map (fun requiredParameter ->
                    comparisonParameters
                    |> List.tryFind (fun comparison ->
                        comparison.parameterName = requiredParameter.Name
                        && columnNonNullable comparison.column comparison.table)
                    |> function
                        | None -> requiredParameter
                        | Some _ -> { requiredParameter with IsNullable = false }
                )

        | Result.Ok expr ->
            parameters

    let missingInsertColumns (schema: DbSchemaLookups) (query:string) =
        match Parser.parse query with
        | Result.Error _ -> None
        | Result.Ok expression ->
            match expression with
            | Expr.InsertQuery insertQuery ->
                let missingInsertColumns =
                    schema.Columns.Values
                    |> Seq.filter (fun column -> column.BaseTableName = insertQuery.Table && not column.OptionalForInsert)
                    |> Seq.filter (fun column -> not (List.contains column.Name insertQuery.Columns))

                if Seq.isEmpty missingInsertColumns then
                    None
                else
                    let errorMessage =
                        missingInsertColumns
                        |> Seq.map (fun column -> sprintf "'%s'" column.Name)
                        |> Seq.toList
                        |> function
                             | [ ] -> "<empty>"
                             | [ first ] -> first
                             | [ first; second ] -> sprintf "%s and %s" first second
                             | columns ->
                                 let lastColumn = List.last columns
                                 let firstColumns =
                                     columns
                                     |> List.rev
                                     |> List.skip 1
                                     |> List.rev
                                     |> String.concat ", "

                                 sprintf "%s and %s" firstColumns lastColumn

                        |> fun columns -> sprintf "INSERT query is missing required columns %s when adding rows to the '%s' table" columns insertQuery.Table

                    Some errorMessage
            | _ ->
                None

    let extractParametersAndOutputColumns(connectionString, commandText, dbSchemaLookups) =
        try
            let parameters, output, enums = InformationSchema.extractParametersAndOutputColumns(connectionString, commandText, false, dbSchemaLookups)
            let parametersWithNullability = determineParameterNullability parameters dbSchemaLookups commandText
            let potentiallyMissingColumns = missingInsertColumns dbSchemaLookups commandText
            Result.Ok (parametersWithNullability, output, potentiallyMissingColumns)
        with
        | :? PostgresException as databaseError ->
            // errors such as syntax errors are reported here
            Result.Error (sprintf "%s: %s" databaseError.Severity databaseError.MessageText)
        | error ->
            // any other generic error
            Result.Error (sprintf "%s\n%s" error.Message error.StackTrace)

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

                            let warning =
                                if String.IsNullOrWhiteSpace(providedParam.name)
                                then createWarning (sprintf "Empty parameter name was provided. Please provide one of %s" expectedParameters) providedParam.range
                                else if List.length codeFixes = 1
                                then createWarning (sprintf "Unexpected parameter '%s' is provided. Did you mean '%s'?" providedParam.name closestAlternative) providedParam.range
                                else createWarning (sprintf "Unexpected parameter '%s' is provided. Did you mean '%s'? %s" providedParam.name closestAlternative expectedParameters) providedParam.range

                            yield { warning with Fixes = codeFixes }
                        else
                            // do parameter type-checking
                            let matchingColumnType = requiredParameters |> List.tryFind(fun p -> p.Name = providedParam.name)
                            match matchingColumnType with
                            | None -> ()
                            | Some requiredParam ->
                                let typeMismatch (shouldUse: string list) =
                                    let formattedSuggestions =
                                        match shouldUse with
                                        | [ ] -> "<empty>"
                                        | [ first ] -> first
                                        | [ first; second ] -> sprintf "%s or %s" first second
                                        | _ ->
                                            let lastSuggestion = List.last shouldUse
                                            let firstSuggestions =
                                                shouldUse
                                                |> List.rev
                                                |> List.skip 1
                                                |> List.rev
                                                |> String.concat ", "

                                            sprintf "%s or %s" firstSuggestions lastSuggestion

                                    let warning =
                                      sprintf "Attempting to provide parameter '%s' of type '%s' using function %s. Please use %s instead."
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

                                if providedParam.paramFunc = "Sql.parameter" then
                                    // do not do anything when the input is a generic param
                                    ()
                                else if not requiredParam.DataType.IsArray then 
                                    match requiredParam.DataType.Name with
                                    | "bit" ->
                                        if requiredParam.IsNullable then 
                                            if providedParam.paramFunc <> "Sql.bit" &&  providedParam.paramFunc <> "Sql.bitOrNone" && providedParam.paramFunc <> "Sql.dbnull"
                                            then yield typeMismatch [ "Sql.bit"; "Sql.bitOrNone";"Sql.dbnull"]
                                        else
                                            if providedParam.paramFunc <> "Sql.bit"
                                            then yield typeMismatch [ "Sql.bit" ]

                                    | ("bool" | "boolean") ->
                                        if requiredParam.IsNullable then 
                                            if providedParam.paramFunc <> "Sql.bool" &&  providedParam.paramFunc <> "Sql.boolOrNone" && providedParam.paramFunc <> "Sql.dbnull"
                                            then yield typeMismatch [ "Sql.bool"; "Sql.boolOrNone"; "Sql.dbnull" ]
                                        else
                                            if providedParam.paramFunc <> "Sql.bool"
                                            then yield typeMismatch [ "Sql.bool" ]

                                    | ("int" | "int32" | "integer" | "serial") ->
                                        if requiredParam.IsNullable then
                                            if providedParam.paramFunc <> "Sql.int" &&  providedParam.paramFunc <> "Sql.intOrNone" && providedParam.paramFunc <> "Sql.dbnull"
                                            then yield typeMismatch [ "Sql.int"; "Sql.intOrNone"; "Sql.dbnull" ]
                                        else
                                            if providedParam.paramFunc <> "Sql.int"
                                            then yield typeMismatch [ "Sql.int" ]

                                    | ("smallint" | "int16") ->
                                        if requiredParam.IsNullable then
                                            if providedParam.paramFunc <> "Sql.int16" &&  providedParam.paramFunc <> "Sql.int16OrNone" && providedParam.paramFunc <> "Sql.dbnull"
                                            then yield typeMismatch [ "Sql.int16"; "Sql.int16OrNone"; "Sql.dbnull" ]
                                        else
                                            if providedParam.paramFunc <> "Sql.int16"
                                            then yield typeMismatch [ "Sql.int16" ]

                                    | ("int64" | "bigint" |"bigserial") ->
                                        if requiredParam.IsNullable then 
                                            if providedParam.paramFunc <> "Sql.int64" && providedParam.paramFunc <> "Sql.int" &&  providedParam.paramFunc <> "Sql.int64OrNone" && providedParam.paramFunc <> "Sql.dbnull"
                                            then yield typeMismatch [ "Sql.int64"; "Sql.int"; "Sql.int64OrNone"; "Sql.dbnull" ]
                                        else
                                            if providedParam.paramFunc <> "Sql.int64" && providedParam.paramFunc <> "Sql.int"
                                            then yield typeMismatch [ "Sql.int64"; "Sql.int" ]
                                    
                                    | ("numeric" | "decimal" | "money") ->
                                        if requiredParam.IsNullable then
                                            if providedParam.paramFunc <> "Sql.decimal" &&  providedParam.paramFunc <> "Sql.decimalOrNone" && providedParam.paramFunc <> "Sql.dbnull"
                                            then yield typeMismatch [ "Sql.decimal"; "Sql.decimalOrNone"; "Sql.dbnull" ]
                                        else
                                            if providedParam.paramFunc <> "Sql.decimal"
                                            then yield typeMismatch [ "Sql.decimal" ]

                                    | "double precision" ->
                                        if requiredParam.IsNullable then
                                            if providedParam.paramFunc <> "Sql.double" &&  providedParam.paramFunc <> "Sql.doubleOrNone" && providedParam.paramFunc <> "Sql.dbnull"
                                            then yield typeMismatch [ "Sql.double"; "Sql.doubleOrNone"; "Sql.dbnull" ]
                                        else
                                            if providedParam.paramFunc <> "Sql.double"
                                            then yield typeMismatch [ "Sql.double" ]

                                    | "bytea" ->
                                        if requiredParam.IsNullable then
                                            if providedParam.paramFunc <> "Sql.bytea" &&  providedParam.paramFunc <> "Sql.byteaOrNone" && providedParam.paramFunc <> "Sql.dbnull"
                                            then yield typeMismatch [ "Sql.bytea"; "Sql.byteaOrNone"; "Sql.dbnull" ]
                                        else
                                            if providedParam.paramFunc <> "Sql.bytea"
                                            then yield typeMismatch [ "Sql.bytea" ]

                                    | "uuid" ->
                                        if requiredParam.IsNullable then 
                                            if providedParam.paramFunc <> "Sql.uuid" &&  providedParam.paramFunc <> "Sql.uuidOrNone" && providedParam.paramFunc <> "Sql.dbnull"
                                            then yield typeMismatch [ "Sql.uuid"; "Sql.uuidOrNone"; "Sql.dbnull" ]
                                        else
                                            if providedParam.paramFunc <> "Sql.uuid"
                                            then yield typeMismatch [ "Sql.uuid" ]

                                    | "date" ->
                                        if requiredParam.IsNullable then
                                            if providedParam.paramFunc <> "Sql.date" &&  providedParam.paramFunc <> "Sql.dateOrNone" && providedParam.paramFunc <> "Sql.dbnull"
                                            then yield typeMismatch [ "Sql.date"; "Sql.dateOrNone"; "Sql.dbnull" ]
                                        else
                                            if providedParam.paramFunc <> "Sql.date"
                                            then yield typeMismatch [ "Sql.date" ]

                                    | ("timestamp"|"timestamp without time zone") ->
                                        if requiredParam.IsNullable then
                                            if providedParam.paramFunc <> "Sql.timestamp" &&  providedParam.paramFunc <> "Sql.timestampOrNone" && providedParam.paramFunc <> "Sql.dbnull"
                                            then yield typeMismatch [ "Sql.timestamp"; "Sql.timestampOrNone"; "Sql.dbnull" ]
                                        else
                                            if providedParam.paramFunc <> "Sql.timestamp"
                                            then yield typeMismatch [ "Sql.timestamp" ]

                                    | ("timestamptz" | "timestamp with time zone") ->
                                        if requiredParam.IsNullable then
                                            if providedParam.paramFunc <> "Sql.timestamptz" &&  providedParam.paramFunc <> "Sql.timestamptzOrNone" && providedParam.paramFunc <> "Sql.dbnull"
                                            then yield typeMismatch [ "Sql.timestamptz"; "Sql.timestamptzOrNone"; "Sql.dbnull" ]
                                        else
                                            if providedParam.paramFunc <> "Sql.timestamptz"
                                            then yield typeMismatch [ "Sql.timestamptz" ]

                                    | "jsonb" ->
                                        if requiredParam.IsNullable then
                                            if providedParam.paramFunc <> "Sql.jsonb" && providedParam.paramFunc <> "Sql.jsonbOrNone" && providedParam.paramFunc <> "Sql.dbnull"
                                            then yield typeMismatch [ "Sql.jsonb"; "Sql.jsonbOrNone"; "Sql.dbnull" ]
                                        else
                                            if providedParam.paramFunc <> "Sql.jsonb"
                                            then yield typeMismatch [ "Sql.jsonb" ]

                                    | ("text"|"json"|"xml") ->
                                        if requiredParam.IsNullable then 
                                            if providedParam.paramFunc <> "Sql.text" && providedParam.paramFunc <> "Sql.textOrNone" && providedParam.paramFunc <> "Sql.string" && providedParam.paramFunc <> "Sql.textOrNone" && providedParam.paramFunc <> "Sql.dbnull"
                                            then yield typeMismatch [ "Sql.text"; "Sql.string"; "Sql.textOrNone"; "Sql.stringOrNone"; "Sql.dbnull" ]
                                        else
                                            if providedParam.paramFunc <> "Sql.text" && providedParam.paramFunc <> "Sql.string"
                                            then yield typeMismatch [ "Sql.text"; "Sql.string" ]
                                    | _ ->
                                        ()
                                else
                                    // data type is array
                                    match requiredParam.DataType.Name.Replace("[]", "") with
                                    | ("int" | "int32" | "integer" | "serial" | "int64" | "bigint" |"bigserial") ->
                                        if requiredParam.IsNullable then
                                            if providedParam.paramFunc <> "Sql.intArray" &&  providedParam.paramFunc <> "Sql.intArrayOrNone" && providedParam.paramFunc <> "Sql.dbnull"
                                            then yield typeMismatch [ "Sql.intArray"; "Sql.intArrayOrNone"; "Sql.dbnull" ]
                                        else
                                            if providedParam.paramFunc <> "Sql.intArray"
                                            then yield typeMismatch [ "Sql.intArray" ]

                                    | "uuid" ->
                                        if requiredParam.IsNullable then
                                            if providedParam.paramFunc <> "Sql.uuidArray" &&  providedParam.paramFunc <> "Sql.uuidArrayOrNone" && providedParam.paramFunc <> "Sql.dbnull"
                                            then yield typeMismatch [ "Sql.uuidArray"; "Sql.uuidArrayOrNone"; "Sql.dbnull" ]
                                        else
                                            if providedParam.paramFunc <> "Sql.uuidArray"
                                            then yield typeMismatch [ "Sql.uuidArray" ]

                                    | ("text"|"json"|"xml") ->
                                        if requiredParam.IsNullable then 
                                            if providedParam.paramFunc <> "Sql.stringArray" &&  providedParam.paramFunc <> "Sql.stringArrayOrNone" && providedParam.paramFunc <> "Sql.dbnull"
                                            then yield typeMismatch [ "Sql.stringArray"; "Sql.stringArrayOrNone"; "Sql.dbnull" ]
                                        else
                                            if providedParam.paramFunc <> "Sql.stringArray"
                                            then yield typeMismatch [ "Sql.stringArray" ]
                                    | _ ->
                                        ()
                ]

    let findColumn (name: string) (availableColumns: InformationSchema.Column list) =
        availableColumns
        |> List.tryFind (fun column -> column.Name = name)

    let formatColumns (availableColumns: InformationSchema.Column list) =
        availableColumns
        |> List.map (fun column ->
            if column.DataType.IsArray 
            then sprintf "| -- %s of type %s[]" column.Name column.DataType.Name
            else sprintf "| -- %s of type %s" column.Name column.DataType.Name
        )
        |> String.concat "\n"

    let analyzeColumnReadingAttempts (columnReadAttempts: ColumnReadAttempt list) (availableColumns: InformationSchema.Column list) =
        [
            for attempt in columnReadAttempts do
                match findColumn attempt.columnName availableColumns with
                | None ->
                    if List.isEmpty availableColumns then
                        let warningMsg = sprintf "Attempting to read column named '%s' from a result set which doesn't return any columns. In case you are executing DELETE, INSERT or UPDATE queries, you might want to use Sql.executeNonQuery or Sql.executeNonQueryAsync to obtain the number of affected rows. You can also add a RETURNING clause in your query to make it return the rows which are updated, inserted or deleted from that query." attempt.columnName
                        yield createWarning warningMsg attempt.columnNameRange
                    else
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
                            match shouldUse with
                            | [ ] -> "<empty>"
                            | [ first ] -> first
                            | [ first; second ] -> sprintf "%s or %s" first second
                            | _ ->
                                let lastSuggestion = List.last shouldUse
                                let firstSuggestions =
                                    shouldUse
                                    |> List.rev
                                    |> List.skip 1
                                    |> List.rev
                                    |> String.concat ", "

                                sprintf "%s or %s" firstSuggestions lastSuggestion

                        String.concat String.Empty [
                            sprintf "Type mismatch: attempting to read column '%s' of " column.Name
                            if column.Nullable then "nullable " else "non-nullable "
                            if column.DataType.IsArray then "array " else ""
                            sprintf "type '%s' using %s. " column.DataType.Name attempt.funcName
                            sprintf "Please use %s instead" formattedFunctions
                        ]

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

                        if not column.DataType.IsArray then 
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
                                else if notUsing "byteaOrNone" && notUsing "bytea"
                                then
                                    if column.Nullable
                                    then yield typeMismatch [ replace "byteaOrNone" ]
                                    else yield typeMismatch [ replace "bytea" ]
                                else ()
                            | _ ->
                                ()
                        else
                            // type is an array
                            match column.DataType.Name with
                            | ("text"|"json"|"xml"|"jsonb"|"varchar") ->
                                if column.Nullable && notUsing "stringArrayOrNone"
                                then yield typeMismatch [ replace "stringArrayOrNone" ]
                                elif notUsing "stringArrayOrNone" && notUsing "stringArray"
                                then
                                    if column.Nullable
                                    then yield typeMismatch [ replace "stringArrayOrNone" ]
                                    else yield typeMismatch [ replace "stringArray" ]

                            | "uuid" ->
                                if column.Nullable && notUsing "uuidArrayOrNone"
                                then yield typeMismatch [ replace "uuidArrayOrNone" ]
                                elif notUsing "uuidArrayOrNone" && notUsing "uuidArray"
                                then
                                    if column.Nullable
                                    then yield typeMismatch [ replace "uuidArrayOrNone" ]
                                    else yield typeMismatch [ replace "uuidArray" ]

                            | ("int"|"integer"|"int32"|"serial"|"int16"|"int8"|"int4"|"int2") ->
                                if column.Nullable && notUsing "intArrayOrNone"
                                then yield typeMismatch [ replace "intArrayOrNone" ]
                                elif notUsing "intArrayOrNone" && notUsing "intArray"
                                then
                                    if column.Nullable
                                    then yield typeMismatch [ replace "intArrayOrNone" ]
                                    else yield typeMismatch [ replace "intArray" ]
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
            | Result.Ok (parameters, outputColunms, errorMessage) ->
                let potentialInsertQueryError =
                    match errorMessage with
                    | None -> [ ]
                    | Some message -> [ createWarning message queryRange ]

                let readingAttempts = defaultArg (findColumnReadAttempts operation) [ ]
                [
                    yield! potentialInsertQueryError
                    yield! analyzeParameters operation parameters
                    yield! analyzeColumnReadingAttempts readingAttempts outputColunms
                ]
