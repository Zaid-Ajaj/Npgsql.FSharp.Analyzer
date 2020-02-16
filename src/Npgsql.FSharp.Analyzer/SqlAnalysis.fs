namespace Npgsql.FSharp.Analyzers

open FSharp.Analyzers.SDK
open FSharp.Compiler.Range

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

    let analyzeParameters (operation: SqlOperation) (requiredParameters: InformationSchema.Parameter list) =
        match findParameters operation with
        | None ->
            let missingParameters =
                requiredParameters
                |> List.map (fun p -> sprintf "%s:%s" p.Name p.DataType.Name)
                |> String.concat ", "
                |> sprintf "Missing parameters [%s]. Please use Sql.parameters to provide them."
            [ createWarning missingParameters operation.range ]
        | Some (queryParams, queryParamsRange) ->
            let missingParameters = [
                for requiredParameter in requiredParameters do
                    if not (queryParams |> List.exists (fun p -> p.parameter.TrimStart('@') = requiredParameter.Name))
                    then
                        let message = sprintf "Missing parameter '%s' of type %s" requiredParameter.Name requiredParameter.DataType.Name
                        yield createWarning message queryParamsRange

                for providedParam in queryParams do
                    if not (requiredParameters |> List.exists (fun p -> p.Name = providedParam.parameter.TrimStart('@')))
                    then
                        let expectedParameters =
                            requiredParameters
                            |> List.map (fun p -> sprintf "%s:%s" p.Name p.DataType.Name)
                            |> String.concat ", "
                            |> sprintf "Required parameters are [%s]."
                        yield createWarning (sprintf "Unexpected parameter %s is provided. %s" providedParam.parameter expectedParameters) providedParam.range
            ]

            missingParameters
        
    let analyzeBlock (operation: SqlOperation) (connectionString: string) : Message list =
        match findQuery operation with
        | None ->
            [ createWarning "Missing query in this block of code. Please use Sql.query to provide a query" operation.range ]
        | Some (query, queryRange) ->
            try
                let databaseSchema = InformationSchema.getDbSchemaLookups(connectionString)
                let queryAnalysis = extractParametersAndOutputColumns(connectionString, query, databaseSchema)
                match queryAnalysis with
                | Result.Error queryError ->
                    [ createWarning queryError queryRange ]
                | Result.Ok (parameters, outputColums) ->
                    let parameterErrors = analyzeParameters operation parameters
                    parameterErrors
            with ex ->
                [ createWarning ex.Message queryRange ]
