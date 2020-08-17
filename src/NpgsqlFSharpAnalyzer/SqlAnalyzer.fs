namespace Npgsql.FSharp.Analyzers

open FSharp.Analyzers.SDK
open System
open System.IO
open System.Linq

module SqlAnalyzer =

    let specializedSeverity = function
    | Core.Severity.Error -> Severity.Error
    | Core.Severity.Info -> Severity.Info
    | Core.Severity.Warning -> Severity.Warning

    let specializedFix (fix: Core.Fix) : Fix =
        {
            FromRange = fix.FromRange
            FromText = fix.FromText
            ToText = fix.ToText
        }

    let specializedMessage (message: Core.Message) : Message =
        {
            Code = message.Code
            Fixes = message.Fixes |> List.map specializedFix
            Message = message.Message
            Range = message.Range
            Severity = message.Severity |> specializedSeverity
            Type = message.Type
        }

    let specializedContext (ctx: Context) : Core.SpecializedContext = {
        Content = ctx.Content
        FileName = ctx.FileName
        GetAllEntities = ctx.GetAllEntities
        ParseTree = ctx.ParseTree
        Symbols = ctx.Symbols
        TypedTree = ctx.TypedTree
    }

    [<Analyzer "Npgsql.FSharp.Analyzer">]
    let queryAnalyzer : Analyzer =
        fun (ctx: Context) ->
            let syntacticBlocks = Core.SyntacticAnalysis.findSqlOperations (specializedContext ctx)
            if List.isEmpty syntacticBlocks then
                [ ]
            else
                let connectionString = Core.SqlAnalyzer.tryFindConnectionString ctx.FileName
                if isNull connectionString || String.IsNullOrWhiteSpace connectionString then
                    [
                        for block in syntacticBlocks ->
                            Core.SqlAnalysis.createWarning "Missing environment variable 'NPGSQL_FSHARP'. Please set that variable to the connection string of your development database put the connection string in a file called 'NPGSQL_FSHARP' relative next your project or in your project root." block.range
                            |> specializedMessage
                    ]
                else
                    match Core.SqlAnalysis.databaseSchema connectionString with
                    | Result.Error connectionError ->
                        [
                            for block in syntacticBlocks ->
                                Core.SqlAnalysis.createWarning (sprintf "Error while connecting to the development database using the connection string from environment variable 'NPGSQL_FSHARP' or put the connection string in a file called 'NPGSQL_FSHARP' relative next your project or in your project root. Connection error: %s" connectionError) block.range
                                |> specializedMessage
                        ]

                    | Result.Ok schema ->
                        syntacticBlocks
                        |> List.collect (fun block -> Core.SqlAnalysis.analyzeOperation block connectionString schema)
                        |> List.map specializedMessage
                        |> List.distinctBy (fun message -> message.Range)
