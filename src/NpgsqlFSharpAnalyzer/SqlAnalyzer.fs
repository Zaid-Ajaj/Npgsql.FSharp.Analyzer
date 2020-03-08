namespace Npgsql.FSharp.Analyzers

open FSharp.Analyzers.SDK
open System

module SqlAnalyzer =
    [<Analyzer "Npgsql.FSharp.Analyzer">]
    let queryAnalyzer : Analyzer =
        fun (ctx: Context) ->
            let syntacticBlocks = SyntacticAnalysis.findSqlOperations ctx
            let connectionString = Environment.GetEnvironmentVariable "NPGSQL_FSHARP"
            if isNull connectionString || String.IsNullOrWhiteSpace connectionString then
                [
                    for block in syntacticBlocks ->
                        SqlAnalysis.createWarning "Missing environment variable 'NPGSQL_FSHARP'. Please set that variable to the connection string of your development database" block.range
                ]
            else
                match SqlAnalysis.databaseSchema connectionString with
                | Result.Error connectionError ->
                    [
                        for block in syntacticBlocks ->
                            SqlAnalysis.createWarning (sprintf "Error while connecting to the development database using the connection string from environment variable 'NPGSQL_FSHARP'. Connection error: %s" connectionError) block.range
                    ]

                | Result.Ok schema ->
                    syntacticBlocks
                    |> List.collect (fun block -> SqlAnalysis.analyzeOperation block connectionString schema)
                    |> List.distinctBy (fun message -> message.Range)
