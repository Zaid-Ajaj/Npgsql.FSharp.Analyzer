namespace Npgsql.FSharp.Analyzers

open FSharp.Analyzers.SDK
open System

module SqlAnalyzer =
    [<Analyzer>]
    let queryAnalyzer : Analyzer =
        fun (ctx: Context) ->
            let syntacticBlocks = SyntacticAnalysis.findSqlBlocks ctx
            let connectionString = Environment.GetEnvironmentVariable "NPGSQL_FSHARP"
            if isNull connectionString || String.IsNullOrWhiteSpace connectionString then
                [
                    for block in syntacticBlocks ->
                        SqlAnalysis.createWarning "Missing environment variable 'NPGSQL_FSHARP'. Please set that variable to the connection string of your development database" block.range
                ]
            else
                syntacticBlocks
                |> List.collect (fun block -> SqlAnalysis.analyzeBlock block connectionString)
                |> List.distinctBy (fun message -> message.Range)
