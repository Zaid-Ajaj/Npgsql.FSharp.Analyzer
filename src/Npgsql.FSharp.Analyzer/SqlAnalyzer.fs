namespace Npgsql.FSharp.Analyzers

open FSharp.Analyzers.SDK
open System

module SqlAnalyzer =
    [<Analyzer>]
    let queryAnalyzer : Analyzer =
        fun ctx ->
            
            let syntacticBlocks = SyntacticAnalysis.findSqlBlocks ctx
            let messages = [  ]
            messages
