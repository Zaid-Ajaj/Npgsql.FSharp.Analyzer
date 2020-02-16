namespace Npgsql.FSharp.Analyzers

open FSharp.Analyzers.SDK

module SqlAnalyzer =
    [<Analyzer>]
    let queryAnalyzer : Analyzer =
        fun ctx ->
            let sqlBlocks = SyntacticAnalysis.findSqlBlocks ctx
            let messages = [ ]
            messages
