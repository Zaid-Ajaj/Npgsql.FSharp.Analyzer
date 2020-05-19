namespace Npgsql.FSharp.Analyzers

open FSharp.Analyzers.SDK
open System
open System.IO
open System.Linq

module SqlAnalyzer =

    /// Recursively tries to find the parent of a file starting from a directory
    let rec findParent (directory: string) (fileToFind: string) = 
        let path = if Directory.Exists(directory) then directory else Directory.GetParent(directory).FullName
        let files = Directory.GetFiles(path)
        if files.Any(fun file -> Path.GetFileName(file).ToLower() = fileToFind.ToLower()) 
        then path 
        else findParent (DirectoryInfo(path).Parent.FullName) fileToFind

    let tryFindConfig (file: string) =
        try
            let parent = (Directory.GetParent file).FullName
            try Some (Path.Combine(findParent parent "NPGSQL_FSHARP", "NPGSQL_FSHARP"))
            with error -> None 
        with error ->
            None

    let tryFindConnectionString (ctx: Context) =
        match tryFindConfig ctx.FileName with
        | Some config ->
            try (File.ReadAllText config)
            with error -> Environment.GetEnvironmentVariable "NPGSQL_FSHARP"
        | None ->
            Environment.GetEnvironmentVariable "NPGSQL_FSHARP"

    [<Analyzer "Npgsql.FSharp.Analyzer">]
    let queryAnalyzer : Analyzer =
        fun (ctx: Context) ->
            let syntacticBlocks = SyntacticAnalysis.findSqlOperations ctx
            if List.isEmpty syntacticBlocks then
                [ ]
            else 
                let connectionString = tryFindConnectionString ctx
                if isNull connectionString || String.IsNullOrWhiteSpace connectionString then
                    [
                        for block in syntacticBlocks ->
                            SqlAnalysis.createWarning "Missing environment variable 'NPGSQL_FSHARP'. Please set that variable to the connection string of your development database put the connection string in a file called 'NPGSQL_FSHARP' relative next your project or in your project root." block.range
                    ]
                else
                    match SqlAnalysis.databaseSchema connectionString with
                    | Result.Error connectionError ->
                        [
                            for block in syntacticBlocks ->
                                SqlAnalysis.createWarning (sprintf "Error while connecting to the development database using the connection string from environment variable 'NPGSQL_FSHARP' or put the connection string in a file called 'NPGSQL_FSHARP' relative next your project or in your project root. Connection error: %s" connectionError) block.range
                        ]

                    | Result.Ok schema ->
                        syntacticBlocks
                        |> List.collect (fun block -> SqlAnalysis.analyzeOperation block connectionString schema)
                        |> List.distinctBy (fun message -> message.Range)
