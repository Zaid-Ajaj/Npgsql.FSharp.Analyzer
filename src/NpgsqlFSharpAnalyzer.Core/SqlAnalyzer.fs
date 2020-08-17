namespace Npgsql.FSharp.Analyzers.Core

open System
open System.IO
open System.Linq
open FSharp.Compiler.SourceCodeServices

module SqlAnalyzer =

    /// Recursively tries to find the parent of a file starting from a directory
    let rec findParent (directory: string) (fileToFind: string) =
        let path = if Directory.Exists(directory) then directory else Directory.GetParent(directory).FullName
        let files = Directory.GetFiles(path)
        if files.Any(fun file -> Path.GetFileName(file).ToLower() = fileToFind.ToLower())
        then path
        else findParent (DirectoryInfo(path).Parent.FullName) fileToFind

    let getSymbols (checkResults: FSharpCheckFileResults) =
        checkResults.PartialAssemblySignature.Entities
        |> Seq.toList

    let checkAnswerResult checkFileAnswer =
        match checkFileAnswer with
        | FSharpCheckFileAnswer.Aborted -> None
        | FSharpCheckFileAnswer.Succeeded results -> Some results

    let tryFindConfig (file: string) =
        try
            let parent = (Directory.GetParent file).FullName
            try Some (Path.Combine(findParent parent "NPGSQL_FSHARP", "NPGSQL_FSHARP"))
            with error -> None
        with error ->
            None

    let tryFindConnectionString fileName =
        match tryFindConfig fileName with
        | Some config ->
            try (File.ReadAllText config)
            with error -> Environment.GetEnvironmentVariable "NPGSQL_FSHARP"
        | None ->
            Environment.GetEnvironmentVariable "NPGSQL_FSHARP"
