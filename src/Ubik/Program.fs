open System
open System.IO
open Npgsql.FSharp.Analyzers.Core
open Spectre.Console
open System.Xml

let resolveFile (path: string) =
    if Path.IsPathRooted path
    then path
    else Path.GetFullPath (Path.Combine(Environment.CurrentDirectory, path))

let getProject (args: string []) =
    try
        match args with
        | [| |] ->
            Directory.GetFiles(Environment.CurrentDirectory, "*.fsproj")
            |> Array.tryHead
            |> Option.map (fun projectPath -> resolveFile projectPath)

        | multipleArgs ->
            let firstArg = multipleArgs.[0]
            if firstArg.EndsWith(".fsproj") then
                Some (resolveFile firstArg)
            else
                Directory.GetFiles(resolveFile firstArg, "*.fsproj")
                |> Array.tryHead
                |> Option.map (fun projectPath -> resolveFile projectPath)
    with
    | error -> None

[<EntryPoint>]
let main argv =
    match getProject argv with
    | None ->
        printfn "No project file found in the current directory"
        1

    | Some project ->
        AnsiConsole.MarkupLine("Analyzing [blue]{0}[/]", project)

        let document = XmlDocument()
        document.LoadXml(File.ReadAllText project)

        let fsharpFileNodes = document.GetElementsByTagName("Compile")
        let fsharpFiles = [
            for item in 0 .. fsharpFileNodes.Count - 1 ->
                let relativePath = fsharpFileNodes.[item].Attributes.["Include"].InnerText
                let projectParent = Directory.GetParent project
                Path.Combine(projectParent.FullName, relativePath)
        ]

        for file in fsharpFiles do
            AnsiConsole.MarkupLine("Analyzing file [green]{0}[/]", file)
            match Project.context file with
            | None -> ()
            | Some context -> 
                let syntacticBlocks = SyntacticAnalysis.findSqlOperations context
                if not syntacticBlocks.IsEmpty then
                    let messages = 
                        let connectionString = SqlAnalyzer.tryFindConnectionString context.FileName
                        if isNull connectionString || String.IsNullOrWhiteSpace connectionString then
                            [ ]
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

                    for message in messages do
                        AnsiConsole.MarkupLine("Error [red]{0}[/]", message.Message)

        0
