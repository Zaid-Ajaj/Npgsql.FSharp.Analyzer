open System
open System.IO
open System.Linq
open Npgsql.FSharp.Analyzers.Core
open Spectre.Console
open System.Xml
open FSharp.Compiler.Text
open FSharp.Data.LiteralProviders

let resolveFile (path: string) =
    if Path.IsPathRooted path
    then path
    else Path.GetFullPath (Path.Combine(Environment.CurrentDirectory, path))

type FilePath =
    | File of path: string
    | InvalidFile of fileName: string

type CliArgs =
    | Version
    | InvalidArgs of error:string
    | Project of projectPath:string
    | Files of fsharpFiles: FilePath[]

let getProject (args: string []) =
    try
        match args with
        | [| |] ->
            Directory.GetFiles(Environment.CurrentDirectory, "*.fsproj")
            |> Array.tryHead
            |> Option.map (fun projectPath -> Project(resolveFile projectPath))
            |> function
                | Some project -> project
                | None ->
                    Directory.GetFiles(Environment.CurrentDirectory, "*.fs")
                    |> Array.map File
                    |> Files

        | [| "--version" |] -> Version

        | multipleArgs ->
            if multipleArgs.Length = 1 && multipleArgs.[0].EndsWith ".fsproj" then
                Project (resolveFile multipleArgs.[0])
            else if Directory.Exists(resolveFile multipleArgs.[0]) then
                Directory.GetFiles(resolveFile multipleArgs.[0], "*.fsproj")
                |> Array.tryHead
                |> Option.map (fun projectPath -> Project(resolveFile projectPath))
                |> function
                    | Some project -> project
                    | None ->
                        Directory.GetFiles(Environment.CurrentDirectory, "*.fs")
                        |> Array.map File
                        |> Files
            else 
                multipleArgs
                |> Array.filter (fun file -> file.EndsWith ".fs")
                |> Array.map (fun file -> try File (resolveFile file) with _ -> InvalidFile file)
                |> Files

    with
    | error -> InvalidArgs error.Message

let analyzeFiles (fsharpFiles: FilePath[]) =
    let errorCount = ResizeArray()
    for fsharpFile in fsharpFiles do
        match fsharpFile with
        | InvalidFile nonExistingFile ->
            AnsiConsole.MarkupLine("Analyzing file did not exist [red]{0}[/]", nonExistingFile)
            errorCount.Add(1)
        | File fsharpFile -> 
            AnsiConsole.MarkupLine("Analyzing file [green]{0}[/]", fsharpFile)
            match Project.context fsharpFile with
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

                        let range = message.Range
                        let source = SourceText.ofString(File.ReadAllText fsharpFile)
                        if range.StartLine = range.EndLine then
                            let marker =
                                source.GetLineString(range.StartLine - 1)
                                |> Seq.mapi (fun index token ->
                                    if index >= range.StartColumn && index < range.EndColumn
                                    then "[orange1]^[/]"
                                    else " "
                                )
                                |> String.concat ""

                            let original =
                                source.GetLineString(range.StartLine - 1)
                                |> Seq.mapi (fun index token ->
                                    if index >= range.StartColumn && index < range.EndColumn
                                    then "[orange1]" + token.ToString().EscapeMarkup() + "[/]"
                                    else token.ToString().EscapeMarkup()
                                )
                                |> String.concat ""

                            let before = (range.StartLine - 1).ToString()
                            let current = range.StartLine.ToString()
                            let after = (range.StartLine + 1).ToString()

                            let maxLength = List.max [ before.Length; current.Length; after.Length ]

                            AnsiConsole.MarkupLine("   [blue]{0} |[/]", before.PadLeft(maxLength, ' '))
                            AnsiConsole.MarkupLine("   [blue]{0} |[/] {1}", current.PadLeft(maxLength, ' '), original)
                            AnsiConsole.MarkupLine("   [blue]{0} |[/] {1}", after.PadLeft(maxLength, ' '), marker)
                            AnsiConsole.MarkupLine("[orange1]{0}[/]", message.Message)
                            errorCount.Add(1)
                        else
                            let lines = [range.StartLine-1 .. range.EndLine+1]
                            let maxLength =
                                lines
                                |> List.map (fun line -> line.ToString().Length)
                                |> List.max

                            for line in lines do
                                if line = range.StartLine - 1 || line = range.EndLine + 1
                                then AnsiConsole.MarkupLine("   [blue]{0} |[/] ", line.ToString().PadLeft(maxLength, ' '))
                                else AnsiConsole.MarkupLine("   [blue]{0} |[/] {1}", line.ToString().PadLeft(maxLength, ' '), source.GetLineString(line - 1).EscapeMarkup())
                             
                            AnsiConsole.MarkupLine("[orange1]{0}[/]", message.Message)
                            errorCount.Add(1)
    let exitCode = errorCount.Sum()
    Console.WriteLine()
    if exitCode = 0
    then AnsiConsole.MarkupLine("[green]No errors found[/]")
    elif exitCode = 1
    then AnsiConsole.MarkupLine("[orange1]Found 1 error[/]", exitCode)
    else AnsiConsole.MarkupLine("[orange1]Found {0} errors[/]", exitCode)
    exitCode

let [<Literal>] projectFile = TextFile<"./Ubik.fsproj">.Text

let projectVersion =
    let doc = XmlDocument()
    use content = new MemoryStream(Text.Encoding.UTF8.GetBytes projectFile)
    doc.Load(content)
    doc.GetElementsByTagName("Version").[0].InnerText

[<EntryPoint>]
let main argv =
    match getProject argv with
    | InvalidArgs error ->
        AnsiConsole.MarkupLine("[red]{0}: {1}[/]", "Error occured while reading CLI arguments: ", error)
        1

    | Version ->
        printfn "%s" projectVersion
        0

    | Files files -> analyzeFiles files

    | Project project ->
        AnsiConsole.MarkupLine("Analyzing project [blue]{0}[/]", project)

        let document = XmlDocument()
        document.LoadXml(File.ReadAllText project)

        let fsharpFileNodes = document.GetElementsByTagName("Compile")
        analyzeFiles [|
            for item in 0 .. fsharpFileNodes.Count - 1 ->
                let relativePath = fsharpFileNodes.[item].Attributes.["Include"].InnerText
                let projectParent = Directory.GetParent project
                File(Path.Combine(projectParent.FullName, relativePath))
        |]
