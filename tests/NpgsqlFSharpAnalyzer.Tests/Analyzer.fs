module AnalyzerBootstrap

open System
open System.IO
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Text
open FSharp.Analyzers.SDK

let checker =
    FSharpChecker.Create(
        keepAllBackgroundResolutions = true,
        keepAssemblyContents = true,
        ImplicitlyStartBackgroundWork = true)

let dumpOpts (opts :  FSharpProjectOptions) =
    printfn  "FSharpProjectOptions.OtherOptions ->"
    opts.OtherOptions
    |> Array.iter(printfn "%s")

let loadProject file =
    async {
        let! source = IO.File.ReadAllTextAsync file |> Async.AwaitTask
        let! (opts, error) = checker.GetProjectOptionsFromScript(file, SourceText.ofString source, assumeDotNetFramework = false, useSdkRefs = true, useFsiAuxLib = true, otherFlags = [|"--targetprofile:netstandard" |])
        let newOO =
            opts.OtherOptions
            |> Array.map(fun i ->
                if i.StartsWith("-r:") then
                    let path = i.Split("-r:", StringSplitOptions.RemoveEmptyEntries).[0]

                    sprintf "-r:%s" (IO.FileInfo(path).FullName)
                else
                    i
            )
        // dumpOpts opts
        return file, opts
    } |> Async.RunSynchronously

let typeCheckFile (file,opts) =
    let text = File.ReadAllText file
    let st = SourceText.ofString text
    let (parseRes, checkAnswer) =
        checker.ParseAndCheckFileInProject(file, 1, st, opts)
        |> Async.RunSynchronously

    match checkAnswer with
    | FSharpCheckFileAnswer.Aborted ->
        printfn "Checking of file %s aborted because %A" file parseRes.Errors
        None
    | FSharpCheckFileAnswer.Succeeded(c) ->
        Some (file, text, parseRes, c)

let entityCache = EntityCache()

let getAllEntities (checkResults: FSharpCheckFileResults) (publicOnly: bool) : AssemblySymbol list =
    try
        let res = [
          yield! AssemblyContentProvider.getAssemblySignatureContent AssemblyContentType.Full checkResults.PartialAssemblySignature
          let ctx = checkResults.ProjectContext
          let assembliesByFileName =
            ctx.GetReferencedAssemblies()
            |> Seq.groupBy (fun asm -> asm.FileName)
            |> Seq.map (fun (fileName, asms) -> fileName, List.ofSeq asms)
            |> Seq.toList
            |> List.rev // if mscorlib.dll is the first then FSC raises exception when we try to
                        // get Content.Entities from it.

          for fileName, signatures in assembliesByFileName do
            let contentType = if publicOnly then Public else Full
            let content = AssemblyContentProvider.getAssemblyContent entityCache.Locking contentType fileName signatures
            yield! content
        ]
        res
    with
    | _ -> []

let createContext (file, text: string, p: FSharpParseFileResults,c: FSharpCheckFileResults) =
    match p.ParseTree, c.ImplementationFile with
    | Some pt, Some tast ->
        let context : Context = {
            FileName = file
            Content = text.Split([|'\n'|])
            ParseTree = pt
            TypedTree = tast
            Symbols = c.PartialAssemblySignature.Entities |> Seq.toList
            GetAllEntities = getAllEntities c
        }
        Some context
    | _ -> None

let context proj =
    let path =
        Path.Combine(Environment.CurrentDirectory, proj)
        |> Path.GetFullPath

    loadProject path
    |> typeCheckFile
    |> Option.bind createContext
