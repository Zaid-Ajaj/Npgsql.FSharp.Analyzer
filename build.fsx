#load ".fake/build.fsx/intellisense.fsx"
#load "docsTool/CLI.fs"
#if !FAKE
#r "Facades/netstandard"
#r "netstandard"
#endif

#r "System.IO.Compression.FileSystem.dll"

open System
open System.IO
open System.IO.Compression
open Fake.SystemHelper
open Fake.Core
open Fake.DotNet
open Fake.Tools
open Fake.IO
open Fake.IO.FileSystemOperators

open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators
open Fake.Api
open Fake.BuildServer
open Fantomas
open Fantomas.FakeHelpers


BuildServer.install [
    AppVeyor.Installer
    Travis.Installer
]

let environVarAsBoolOrDefault varName defaultValue =
    let truthyConsts = [
        "1"
        "Y"
        "YES"
        "T"
        "TRUE"
    ]
    try
        let envvar = (Environment.environVar varName).ToUpper()
        truthyConsts |> List.exists((=)envvar)
    with
    | _ ->  defaultValue

//-----------------------------------------------------------------------------
// Metadata and Configuration
//-----------------------------------------------------------------------------

let productName = "NpgsqlFSharpAnalyzer"
let sln = "NpgsqlFSharpAnalyzer.sln"


let srcCodeGlob =
    !! (__SOURCE_DIRECTORY__  @@ "src/**/*.fs")
    ++ (__SOURCE_DIRECTORY__  @@ "src/**/*.fsx")

let testsCodeGlob =
    !! (__SOURCE_DIRECTORY__  @@ "tests/**/*.fs")
    ++ (__SOURCE_DIRECTORY__  @@ "tests/**/*.fsx")

let srcGlob =__SOURCE_DIRECTORY__  @@ "src/**/*.??proj"
let testsGlob = __SOURCE_DIRECTORY__  @@ "tests/**/*.??proj"

let srcAndTest =
    !! srcGlob
    ++ testsGlob

let distDir = __SOURCE_DIRECTORY__  @@ "dist"
let distGlob = distDir @@ "*.nupkg"

let coverageThresholdPercent = 1
let coverageReportDir =  __SOURCE_DIRECTORY__  @@ "docs" @@ "coverage"


let docsDir = __SOURCE_DIRECTORY__  @@ "docs"
let docsSrcDir = __SOURCE_DIRECTORY__  @@ "docsSrc"
let docsToolDir = __SOURCE_DIRECTORY__ @@ "docsTool"

let releaseNotes = Fake.Core.ReleaseNotes.load "RELEASE_NOTES.md"

let publishUrl = "https://www.nuget.org"

let disableCodeCoverage = environVarAsBoolOrDefault "DISABLE_COVERAGE" true

//-----------------------------------------------------------------------------
// Helpers
//-----------------------------------------------------------------------------

let isRelease (targets : Target list) =
    targets
    |> Seq.map(fun t -> t.Name)
    |> Seq.exists ((=)"Release")

let invokeAsync f = async { f () }

let configuration (targets : Target list) =
    let defaultVal = if isRelease targets then "Release" else "Debug"
    match Environment.environVarOrDefault "CONFIGURATION" defaultVal with
    | "Debug" -> DotNet.BuildConfiguration.Debug
    | "Release" -> DotNet.BuildConfiguration.Release
    | config -> DotNet.BuildConfiguration.Custom config

let failOnBadExitAndPrint (p : ProcessResult) =
    if p.ExitCode <> 0 then
        p.Errors |> Seq.iter Trace.traceError
        failwithf "failed with exitcode %d" p.ExitCode

// CI Servers can have bizzare failures that have nothing to do with your code
let rec retryIfInCI times fn =
    match Environment.environVarOrNone "CI" with
    | Some _ ->
        if times > 1 then
            try
                fn()
            with
            | _ -> retryIfInCI (times - 1) fn
        else
            fn()
    | _ -> fn()

module dotnet =
    let watch cmdParam program args =
        DotNet.exec cmdParam (sprintf "watch %s" program) args

    let run cmdParam args =
        DotNet.exec cmdParam "run" args

    let tool optionConfig command args =
        DotNet.exec optionConfig (sprintf "%s" command) args
        |> failOnBadExitAndPrint

    let reportgenerator optionConfig args =
        tool optionConfig "reportgenerator" args

    let sourcelink optionConfig args =
        tool optionConfig "sourcelink" args

    let fcswatch optionConfig args =
        tool optionConfig "fcswatch" args

open DocsTool.CLIArgs
module DocsTool =
    open Argu
    let buildparser = ArgumentParser.Create<BuildArgs>(programName = "docstool")
    let buildCLI =
        [
            BuildArgs.ProjectGlob srcGlob
            BuildArgs.DocsOutputDirectory docsDir
            BuildArgs.DocsSourceDirectory docsSrcDir
            BuildArgs.ReleaseVersion releaseNotes.NugetVersion
        ]
        |> buildparser.PrintCommandLineArgumentsFlat

    let build () =
        dotnet.run (fun args ->
            { args with WorkingDirectory = docsToolDir }
        ) (sprintf " -- build %s" (buildCLI))
        |> failOnBadExitAndPrint

    let watchparser = ArgumentParser.Create<WatchArgs>(programName = "docstool")
    let watchCLI =
        [
            WatchArgs.ProjectGlob srcGlob
            WatchArgs.DocsSourceDirectory docsSrcDir
            WatchArgs.ReleaseVersion releaseNotes.NugetVersion
        ]
        |> watchparser.PrintCommandLineArgumentsFlat

    let watch projectpath =
        dotnet.watch (fun args ->
           { args with WorkingDirectory = docsToolDir }
        ) "run" (sprintf "-- watch %s" (watchCLI))
        |> failOnBadExitAndPrint

//-----------------------------------------------------------------------------
// Target Implementations
//-----------------------------------------------------------------------------

let clean _ =
    ["bin"; "temp" ; distDir; coverageReportDir]
    |> Shell.cleanDirs

    !! srcGlob
    ++ testsGlob
    |> Seq.collect(fun p ->
        ["bin";"obj"]
        |> Seq.map(fun sp -> IO.Path.GetDirectoryName p @@ sp ))
    |> Shell.cleanDirs

    [
        "paket-files/paket.restore.cached"
    ]
    |> Seq.iter Shell.rm

let dotnetRestore _ =
    let restoreExitCode = Shell.Exec("dotnet", "restore", __SOURCE_DIRECTORY__)
    if restoreExitCode <> 0
    then failwith "Restore failed"

let dotnetBuild _ =
    let buildResult = Shell.Exec("dotnet", "build", __SOURCE_DIRECTORY__)
    if buildResult <> 0
    then failwith "dotnet build failed"

let dotnetTest ctx =
    let testResult = Shell.Exec("dotnet", "run", "tests" </> "NpgsqlFSharpAnalyzer.Tests")
    if testResult <> 0
    then failwith "Tests failed"

let generateCoverageReport _ =
    let coverageReports =
        !!"tests/**/coverage.*.xml"
        |> String.concat ";"
    let sourceDirs =
        !! srcGlob
        |> Seq.map Path.getDirectory
        |> String.concat ";"
    let independentArgs =
            [
                sprintf "-reports:%s"  coverageReports
                sprintf "-targetdir:%s" coverageReportDir
                // Add source dir
                sprintf "-sourcedirs:%s" sourceDirs
                // Ignore Tests and if AltCover.Recorder.g sneaks in
                sprintf "-assemblyfilters:\"%s\"" "-*.Tests;-AltCover.Recorder.g"
                sprintf "-Reporttypes:%s" "Html"
            ]
    let args =
        independentArgs
        |> String.concat " "
    dotnet.reportgenerator id args

let watchTests _ =
    !! testsGlob
    |> Seq.map(fun proj -> fun () ->
        dotnet.watch
            (fun opt ->
                opt |> DotNet.Options.withWorkingDirectory (IO.Path.GetDirectoryName proj))
            "test"
            ""
        |> ignore
    )
    |> Seq.iter (invokeAsync >> Async.Catch >> Async.Ignore >> Async.Start)

    printfn "Press Ctrl+C (or Ctrl+Break) to stop..."
    let cancelEvent = Console.CancelKeyPress |> Async.AwaitEvent |> Async.RunSynchronously
    cancelEvent.Cancel <- true

let generateAssemblyInfo _ =

    let (|Fsproj|Csproj|Vbproj|) (projFileName:string) =
        match projFileName with
        | f when f.EndsWith("fsproj") -> Fsproj
        | f when f.EndsWith("csproj") -> Csproj
        | f when f.EndsWith("vbproj") -> Vbproj
        | _                           -> failwith (sprintf "Project file %s not supported. Unknown project type." projFileName)

    let releaseChannel =
        match releaseNotes.SemVer.PreRelease with
        | Some pr -> pr.Name
        | _ -> "release"
    let getAssemblyInfoAttributes projectName =
        [
            AssemblyInfo.Title (projectName)
            AssemblyInfo.Product productName
            AssemblyInfo.Version releaseNotes.AssemblyVersion
            AssemblyInfo.Metadata("ReleaseDate", releaseNotes.Date.Value.ToString("o"))
            AssemblyInfo.FileVersion releaseNotes.AssemblyVersion
            AssemblyInfo.InformationalVersion releaseNotes.AssemblyVersion
            AssemblyInfo.Metadata("ReleaseChannel", releaseChannel)
            AssemblyInfo.Metadata("GitHash", Git.Information.getCurrentSHA1(null))
        ]

    let getProjectDetails projectPath =
        let projectName = IO.Path.GetFileNameWithoutExtension(projectPath)
        (
            projectPath,
            projectName,
            IO.Path.GetDirectoryName(projectPath),
            (getAssemblyInfoAttributes projectName)
        )

    srcAndTest
    |> Seq.map getProjectDetails
    |> Seq.iter (fun (projFileName, _, folderName, attributes) ->
        match projFileName with
        | Fsproj -> AssemblyInfoFile.createFSharp (folderName @@ "AssemblyInfo.fs") attributes
        | Csproj -> AssemblyInfoFile.createCSharp ((folderName @@ "Properties") @@ "AssemblyInfo.cs") attributes
        | Vbproj -> AssemblyInfoFile.createVisualBasic ((folderName @@ "My Project") @@ "AssemblyInfo.vb") attributes
        )

let dotnetPack ctx =
    let args =
        [
            "pack"
            "--configuration Release"
            sprintf "/p:PackageVersion=%s" releaseNotes.NugetVersion
            sprintf "/p:PackageReleaseNotes=\"%s\"" (String.concat "\n" releaseNotes.Notes)
            sprintf "--output %s" (__SOURCE_DIRECTORY__ </> "dist")
        ]

    let exitCode = Shell.Exec("dotnet", String.concat " " args, "src" </> "NpgsqlFSharpAnalyzer")
    if exitCode <> 0 then
        failwith "dotnet pack failed"
    else
        match Shell.Exec("dotnet", "publish -c Release --framework netcoreapp2.0", "src" </> "NpgsqlFSharpAnalyzer") with
        | 0 ->
            let nupkg =
                System.IO.Directory.GetFiles(__SOURCE_DIRECTORY__ </> "dist")
                |> Seq.head
                |> IO.Path.GetFullPath

            let nugetParent = DirectoryInfo(nupkg).Parent.FullName
            let nugetFileName = IO.Path.GetFileNameWithoutExtension(nupkg)

            let publishPath = "src" </> "NpgsqlFSharpAnalyzer" </> "bin" </> "Release" </> "netcoreapp2.0" </> "publish"
            ZipFile.ExtractToDirectory(nupkg, nugetParent </> nugetFileName)
            File.Delete nupkg
            Shell.deleteDir (nugetParent </> nugetFileName </> "lib" </> "netcoreapp2.0")
            Shell.copyDir (nugetParent </> nugetFileName </> "lib" </> "netcoreapp2.0") publishPath (fun _ -> true)
            ZipFile.CreateFromDirectory(nugetParent </> nugetFileName, nupkg)
            Shell.deleteDir(nugetParent </> nugetFileName)
        | _ ->
            ()

let publishToNuget _ =
    let nugetKey =
        match Environment.environVarOrNone "NUGET_KEY" with
        | Some nugetKey -> nugetKey
        | None -> failwith "The Nuget API key must be set in a NUGET_KEY environmental variable"

    let nupkg =
        System.IO.Directory.GetFiles(__SOURCE_DIRECTORY__ </> "dist")
        |> Seq.head
        |> IO.Path.GetFullPath

    let exitCode = Shell.Exec("dotnet", sprintf "nuget push %s -s nuget.org -k %s" nupkg nugetKey, "dist")
    if exitCode <> 0
    then failwith "Could not publish package"

let buildDocs _ =
    DocsTool.build ()

let watchDocs _ =
    let watchBuild () =
        !! srcGlob
        |> Seq.map(fun proj -> fun () ->
            dotnet.watch
                (fun opt ->
                    opt |> DotNet.Options.withWorkingDirectory (IO.Path.GetDirectoryName proj))
                "build"
                ""
            |> ignore
        )
        |> Seq.iter (invokeAsync >> Async.Catch >> Async.Ignore >> Async.Start)
    watchBuild ()
    DocsTool.watch ()


//-----------------------------------------------------------------------------
// Target Declaration
//-----------------------------------------------------------------------------

Target.create "Clean" clean
Target.create "DotnetRestore" dotnetRestore
Target.create "DotnetBuild" dotnetBuild
Target.create "DotnetTest" dotnetTest
Target.create "GenerateCoverageReport" generateCoverageReport
Target.create "WatchTests" watchTests
Target.create "GenerateAssemblyInfo" generateAssemblyInfo
Target.create "DotnetPack" dotnetPack
Target.create "PublishToNuGet" publishToNuget
Target.create "Release" ignore
Target.create "BuildDocs" buildDocs
Target.create "WatchDocs" watchDocs

//-----------------------------------------------------------------------------
// Target Dependencies
//-----------------------------------------------------------------------------

// Ensure Clean is called before DotnetRestore
"Clean" ==> "DotnetRestore"
"Clean" ==> "DotnetPack"

// Only call AssemblyInfo if Publish was in the call chain
// Ensure AssemblyInfo is called after DotnetRestore and before DotnetBuild
"DotnetRestore" ?=> "GenerateAssemblyInfo"
"GenerateAssemblyInfo" ?=> "DotnetBuild"
"GenerateAssemblyInfo" ==> "PublishToNuGet"

"DotnetBuild" ==> "BuildDocs"
"BuildDocs" ?=> "PublishToNuget"
"DotnetPack" ?=> "BuildDocs"

"DotnetBuild" ==> "WatchDocs"

"DotnetRestore"
    ==> "DotnetBuild"
    ==> "DotnetTest"
    =?> ("GenerateCoverageReport", not disableCodeCoverage)
    ==> "DotnetPack"
    ==> "PublishToNuGet"
    ==> "Release"

"DotnetRestore"
    ==> "WatchTests"

//-----------------------------------------------------------------------------
// Target Start
//-----------------------------------------------------------------------------

Target.runOrDefaultWithArguments "DotnetPack"
