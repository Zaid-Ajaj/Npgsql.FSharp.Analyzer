[<RequireQualifiedAccess>]
module Tools

open Fake.IO
open Nuke.Common.Tooling
open Fake.Core

let dotnetPath = ToolPathResolver.GetPathExecutable("dotnet")
let npm = ToolPathResolver.GetPathExecutable("npm")
let node = ToolPathResolver.GetPathExecutable("node")

let dotnet argument workingDir =
    if Shell.Exec(dotnetPath, argument, workingDir) <> 0
    then failwithf "FAILED %s> dotnet %s" workingDir argument 
