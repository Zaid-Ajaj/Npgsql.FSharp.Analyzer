open System
open System.IO
open Fake.IO

let targets = ResizeArray<string * (unit -> unit)>()

let target name f = targets.Add(name, f)

let rec retry n f =
    if n = 0 then
        ignore()
    else
        try f()
        with ex ->
            System.Threading.Thread.Sleep(1000)
            retry (n - 1) f

// Paths to different directories in the solution
let path xs = Path.Combine(Array.ofList xs)
let solutionRoot = Files.findParent __SOURCE_DIRECTORY__ "NpgsqlFSharpAnalyzer.sln";
let analyzerCore = path [ solutionRoot; "src"; "NpgsqlFSharpAnalyzer" ]
let analyzer = path [ solutionRoot; "src"; "NpgsqlFSharpAnalyzer" ]
let analyzerVs = path [ solutionRoot; "src"; "NpgsqlFSharpVs" ]
let tests = path [ solutionRoot; "tests"; "NpgsqlFSharpAnalyzer.Tests" ]

let clean() =
    retry 5 <| fun _ -> List.iter Shell.deleteDir [
        path [ analyzerCore; "bin" ]
        path [ analyzerCore; "obj" ]
        path [ analyzer; "bin" ]
        path [ analyzer; "obj" ]
        path [ analyzerVs; "bin" ]
        path [ analyzerVs; "obj" ]
        path [ tests; "bin" ]
        path [ tests; "obj" ]
    ]

let build() = Tools.dotnet "build -c Release" solutionRoot
let test() = Tools.dotnet "run" tests

target "Clean" clean

target "Test" (fun _ ->
    clean()
    test()
)

target "Build" (fun _ ->
    clean()
    build()
)

target "Default" test

let run target =
    targets
    |> Seq.find (fun (t, f) -> t = target)
    |> fun (target, execute) -> execute()

[<EntryPoint>]
let main args =
    Console.WriteLine(Swag.logo)

    try
        match args with
        | [| "RunDefaultOr" |] -> run "Default"
        | [| "RunDefaultOr"; target |] -> run target
        | _ ->
            Console.WriteLine("[Interactive Mode] Available Targets: ")
            for (addedTarget, execute) in targets do Console.WriteLine(sprintf "  | -- %s" addedTarget);
            Console.WriteLine()
            Console.Write("[Interactive Mode] Run build target: ")
            let target = Console.ReadLine()
            if not (targets |> Seq.exists (fun (t, f) -> t = target)) then
                Console.WriteLine(sprintf "Target %s was not recognized" target)
            else
                run target
        0
    with ex ->
        printfn "%s" ex.Message
        1
