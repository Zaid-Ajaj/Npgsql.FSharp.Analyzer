module ExpectoTemplate

open Expecto
open Expecto.Logging

let config = { defaultConfig with verbosity = LogLevel.Verbose }

[<EntryPoint>]
let main argv = Tests.runTestsInAssembly config argv
