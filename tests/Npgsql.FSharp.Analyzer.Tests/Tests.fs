module Tests

open System
open Expecto
open BinaryDefense.FSharp.Analyzers

let analyzers = [
    SqlAnalysis.queryAnalyzer
]

let testSeq name tests = testList name (List.ofSeq tests)

let inline find file = IO.Path.Combine(__SOURCE_DIRECTORY__ , file)

let inline analyze file = AnalyzerBootstrap.runProject file analyzers

let inline context file = AnalyzerBootstrap.context file

[<Tests>]
let tests =
    testList "Postgres" [
        test "Syntactic Analysis" {
            match context (find "../examples/hashing/syntacticAnalysis.fs") with
            | None -> failwith "Could not crack project"
            | Some context ->
                let operationBlocks = SqlAnalysis.findSqlBlocks context 
                Expect.equal 2 (List.length operationBlocks) "Found two operation blocks"
        }
    ]
