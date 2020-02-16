namespace Npgsql.FSharp.Analyzers

open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Range

type ColumnReadAttempt = {
    funcName: string;
    columnName: string;
    range : range
}

[<RequireQualifiedAccess>]
type SqlAnalyzerBlock =
    | Query of string * range
    | Parameters of {| parameter: string; range: range |} list *  range
    | ReadingColumns of ColumnReadAttempt list

type SqlOperation = {
    blocks : SqlAnalyzerBlock list
    range : range
    fileName : string
}
