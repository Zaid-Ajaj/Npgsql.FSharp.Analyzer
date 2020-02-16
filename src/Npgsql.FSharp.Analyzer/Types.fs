namespace Npgsql.FSharp.Analyzers

open FSharp.Compiler.SourceCodeServices

type ColumnReadAttempt = {
    funcName: string;
    columnName: string;
    range : Range
}

[<RequireQualifiedAccess>]
type SqlAnalyzerBlock =
    | Query of string * Range
    | Parameters of {| parameter: string; range: Range |} list *  Range
    | ReadingColumns of ColumnReadAttempt list

type SqlOperation = {
    blocks : SqlAnalyzerBlock list
    range : Range
    fileName : string
}
