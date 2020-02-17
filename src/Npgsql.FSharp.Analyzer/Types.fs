namespace Npgsql.FSharp.Analyzers

open FSharp.Compiler.Range

type ColumnReadAttempt = {
    funcName: string;
    columnName: string;
    range : range
}

type UsedParameter = {
    parameter : string
    range : range 
}

[<RequireQualifiedAccess>]
type SqlAnalyzerBlock =
    | Query of string * range
    | StoredProcedure of string * range
    | Parameters of UsedParameter list *  range
    | ReadingColumns of ColumnReadAttempt list

type SqlOperation = {
    blocks : SqlAnalyzerBlock list
    range : range
    fileName : string
}
