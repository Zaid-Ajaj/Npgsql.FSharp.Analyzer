namespace Npgsql.FSharp.Analyzers.Core

open FSharp.Compiler.Range

open System
open FSharp.Compiler
open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.SourceCodeServices

type SpecializedContext =
    { FileName: string
      Content: string[]
      ParseTree: ParsedInput
      Symbols: FSharpEntity list }

type Fix =
    { FromRange : range
      FromText : string
      ToText : string }

type Severity =
    | Info
    | Warning
    | Error

type Message =
    { Type: string
      Message: string
      Code: string
      Severity: Severity
      Range: range
      Fixes: Fix list }


type ColumnReadAttempt = {
    funcName: string;
    columnName: string;
    columnNameRange : range
    funcCallRange: range
}

type UsedParameter = {
    name : string
    range : range
    paramFunc : string
    paramFuncRange : range
    applicationRange : range option
}

[<RequireQualifiedAccess>]
type SqlAnalyzerBlock =
    | Query of string * range
    | LiteralQuery of ident:string * range
    | StoredProcedure of string * range
    | Parameters of UsedParameter list *  range
    | ReadingColumns of ColumnReadAttempt list

type SqlOperation = {
    blocks : SqlAnalyzerBlock list
    range : range
}
