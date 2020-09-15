namespace Npgsql.FSharp.Analyzers.Core

open FSharp.Compiler.Range

open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.SourceCodeServices

type SqlAnalyzerContext =
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

    with
        member self.IsWarning() = self.Severity = Warning
        member self.IsInfo() = self.Severity = Info
        member self.IsError() = self.Severity = Error

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

type ParameterSet = {
    parameters : UsedParameter list
    range : range
}

type TransactionQuery = {
    query: string
    queryRange : range
    parameterSets : ParameterSet list
}

[<RequireQualifiedAccess>]
type SqlAnalyzerBlock =
    | Query of string * range
    | LiteralQuery of ident:string * range
    | StoredProcedure of string * range
    | Parameters of UsedParameter list *  range
    | ReadingColumns of ColumnReadAttempt list
    | Transaction of TransactionQuery list
    | SkipAnalysis 

type SqlOperation = {
    blocks : SqlAnalyzerBlock list
    range : range
}
