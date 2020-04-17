namespace Npgsql.FSharp.Analyzers

open FSharp.Analyzers.SDK
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.Range

module SyntacticAnalysis =

    let (|FuncName|_|) = function
        | SynExpr.Ident ident -> Some (ident.idText)
        | SynExpr.LongIdent(isOptional, longDotId, altName, range) ->
            match longDotId with
            | LongIdentWithDots(listOfIds, ranges) ->
                let fullName =
                    listOfIds
                    |> List.map (fun id -> id.idText)
                    |> String.concat "."

                Some fullName
        | _ -> None

    let (|Apply|_|) = function
        | SynExpr.App(atomicFlag, isInfix, funcExpr, argExpr, applicationRange) ->
            match funcExpr with
            | SynExpr.Ident ident -> Some (ident.idText, argExpr, funcExpr.Range, applicationRange)
            | SynExpr.LongIdent(isOptional, longDotId, altName, identRange) ->
                match longDotId with
                | LongIdentWithDots(listOfIds, ranges) ->
                    let fullName =
                        listOfIds
                        |> List.map (fun id -> id.idText)
                        |> String.concat "."

                    Some (fullName, argExpr, funcExpr.Range, applicationRange)
            | _ -> None
        | _ -> None

    let (|ParameterTuple|_|) = function
        | SynExpr.Tuple(isStruct, [ SynExpr.Const(SynConst.String(parameterName, paramRange), constRange); Apply(funcName, exprArgs, funcRange, appRange) ], commaRange, tupleRange) ->
            Some (parameterName, paramRange, funcName, funcRange, Some appRange)
        | SynExpr.Tuple(isStruct, [ SynExpr.Const(SynConst.String(parameterName, paramRange), constRange); secondItem ], commaRange, tupleRange) ->
            match secondItem with
            | SynExpr.LongIdent(isOptional, longDotId, altName, identRange) ->
                match longDotId with
                | LongIdentWithDots(listOfIds, ranges) ->
                    let fullName =
                        listOfIds
                        |> List.map (fun id -> id.idText)
                        |> String.concat "."

                    Some (parameterName, paramRange, fullName, identRange, None)
            | _ ->
                None
        | _ ->
            None

    let rec readParameters = function
        | ParameterTuple (name, range, func, funcRange, appRange) ->
            [ name, range, func, funcRange, appRange ]
        | SynExpr.Sequential(_debugSeqPoint, isTrueSeq, expr1, expr2, seqRange) ->
            [ yield! readParameters expr1; yield! readParameters expr2 ]
        | _ ->
            [ ]

    let (|SqlParameters|_|) = function
        | Apply ("Sql.parameters", SynExpr.ArrayOrListOfSeqExpr(isArray, listExpr, listRange) , funcRange, appRange) ->
            match listExpr with
            | SynExpr.CompExpr(isArrayOfList, isNotNakedRefCell, compExpr, compRange) ->
                Some (readParameters compExpr, compRange)
            | _ ->
                None
        | _ ->
            None

    let (|ReadColumnAttempt|_|) = function
        | Apply(funcName, SynExpr.Const(SynConst.String(columnName, queryRange), constRange), funcRange, appRange) ->
            if funcName.StartsWith "Sql.read" && funcName <> "Sql.readRow"
            then Some {
                funcName = funcName
                columnName = columnName
                columnNameRange = constRange
                funcCallRange = funcRange }
            else
                let possibleFunctions = [
                    ".int"
                    ".intOrNone"
                    ".bool"
                    ".bootOrNone"
                    ".text"
                    ".textOrNone"
                    ".int16"
                    ".int16OrNone"
                    ".int64"
                    ".int64OrNone"
                    ".string"
                    ".stringorNone"
                    ".decimal"
                    ".decimalOrNone"
                    ".bytea"
                    ".byteaOrNone"
                    ".double"
                    ".doubleOrNone"
                    ".timestamp"
                    ".timestampOrNone"
                    ".timestamptz"
                    ".timestamptzOrNone"
                    ".uuid"
                    ".uuidOrNone"
                    ".float"
                    ".floatOrNone"
                    ".interval"
                    ".date"
                    ".dateOrNone"
                    ".dateTime"
                    ".dateTimeOrNone"
                    ".intArray"
                    ".intArrayOrNone"
                    ".stringArray"
                    ".stringArrayOrNone"
                ]

                if possibleFunctions |> List.exists funcName.EndsWith then
                    Some {
                        funcName = funcName
                        columnName = columnName
                        columnNameRange = constRange
                        funcCallRange = funcRange }
                else
                    None
        | _ ->
           None

    /// Detects `Sql.query {SQL}` pattern
    let (|SqlQuery|_|) = function
        | Apply("Sql.query", SynExpr.Const(SynConst.String(query, queryRange), constRange), range, appRange) ->
            Some (query, constRange)
        | _ ->
            None

    let (|LiteralQuery|_|) = function
        | Apply("Sql.query", SynExpr.Ident(identifier), funcRange, appRange) ->
            Some (identifier.idText, funcRange)
        | _ ->
            None

    let (|SqlStoredProcedure|_|) = function
        | Apply("Sql.func", SynExpr.Const(SynConst.String(funcName, funcNameRange), constRange), funcRange, appRange) ->
            Some (funcName, constRange)
        | _ ->
            None

    let rec findQuery = function
        | SqlQuery (query, range) ->
            [ SqlAnalyzerBlock.Query(query, range) ]
        | LiteralQuery (identifier, range) ->
            [ SqlAnalyzerBlock.LiteralQuery(identifier, range) ]
        | SynExpr.App(exprAtomic, isInfix, funcExpr, argExpr, range) ->
            [ yield! findQuery funcExpr; yield! findQuery argExpr ]
        | _ ->
            [ ]

    let rec findFunc = function
        | SqlStoredProcedure (funcName, range) ->
            [ SqlAnalyzerBlock.StoredProcedure(funcName, range) ]
        | SynExpr.App(exprAtomic, isInfix, funcExpr, argExpr, range) ->
            [ yield! findFunc funcExpr; yield! findFunc argExpr ]
        | _ ->
            [ ]

    let rec findParameters = function
        | SqlParameters(parameters, range) ->
            let sqlParameters =
                parameters
                |> List.map (fun (name, range, func, funcRange, appRange) -> { name = name.TrimStart('@'); range = range; paramFunc = func; paramFuncRange = funcRange; applicationRange = appRange })
            [ SqlAnalyzerBlock.Parameters(sqlParameters, range) ]

        | SynExpr.App(exprAtomic, isInfix, funcExpr, argExpr, range) ->
            [ yield! findParameters funcExpr; yield! findParameters argExpr ]

        | _ ->
            [ ]

    let rec findReadColumnAttempts = function
        | ReadColumnAttempt (attempt) ->
            [ attempt ]
        | SynExpr.App(exprAtomic, isInfix, funcExpr, argExpr, range) ->
            [ yield! findReadColumnAttempts funcExpr; yield! findReadColumnAttempts argExpr ]
        | SynExpr.Paren(expr, leftRange, rightRange, range) ->
            [ yield! findReadColumnAttempts expr ]
        | SynExpr.Lambda(fromMethod, inLambdaSeq, args, body, range) ->
            [ yield! findReadColumnAttempts body ]
        | SynExpr.LetOrUse(isRecursive, isUse, bindings, body, range) ->
             [ yield! findReadColumnAttempts body
               for binding in bindings do
               match binding with
               | SynBinding.Binding (access, kind, mustInline, isMutable, attrs, xmlDecl, valData, headPat, returnInfo, expr, range, seqPoint) ->
                   yield! findReadColumnAttempts expr ]

        | SynExpr.LetOrUseBang(sequencePoint, isUse, isFromSource, syntaxPattern, expr1, andExprs, body, range) ->
            [
                yield! findReadColumnAttempts expr1
                yield! findReadColumnAttempts body
                for (pointInfo, _, _, pattern, expr, range) in andExprs do
                    yield! findReadColumnAttempts expr
            ]

        | SynExpr.CompExpr(isArray, _, expression, range) ->
            [ yield! findReadColumnAttempts expression ]

        | SynExpr.AnonRecd(isStruct, copyInfo, recordFields, range) ->
            [
                match copyInfo with
                | Some(expr, info) -> yield! findReadColumnAttempts expr
                | None -> ()

                for (fieldName, fieldBody) in recordFields do
                    yield! findReadColumnAttempts fieldBody
            ]

        | SynExpr.ArrayOrList(isList, elements, range) ->
            [
                for elementExpr in elements do
                    yield! findReadColumnAttempts elementExpr
            ]

        | SynExpr.ArrayOrListOfSeqExpr (isArray, body, range) ->
            [
                yield! findReadColumnAttempts body
            ]

        | SynExpr.Record(info, copyInfo, recordFields, range) ->
            [
                for (fieldName, body, blockSep) in recordFields do
                    match body with
                    | Some bodyExpr ->  yield! findReadColumnAttempts bodyExpr
                    | None -> ()
            ]

        | SynExpr.IfThenElse(ifExpr, elseExpr, thenExpr, _, _, _, _) ->
            [
                yield! findReadColumnAttempts ifExpr
                yield! findReadColumnAttempts elseExpr
                match thenExpr with
                | Some expr -> yield! findReadColumnAttempts expr
                | None -> ()
            ]

        | SynExpr.Lambda(_, _, args, body, range) ->
            [
                yield! findReadColumnAttempts body
            ]

        | SynExpr.Lazy(body, range) ->
            [
                yield! findReadColumnAttempts body
            ]

        | SynExpr.New(protocol, typeName, expr, range) ->
            [
                yield! findReadColumnAttempts expr
            ]

        | SynExpr.Tuple (isStruct, exprs, commaRanges, range) ->
            [
                for expr in exprs do
                    yield! findReadColumnAttempts expr
            ]

        | SynExpr.Match(seqPoint, matchExpr, clauses, range) ->
            [
                yield! findReadColumnAttempts matchExpr
                for SynMatchClause.Clause(pattern, whenExpr, body, range, seqPoint) in clauses do
                    yield! findReadColumnAttempts body
                    match whenExpr with
                    | Some body -> yield! findReadColumnAttempts body
                    | None -> ()
            ]

        | SynExpr.MatchBang(seqPoint, matchExpr, clauses, range) ->
            [
                yield! findReadColumnAttempts matchExpr
                for SynMatchClause.Clause(pattern, whenExpr, body, range, seqPoint) in clauses do
                    yield! findReadColumnAttempts body
                    match whenExpr with
                    | Some body -> yield! findReadColumnAttempts body
                    | None -> ()
            ]

        | _ ->
            [ ]


    let rec visitSyntacticExpression (expr: SynExpr) (fullExpressionRange: range) =
        match expr with
        | SynExpr.App(exprAtomic, isInfix, funcExpr, argExpr, range) ->
            match argExpr with
            | Apply(("Sql.executeReader"|"Sql.executeReaderAsync"), lambdaExpr, _, appRange) ->
                let columns = findReadColumnAttempts lambdaExpr
                let blocks = [
                    yield! findQuery funcExpr
                    yield! findParameters funcExpr
                    yield! findFunc funcExpr
                    yield SqlAnalyzerBlock.ReadingColumns columns
                ]

                [ { blocks = blocks; range = range; } ]

            | Apply(("Sql.execute"|"Sql.executeAsync"), lambdaExpr, funcRange, appRange) ->
                let columns = findReadColumnAttempts lambdaExpr
                let blocks = [
                    yield! findQuery funcExpr
                    yield! findParameters funcExpr
                    yield! findFunc funcExpr
                    yield SqlAnalyzerBlock.ReadingColumns columns
                ]

                [ { blocks = blocks; range = range; } ]

            | SqlQuery(query, queryRange) ->

                let blocks = [
                    SqlAnalyzerBlock.Query(query, queryRange)
                ]

                [ { blocks = blocks; range = range; } ]

            | LiteralQuery(identifier, queryRange) ->
                let blocks = [
                    SqlAnalyzerBlock.LiteralQuery(identifier, queryRange)
                ]

                [ { blocks = blocks; range = range; } ]

            | SqlParameters(parameters, range) ->
                let sqlParameters =
                    parameters
                    |> List.map (fun (name, range, func, funcRange, appRange) -> { name = name.TrimStart('@'); range = range; paramFunc = func; paramFuncRange = funcRange; applicationRange = appRange })

                let blocks = [
                    yield! findQuery funcExpr
                    yield SqlAnalyzerBlock.Parameters(sqlParameters, range)
                ]

                [ { blocks = blocks; range = range; } ]

            | FuncName(functionWithoutParameters) ->
                let blocks = [
                    yield! findFunc funcExpr
                    yield! findQuery funcExpr
                    yield! findParameters funcExpr
                ]

                [ { blocks = blocks; range = range; } ]

            | Apply(anyOtherFunction, functionArg, range, appRange) ->
                let blocks = [
                    yield! findFunc funcExpr
                    yield! findQuery funcExpr
                    yield! findParameters funcExpr
                    yield SqlAnalyzerBlock.ReadingColumns (findReadColumnAttempts funcExpr)
                ]

                [ { blocks = blocks; range = range; } ]
            | _ ->
                [ ]
        | SynExpr.LetOrUse(isRecursive, isUse, bindings, body, range) ->
            [
                yield! visitSyntacticExpression body range
                for binding in bindings do yield! visitBinding binding
            ]

        | otherwise ->
            [ ]

    and visitBinding (binding: SynBinding) : SqlOperation list =
        match binding with
        | SynBinding.Binding (access, kind, mustInline, isMutable, attrs, xmlDecl, valData, headPat, returnInfo, expr, range, seqPoint) ->
            visitSyntacticExpression expr range

    let findLiterals (ctx: Context) =
        let values = new ResizeArray<string * string>()
        for symbol in ctx.Symbols |> Seq.collect (fun s -> s.TryGetMembersFunctionsAndValues) do
            match symbol.LiteralValue with
            | Some value when value.GetType() = typeof<string> ->
                values.Add((symbol.LogicalName, unbox<string> value))
            | _ -> ()

        Map.ofSeq values

    /// Tries to replace [<Literal>] strings inside the module with the identifiers that were used with Sql.query.
    let applyLiterals (literals: Map<string, string>) (operation: SqlOperation) =
        let modifiedBlocks =
            operation.blocks
            |> List.choose (function
                | SqlAnalyzerBlock.LiteralQuery(identifier, range) ->
                    match literals.TryFind identifier with
                    | Some literalQuery -> Some (SqlAnalyzerBlock.Query(literalQuery, range))
                    | None -> None
                | differentBlock ->
                    Some differentBlock)

        { operation with blocks = modifiedBlocks }

    let findSqlOperations (ctx: Context) =
        let operations = ResizeArray<SqlOperation>()
        match ctx.ParseTree with
        | ParsedInput.ImplFile input ->
            match input with
            | ParsedImplFileInput.ParsedImplFileInput(fileName, isScript, qualifiedName, _, _, modules, _) ->
                for parsedModule in modules do
                    match parsedModule with
                    | SynModuleOrNamespace(identifier, isRecursive, kind, declarations, _, _, _, _) ->
                        for declaration in declarations do
                            match declaration with
                            | SynModuleDecl.Let(isRecursiveDef, bindings, range) ->
                                for binding in bindings do
                                    operations.AddRange (visitBinding binding)
                            | _ ->
                                ()

        | ParsedInput.SigFile file ->
            ()

        let moduleLiterals = findLiterals ctx
        operations
        |> Seq.map (applyLiterals moduleLiterals)
        |> Seq.toList
