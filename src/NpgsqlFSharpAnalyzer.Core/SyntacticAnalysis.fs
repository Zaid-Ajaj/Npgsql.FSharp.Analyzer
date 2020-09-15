namespace Npgsql.FSharp.Analyzers.Core

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

    let (|Applied|_|) = function
        | SynExpr.App(atomicFlag, isInfix, funcExpr, argExpr, applicationRange) ->
            match argExpr with
            | SynExpr.Ident ident -> Some (ident.idText, funcExpr.Range, applicationRange)
            | SynExpr.LongIdent(isOptional, longDotId, altName, identRange) ->
                match longDotId with
                | LongIdentWithDots(listOfIds, ranges) ->
                    let fullName =
                        listOfIds
                        |> List.map (fun id -> id.idText)
                        |> String.concat "."

                    Some (fullName, funcExpr.Range, applicationRange)
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

    let rec flattenList = function
        | SynExpr.Sequential(_debugSeqPoint, isTrueSeq, expr1, expr2, seqRange) ->
            [ yield! flattenList expr1; yield! flattenList expr2 ]
        | expr ->
            [ expr ]

    let (|SqlParameters|_|) = function
        | Apply ("Sql.parameters", SynExpr.ArrayOrListOfSeqExpr(isArray, listExpr, listRange) , funcRange, appRange) ->
            match listExpr with
            | SynExpr.CompExpr(isArrayOfList, isNotNakedRefCell, compExpr, compRange) ->
                Some (readParameters compExpr, compRange)
            | _ ->
                None
        | _ ->
            None

    let readParameterSets parameterSetsExpr =
        let sets = ResizeArray<ParameterSet>()
        
        match parameterSetsExpr with
        | SynExpr.ArrayOrListOfSeqExpr(isArray, listExpr, listRange) ->
            match listExpr with
            | SynExpr.CompExpr(isArrayOfList, isNotNakedRefCell, outerListExpr, outerListRange) ->
                match outerListExpr with
                | SynExpr.ForEach(_, _, _, _, enumExpr, bodyExpr, forEachRange) ->
                    match bodyExpr with
                    | SynExpr.YieldOrReturn(_, outputExpr, outputExprRange) ->
                        match outputExpr with
                        | SynExpr.ArrayOrListOfSeqExpr(isArray, parameterListExpr, parameterListRange) ->
                            let parameterSet = {
                                range = parameterListRange
                                parameters = [ ]
                            }

                            match parameterListExpr with
                            | SynExpr.CompExpr(isArrayOfList, isNotNakedRefCell, compExpr, compRange) ->
                                let parameters : UsedParameter list = [
                                    for expr in flattenList compExpr do
                                        match expr with
                                        | ParameterTuple(name, range, func, funcRange, appRange) ->
                                            {
                                                paramFunc = func
                                                paramFuncRange = funcRange
                                                name = name.TrimStart '@'
                                                range = range
                                                applicationRange = appRange
                                            }
                                        | _ ->
                                            ()
                                ]

                                sets.Add { parameterSet with parameters = parameters }

                            | _ ->
                                ()

                        | SynExpr.ArrayOrList(isList, expressions, range) ->
                            let parameters : UsedParameter list = [
                                for expr in expressions do
                                    match expr with
                                    | ParameterTuple(name, range, func, funcRange, appRange) ->
                                        {
                                            paramFunc = func
                                            paramFuncRange = funcRange
                                            name = name.TrimStart '@'
                                            range = range
                                            applicationRange = appRange
                                        }
                                    | _ ->
                                        ()
                            ]

                            sets.Add {
                                range = range
                                parameters = parameters
                            }

                        | _ ->
                            ()
                    | _ ->
                        ()
                | _ -> 
                    let parameterSets = flattenList outerListExpr
                
                    for parameterSetExpr in parameterSets do
                        match parameterSetExpr with
                        | SynExpr.ArrayOrListOfSeqExpr(isArray, parameterListExpr, parameterListRange) ->
                            let parameterSet = {
                                range = parameterListRange
                                parameters = [ ]
                            }

                            match parameterListExpr with
                            | SynExpr.CompExpr(isArrayOfList, isNotNakedRefCell, compExpr, compRange) ->
                                let parameters : UsedParameter list = [
                                    for expr in flattenList compExpr do
                                        match expr with
                                        | ParameterTuple(name, range, func, funcRange, appRange) ->
                                            {
                                                paramFunc = func
                                                paramFuncRange = funcRange
                                                name = name.TrimStart '@'
                                                range = range
                                                applicationRange = appRange
                                            }
                                        | _ ->
                                            ()
                                ]

                                sets.Add { parameterSet with parameters = parameters }

                            | _ ->
                                ()

                        | SynExpr.ArrayOrList(isList, expressions, range) ->
                            let parameters : UsedParameter list = [
                                for expr in expressions do
                                    match expr with
                                    | ParameterTuple(name, range, func, funcRange, appRange) ->
                                        {
                                            paramFunc = func
                                            paramFuncRange = funcRange
                                            name = name.TrimStart '@'
                                            range = range
                                            applicationRange = appRange
                                        }
                                    | _ ->
                                        ()
                            ]

                            sets.Add {
                                range = range
                                parameters = parameters
                            }

                        | _ ->
                            ()
            | _ ->
                ()

        | _ -> ()

        Seq.toList sets

    let (|TransactionQuery|_|) = function
        | SynExpr.Tuple(isStruct, [ SynExpr.Const(SynConst.String(query, queryRange), constRange); parameterSetsExpr ], commaRange, tupleRange) ->
            let transaction =  {
                query = query;
                queryRange = queryRange
                parameterSets = readParameterSets parameterSetsExpr
            }

            Some transaction

        | SynExpr.Tuple(isStruct, [ SynExpr.Ident value; parameterSetsExpr ], commaRange, tupleRange) ->
            let transaction =  {
                query = value.idText;
                queryRange = value.idRange
                parameterSets = readParameterSets parameterSetsExpr
            }

            Some transaction
        | _ ->
            None

    let rec readTransactionQueries = function
        | TransactionQuery transactionQuery ->
            [ transactionQuery ]
        | SynExpr.Sequential(_debugSeqPoint, isTrueSeq, expr1, expr2, seqRange) ->
            [
                yield! readTransactionQueries expr1;
                yield! readTransactionQueries expr2
            ]
        | _ ->
            [ ]

    let (|SqlExecuteTransaction|_|) = function
        | Apply (("Sql.executeTransaction"|"Sql.executeTransactionAsync"), SynExpr.ArrayOrListOfSeqExpr(isArray, listExpr, listRange) , funcRange, appRange) ->
            match listExpr with
            | SynExpr.CompExpr(isArrayOfList, isNotNakedRefCell, compExpr, compRange) ->
                Some (readTransactionQueries compExpr)
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
                    ".uuidArray"
                    ".uuidArrayOrNone"
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

    let rec findSkipAnalysis expr =
        match expr with
        | Applied("Sql.skipAnalysis", range, appRange) ->
            [ SqlAnalyzerBlock.SkipAnalysis ]
        | SynExpr.App(exprAtomic, isInfix, funcExpr, argExpr, range) ->
            [ yield! findSkipAnalysis funcExpr; yield! findSkipAnalysis argExpr ]
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
                |> List.map (fun (name, range, func, funcRange, appRange) -> { name = name.Trim().TrimStart('@'); range = range; paramFunc = func; paramFuncRange = funcRange; applicationRange = appRange })
            [ SqlAnalyzerBlock.Parameters(sqlParameters, range) ]

        | SynExpr.App(exprAtomic, isInfix, funcExpr, argExpr, range) ->
            [ yield! findParameters funcExpr; yield! findParameters argExpr ]

        | _ ->
            [ ]

    let rec findExecuteTransaction = function
        | SqlExecuteTransaction transactionQueries ->
            [ SqlAnalyzerBlock.Transaction transactionQueries ]

        | SynExpr.App(exprAtomic, isInfix, funcExpr, argExpr, range) ->
            [
                yield! findExecuteTransaction funcExpr;
                yield! findExecuteTransaction argExpr
            ]

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
        | SynExpr.CompExpr(isArrayOrList, _, innerExpr, range) ->
            visitSyntacticExpression innerExpr range
        | SynExpr.YieldOrReturn(_, innerExpr, innerRange) ->
            visitSyntacticExpression innerExpr innerRange
        | SynExpr.YieldOrReturnFrom(_, innerExpr, innerRange) ->
            visitSyntacticExpression innerExpr innerRange
        | SynExpr.App(exprAtomic, isInfix, funcExpr, argExpr, range) ->
            match argExpr with
            | SynExpr.CompExpr(isArrayOrList, _, innerExpr, range) ->
                visitSyntacticExpression innerExpr range
            | Apply(("Sql.executeReader"|"Sql.executeReaderAsync"), lambdaExpr, _, appRange) ->
                let columns = findReadColumnAttempts lambdaExpr
                let blocks = [
                    yield! findQuery funcExpr
                    yield! findParameters funcExpr
                    yield! findFunc funcExpr
                    yield! findSkipAnalysis funcExpr
                    yield SqlAnalyzerBlock.ReadingColumns columns
                ]

                [ { blocks = blocks; range = range; } ]

            | Apply(("Sql.execute"|"Sql.executeAsync"|"Sql.executeRow"|"Sql.executeRowAsync"|"Sql.iter"|"Sql.iterAsync"), lambdaExpr, funcRange, appRange) ->
                let columns = findReadColumnAttempts lambdaExpr
                let blocks = [
                    yield! findQuery funcExpr
                    yield! findParameters funcExpr
                    yield! findFunc funcExpr
                    yield! findSkipAnalysis funcExpr
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
                    |> List.map (fun (name, range, func, funcRange, appRange) -> { name = name.Trim().TrimStart('@'); range = range; paramFunc = func; paramFuncRange = funcRange; applicationRange = appRange })

                let blocks = [
                    yield! findQuery funcExpr
                    yield! findSkipAnalysis funcExpr
                    yield SqlAnalyzerBlock.Parameters(sqlParameters, range)
                ]

                [ { blocks = blocks; range = range; } ]

            | SqlExecuteTransaction (transactionQueries) ->
                let blocks = [
                    yield! findFunc funcExpr
                    yield! findQuery funcExpr
                    yield! findParameters funcExpr
                    yield! findSkipAnalysis funcExpr
                    yield SqlAnalyzerBlock.Transaction transactionQueries
                ]

                [ { blocks = blocks; range = range; } ]

            | FuncName(functionWithoutParameters) ->
                let blocks = [
                    yield! findFunc funcExpr
                    yield! findQuery funcExpr
                    yield! findParameters funcExpr
                    yield! findSkipAnalysis funcExpr
                ]

                [ { blocks = blocks; range = range; } ]

            | Apply("Sql.skipAnalysis", functionArg, range, appRange) ->
                let blocks = [
                    yield! findFunc funcExpr
                    yield! findQuery funcExpr
                    yield! findParameters funcExpr
                    yield SqlAnalyzerBlock.SkipAnalysis
                    yield SqlAnalyzerBlock.ReadingColumns (findReadColumnAttempts funcExpr)
                ]

                [ { blocks = blocks; range = range; } ]

            | Apply(anyOtherFunction, functionArg, range, appRange) ->
                let blocks = [
                    yield! findFunc funcExpr
                    yield! findQuery funcExpr
                    yield! findParameters funcExpr
                    yield! findSkipAnalysis funcExpr
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

        | SynExpr.App(flag, _, SynExpr.Ident ident, SynExpr.CompExpr(_, _, innerExpr, innerExprRange), r) when ident.idText = "async" ->
            visitSyntacticExpression innerExpr innerExprRange

        | SynExpr.LetOrUseBang(_, isUse, isFromSource, pat, rhs, andBangs, body, range) ->
            [
                yield! visitSyntacticExpression body range
                yield! visitSyntacticExpression rhs range
            ]

        | SynExpr.IfThenElse(ifExpr, thenExpr, elseExpr, _, _ ,ifToThenRange, range) ->
            [
                yield! visitSyntacticExpression ifExpr ifToThenRange
                yield! visitSyntacticExpression thenExpr range
                match elseExpr with
                | None -> ()
                | Some expr -> yield! visitSyntacticExpression expr range
            ]

        | otherwise ->
            [ ]

    and visitBinding (binding: SynBinding) : SqlOperation list =
        match binding with
        | SynBinding.Binding (access, kind, mustInline, isMutable, attrs, xmlDecl, valData, headPat, returnInfo, expr, range, seqPoint) ->
            visitSyntacticExpression expr range

    let findLiterals (ctx: SqlAnalyzerContext) =
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

                | SqlAnalyzerBlock.Transaction queries ->
                    let modifiedQueries =
                        queries
                        |> List.map (fun transactionQuery ->
                            match literals.TryFind transactionQuery.query with
                            | Some literalQuery -> { transactionQuery with query = literalQuery }
                            | None -> transactionQuery
                        )

                    Some (SqlAnalyzerBlock.Transaction modifiedQueries)

                | differentBlock ->
                    Some differentBlock)

        { operation with blocks = modifiedBlocks }

    let findSqlOperations (ctx: SqlAnalyzerContext) =
        let operations = ResizeArray<SqlOperation>()
        match ctx.ParseTree with
        | ParsedInput.ImplFile input ->
            match input with
            | ParsedImplFileInput.ParsedImplFileInput(fileName, isScript, qualifiedName, _, _, modules, _) ->
                for parsedModule in modules do
                    match parsedModule with
                    | SynModuleOrNamespace(identifier, isRecursive, kind, declarations, _, _, _, _) ->
                        let rec iterTypeDefs defs =
                            for def in defs do
                                match def with
                                | SynTypeDefn.TypeDefn(typeInfo, typeRepr, members, range) ->
                                    for memberDefn in members do
                                        match memberDefn with
                                        | SynMemberDefn.Member (binding, _) ->
                                            operations.AddRange (visitBinding binding)
                                        | SynMemberDefn.LetBindings (bindings, _, _, _) ->
                                            for binding in bindings do
                                                operations.AddRange (visitBinding binding)
                                        | SynMemberDefn.NestedType (nestedTypeDef, _, _) ->
                                            iterTypeDefs [ nestedTypeDef ]
                                        | _ ->
                                            ()

                                    match typeRepr with
                                    | SynTypeDefnRepr.ObjectModel (modelKind, members, range) ->
                                        for memberDefn in members do
                                            match memberDefn with
                                            | SynMemberDefn.Member (binding, _) ->
                                                operations.AddRange (visitBinding binding)
                                            | SynMemberDefn.LetBindings (bindings, _, _, _) ->
                                                for binding in bindings do
                                                    operations.AddRange (visitBinding binding)
                                            | SynMemberDefn.NestedType (nestedTypeDef, _, _) ->
                                                iterTypeDefs [ nestedTypeDef ]
                                            | _ ->
                                                ()
                                    | _ ->
                                        ()

                        let rec iterDeclarations decls =
                            for declaration in decls do
                                match declaration with
                                | SynModuleDecl.Let(isRecursiveDef, bindings, range) ->
                                    for binding in bindings do
                                        operations.AddRange (visitBinding binding)
                                | SynModuleDecl.NestedModule(moduleInfo, isRecursive, nestedDeclarations, _, _) ->
                                    iterDeclarations nestedDeclarations

                                | SynModuleDecl.Types(definitions, range)  ->
                                    iterTypeDefs definitions

                                | _ ->
                                    ()

                        iterDeclarations declarations

        | ParsedInput.SigFile file ->
            ()

        let moduleLiterals = findLiterals ctx

        operations
        |> Seq.map (applyLiterals moduleLiterals)
        |> Seq.filter (fun operation -> not (List.isEmpty operation.blocks))
        |> Seq.toList
