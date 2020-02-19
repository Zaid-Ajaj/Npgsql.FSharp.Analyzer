namespace Npgsql.FSharp.Analyzers

open FSharp.Analyzers.SDK
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Ast
open FSharp.Compiler.Range

module SyntacticAnalysis =

    let readRange (r: range) = r

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
        | SynExpr.App(atomicFlag, isInfix, funcExpr, argExpr, range) ->
            match funcExpr with
            | SynExpr.Ident ident -> Some (ident.idText, argExpr, range)
            | SynExpr.LongIdent(isOptional, longDotId, altName, range) ->
                match longDotId with
                | LongIdentWithDots(listOfIds, ranges) ->
                    let fullName =
                        listOfIds
                        |> List.map (fun id -> id.idText)
                        |> String.concat "."
                         
                    Some (fullName, argExpr, range)
            | _ -> None
        | _ -> None 

    let (|ParameterTuple|_|) = function
        | SynExpr.Tuple(isStruct, [ SynExpr.Const(SynConst.String(parameterName, paramRange), constRange); secondItem ], commaRange, tupleRange) ->
            Some (parameterName, readRange paramRange)
        | _ ->
            None

    let rec readParameters = function
        | ParameterTuple (name, range) ->
            [ name, range ]
        | SynExpr.Sequential(SequencePointInfoForSeq.SequencePointsAtSeq, isTrueSeq, expr1, expr2, seqRange) ->
            [ yield! readParameters expr1; yield! readParameters expr2 ]
        | _ ->
            [ ]

    let (|SqlParameters|_|) = function
        | Apply ("Sql.parameters", SynExpr.ArrayOrListOfSeqExpr(isArray, listExpr, listRange) , range) ->
            match listExpr with
            | SynExpr.CompExpr(isArrayOfList, isNotNakedRefCell, compExpr, compRange) ->
                Some (readParameters compExpr, readRange compRange)
            | _ ->
                None
        | _ ->
            None

    let (|ReadColumnAttempt|_|) = function
        | Apply(funcName, SynExpr.Const(SynConst.String(columnName, queryRange), constRange), range) ->
            if funcName.StartsWith "Sql.read" && funcName <> "Sql.readRow"
            then Some {
                funcName = funcName
                columnName = columnName
                columnNameRange = constRange
                funcCallRange = range }
            else
                None
        | _ ->
           None

    /// Detects `Sql.query {SQL}` pattern
    let (|SqlQuery|_|) = function
        | Apply("Sql.query", SynExpr.Const(SynConst.String(query, queryRange), constRange), range) ->
            Some (query, constRange)
        | _ ->
            None

    let (|LiteralQuery|_|) = function
        | Apply("Sql.query", SynExpr.Ident(identifier), range) ->
            Some (identifier.idText, range)
        | _ ->
            None

    let (|SqlStoredProcedure|_|) = function
        | Apply("Sql.func", SynExpr.Const(SynConst.String(funcName, funcNameRange), constRange), range) ->
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
                |> List.map (fun (name, range) -> { parameter = name; range = range })
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
             [ yield! findReadColumnAttempts body ]
        | SynExpr.LetOrUseBang(sequencePoint, isUse, isFromSource, syntaxPattern, expr1, expr2, range) ->
            [ yield! findReadColumnAttempts expr1; yield! findReadColumnAttempts expr2 ]
        | SynExpr.CompExpr(isArray, _, expression, range) ->
            [ yield! findReadColumnAttempts expression ]
        | _ ->
            [ ]

    let rec visitSyntacticExpression (expr: SynExpr) (fullExpressionRange: range) =
        match expr with
        | SynExpr.App(exprAtomic, isInfix, funcExpr, argExpr, range) ->
            match argExpr with
            | Apply(("Sql.executeReader"|"Sql.executeReaderAsync"), lambdaExpr, _) ->
                let columns = findReadColumnAttempts lambdaExpr
                let blocks = [
                    yield! findQuery funcExpr
                    yield! findParameters funcExpr
                    yield! findFunc funcExpr
                    yield SqlAnalyzerBlock.ReadingColumns columns
                ]

                [ { blocks = blocks; range = range; fileName = "" } ]

            | SqlQuery(query, queryRange) ->
                
                let blocks = [
                    SqlAnalyzerBlock.Query(query, queryRange)
                ]

                [ { blocks = blocks; range = range; fileName = "" } ]

            | LiteralQuery(identifier, queryRange) ->
                let blocks = [
                    SqlAnalyzerBlock.LiteralQuery(identifier, queryRange)
                ]

                [ { blocks = blocks; range = range; fileName = "" } ]

            | SqlParameters(parameters, range) ->
                let sqlParameters =
                    parameters
                    |> List.map (fun (name, range) -> { parameter = name; range = range })

                let blocks = [
                    yield! findQuery funcExpr
                    yield SqlAnalyzerBlock.Parameters(sqlParameters, range)
                ]

                [ { blocks = blocks; range = range; fileName = "" } ]

            | FuncName(functionWithoutParameters) ->
                let blocks = [
                    yield! findFunc funcExpr
                    yield! findQuery funcExpr
                    yield! findParameters funcExpr
                ]

                [ { blocks = blocks; range = range; fileName = "" } ]

            | Apply(anyOtherFunction, functionArg, range) ->
                let blocks = [
                    yield! findFunc funcExpr
                    yield! findQuery funcExpr
                    yield! findParameters funcExpr
                    yield SqlAnalyzerBlock.ReadingColumns (findReadColumnAttempts funcExpr)
                ]

                [ { blocks = blocks; range = range; fileName = "" } ]
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

    let findSqlBlocks (ctx: Context) =
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
                                    visitBinding binding
                                    |> List.map (fun block -> { block with fileName = ctx.FileName })
                                    |> operations.AddRange
                            | _ ->
                                ()

        | ParsedInput.SigFile file ->
            ()

        let moduleLiterals = findLiterals ctx
        operations
        |> Seq.map (applyLiterals moduleLiterals)
        |> Seq.toList
