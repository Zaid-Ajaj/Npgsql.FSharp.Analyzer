[<RequireQualifiedAccess>]
module rec NpgsqlFSharpParser.Parser

#nowarn "40" // Recursive objects

open FParsec
open System
/// https://www.postgresql.org/docs/13/sql-keywords-appendix.html
let reserved = [
    "ALL"
    "ANALYSE"
    "ANALYZE"
    "AND"
    "ANY"
    "ARRAY"
    "AS"
    "ASC"
    "ASYMMETRIC"
    "BOTH"
    "CASE"
    "CAST"
    "CHECK"
    "COLLATE"
    "COLUMN"
    "CONSTRAINT"
    "CREATE"
    "DEFAULT"
    "DESC"
    "DISTINCT"
    "DO"
    "ELSE"
    "END"
    "FALSE"
    "FOR"
    "FOREIGN"
    "FROM"
    "GROUP"
    "HAVING"
    "IN"
    "INNER"
    "INTERSECT"
    "INTO"
    "IS"
    "ISNULL"
    "JOIN"
    "LEADING"
    "LEFT"
    "LIMIT"
    "LOCALTIME"
    "LOCALTIMESTAMP"
    "NEW"
    "NOT"
    "NULL"
    "OFF"
    "OFFSET"
    "OLD"
    "ON"
    "ONLY"
    "OR"
    "ORDER"
    "OUTER"
    "OVERLAPS"
    "PLACING"
    "PRIMARY"
    "REFERENCES"
    "RIGHT"
    "SELECT"
    "SOME"
    "SYMMETRIC"
    "TABLE"
    "THEN"
    "TO"
    "TRUE"
    "UNION"
    "UNIQUE"
    "USER"
    "USING"
    "WHEN"
    "WHERE"
]

// Applies popen, then pchar repeatedly until pclose succeeds,
// returns the string in the middle
let manyCharsBetween popen pclose pchar = popen >>? manyCharsTill pchar pclose

// Parses any string between popen and pclose
let anyStringBetween popen pclose = manyCharsBetween popen pclose anyChar

// Cannot be a reserved keyword.
let unquotedIdentifier : Parser<string, unit> =
    let isIdentifierFirstChar token = isLetter token
    let isIdentifierChar token = isLetter token || isDigit token || token = '_'

    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> spacesOrComment
    >>= fun identifier ->
    if List.contains (identifier.ToUpper()) reserved
    then fail (sprintf "Identifier %s is a reserved keyword" identifier)
    else preturn identifier

// Can be a reserved keyword.
let quotedIdentifier : Parser<string, unit> =
    (skipChar '\"' |> anyStringBetween <| skipChar '\"') .>> spacesOrComment

let stringIdentifier =
    quotedIdentifier
    <|> unquotedIdentifier

let simpleIdentifier =
    attempt(
        stringIdentifier >>= fun schema ->
        text "." >>. stringIdentifier >>= fun table ->
        text "." >>. stringIdentifier >>= fun column ->
        preturn (sprintf "%s.%s.%s" schema table column))
    <|>
    attempt(
        stringIdentifier >>= fun table ->
        text "." >>. stringIdentifier >>= fun column ->
        preturn (sprintf "%s.%s" table column))
    <|>
    attempt stringIdentifier

let identifier : Parser<Expr, unit> =
    simpleIdentifier |>> Expr.Ident

let parameter : Parser<Expr, unit> =
    let isIdentifierFirstChar token = token = '@'
    let isIdentifierChar token = isLetter token || isDigit token || token = '_'
    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> spaces
    |>> Expr.Parameter

let text value : Parser<string, unit> =
    spaces >>. pstringCI value .>> spacesOrComment

let star : Parser<Expr, unit> =
    text "*" |>> fun _ -> Expr.Star

let opp = new OperatorPrecedenceParser<Expr, unit, unit>()

let expr = opp.ExpressionParser

let parens parser = between (text "(") (text ")") parser

let comma = text ","

let integer : Parser<Expr, unit> =
    spaces >>. pint32 .>> spacesOrComment
    |>> Expr.Integer

let number : Parser<Expr, unit> =
    spaces >>. pfloat .>> spacesOrComment
    |>> Expr.Float

let timestamp : Parser<Expr, unit> =
    attempt (spaces >>. (text "TIMESTAMP") >>. spacesOrComment
    >>. quotedString .>> spacesOrComment)
    |>> Expr.Timestamp

let date : Parser<Expr, unit> =
    attempt (spaces >>. (text "DATE") >>. spacesOrComment
    >>. quotedString .>> spacesOrComment)
    |>> Expr.Date

let boolean : Parser<Expr, unit> =
    (text "true" |>> fun _ -> Expr.Boolean true)
    <|> (text "false" |>> fun _ -> Expr.Boolean false)

// Parses any string between double quotes
let quotedString =
    (attempt (pstring "''") |>> fun _ -> String.Empty)
    <|> (skipChar '\'' |> anyStringBetween <| skipChar '\'')

let stringLiteral : Parser<Expr, unit> =
    quotedString .>> spacesOrComment
    |>> Expr.StringLiteral

let commaSeparatedExprs = sepBy expr comma

let selections =
    (star |>> List.singleton)
    <|> (attempt commaSeparatedExprs)
    <|> (attempt (parens commaSeparatedExprs))

let functionExpr =
    let isIdentifierFirstChar token = isLetter token
    let isIdentifierChar token = isLetter token || isDigit token || token = '.' || token = '_'
    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> spaces
    >>= fun functionName ->
    (parens commaSeparatedExprs)
    |>> fun arguments -> Expr.Function(functionName, arguments)

let innerJoin =
    (attempt (text "INNER JOIN") <|> attempt (text "JOIN")) >>. simpleIdentifier .>> text "ON" >>= fun tableName ->
    expr |>> fun expr -> JoinExpr.InnerJoin(tableName, expr)

let outerJoin =
    (text "OUTER JOIN" <|> text "FULL OUTER JOIN") >>. simpleIdentifier .>> text "ON" >>= fun tableName ->
    expr |>> fun expr -> JoinExpr.FullJoin(tableName, expr)

let leftJoin =
    (text "LEFT JOIN") >>. simpleIdentifier .>> text "ON" >>= fun tableName ->
    expr |>> fun expr -> JoinExpr.LeftJoin(tableName, expr)

let rightJoin =
    (text "RIGHT JOIN") >>. simpleIdentifier .>> text "ON" >>= fun tableName ->
    expr |>> fun expr -> JoinExpr.RightJoin(tableName, expr)

let joinExpr =
    many (
        attempt innerJoin
        <|> attempt outerJoin
        <|> attempt leftJoin
        <|> attempt rightJoin
    )

let orderByAsc =
    let parser = attempt (simpleIdentifier .>> text "ASC") <|> attempt simpleIdentifier
    parser |>> fun columnName -> Ordering.Asc columnName

let orderByAscNullsFirst =
    let parser = attempt (simpleIdentifier .>> text "ASC NULLS FIRST")
    parser |>> fun columnName -> Ordering.AscNullsFirst columnName

let orderByAscNullsLast =
    let parser = attempt (simpleIdentifier .>> text "ASC NULLS LAST")
    parser |>> fun columnName -> Ordering.AscNullsLast columnName

let orderByDesc =
    let parser = attempt (simpleIdentifier .>> text "DESC")
    parser |>> fun columnName -> Ordering.Desc columnName

let orderByDescNullsFirst =
    let parser = attempt (simpleIdentifier .>> text "DESC NULLS FIRST")
    parser |>> fun columnName -> Ordering.DescNullsFirst columnName

let orderByDescNullsLast =
    let parser = attempt (simpleIdentifier .>> text "DESC NULLS LAST")
    parser |>> fun columnName -> Ordering.DescNullsLast columnName

let orderingExpr =
    attempt orderByDescNullsLast
    <|> attempt orderByDescNullsFirst
    <|> attempt orderByAscNullsLast
    <|> attempt orderByAscNullsFirst
    <|> attempt orderByDesc
    <|> attempt orderByAsc

let optionalExpr parser =
    (attempt parser |>> Some) <|> preturn None

let optionalOrderingExpr =
    optionalExpr (text "ORDER BY" >>. (sepBy1 orderingExpr comma))
    |>> function
        | Some exprs -> exprs
        | None -> [ ]

let optionalRetuningExpr =
    optionalExpr (text "RETURNING " >>. selections)
    |>> function
        | Some exprs -> exprs
        | None -> [ ]

let optionalDistinct =
    optional (attempt (text "DISTINCT ON") <|> attempt (text "DISTINCT"))

let commaSeparatedIdentifiers = sepBy1 identifier comma

let optionalWhereClause = optionalExpr (text "WHERE" >>. expr)

let optionalHavingClause = optionalExpr (text "HAVING" >>. expr)

let optionalScope =
    optionalExpr (
        (text "LOCAL" |>> fun _ -> Local)
        <|>
        (text "SESSION" |>> fun _ -> Session)
    )

let optionalFrom =
    optionalExpr (
        attempt (
            text "FROM" >>. (parens selectQuery) >>= fun subQuery ->
            optional (text "AS") >>= fun _ ->
            simpleIdentifier >>= fun alias ->
            preturn (Expr.As(subQuery, Expr.Ident alias))
        )
        <|>
        attempt (
            text "FROM" >>. simpleIdentifier >>= fun table ->
            optional (text "AS") >>= fun _ ->
            simpleIdentifier >>= fun alias ->
            preturn (Expr.As(Expr.Ident table, Expr.Ident alias))
        )
        <|>
        attempt (
            text "FROM" >>. (parens selectQuery) >>= fun subQuery ->
            preturn subQuery
        )
        <|>
        attempt (
            text "FROM" >>. identifier
        )
    )

let optionalLimit = optionalExpr (text "LIMIT" >>. expr)

let optionalOffset = optionalExpr (text "OFFSET" >>. expr)

let optionalGroupBy =
    optionalExpr (text "GROUP BY" >>. commaSeparatedIdentifiers)
    |>> function
        | Some exprs -> exprs
        | None -> [ ]

let selectQuery =
    text "SELECT" >>= fun _ ->
    optionalDistinct >>= fun _ ->
    selections >>= fun selections ->
    optionalFrom >>= fun tableName ->
    joinExpr >>= fun joinExprs ->
    optionalWhereClause >>= fun whereExpr ->
    optionalGroupBy >>= fun groupByExpr ->
    optionalHavingClause >>= fun havingExpr ->
    optionalOrderingExpr >>= fun orderingExprs ->
    optionalLimit >>= fun limitExpr ->
    optionalOffset >>= fun offsetExpr ->
        let query =
            { SelectExpr.Default with
                Columns = selections
                From = tableName
                Where = whereExpr
                Joins = joinExprs
                GroupBy = groupByExpr
                Having = havingExpr
                OrderBy = orderingExprs
                Limit = limitExpr
                Offset = offsetExpr }

        preturn (Expr.SelectQuery query)

let deleteQuery =
    text "DELETE FROM " >>. simpleIdentifier >>= fun tableName ->
    optionalWhereClause >>= fun where ->
    optionalRetuningExpr >>= fun returningExpr ->
        let query = {
            DeleteExpr.Default with
                Table = tableName
                Where = where
                Returning = returningExpr
        }

        preturn (Expr.DeleteQuery query)

let insertQuery =
    text "INSERT INTO " >>. simpleIdentifier >>= fun tableName ->
    (parens (sepBy1 simpleIdentifier comma)) >>= fun columns ->
    text "VALUES" >>= fun _ ->
    (parens (sepBy1 expr comma)) >>= fun values ->
    optionalRetuningExpr >>= fun returningExpr ->
        let query = {
            InsertExpr.Default with
                Table = tableName
                Columns = columns
                Values = values
                Returning = returningExpr
        }

        preturn (Expr.InsertQuery query)

let updateQuery =
    text "UPDATE " >>. simpleIdentifier >>= fun tableName ->
    text "SET " >>= fun _ ->
    (sepBy1 expr comma) >>= fun assignments ->
    optionalWhereClause >>= fun whereExpr ->
    optionalRetuningExpr >>= fun returningExpr ->
        let query = {
            UpdateExpr.Default with
                Table = tableName
                Where = whereExpr
                Returning = returningExpr
                Assignments = assignments
        }

        preturn (Expr.UpdateQuery query)


let toOrEquals =
    text "=" <|> text "TO"



// TODO: SET TIME ZONE value is an alias for SET timezone TO value
let setQuery =
    text "SET" >>.
    optionalScope >>= fun scope ->
    simpleIdentifier >>= fun parameter ->
    toOrEquals >>= fun _ ->
    expr >>= fun value ->
        let query = {
            SetExpr.Default with
                Parameter = parameter
                Scope = defaultArg scope Session
                Value = Some value
        }

        preturn (Expr.SetQuery query)

let declareQuery =
    text "DECLARE" >>.
    simpleIdentifier >>= fun parameter ->
    text "CURSOR FOR" >>.
    expr >>= fun query ->
        let query = {
            Parameter = parameter
            Query = query
        }
        preturn (Expr.DeclareQuery (Cursor query))

let spacesOrComment =
    let comment = skipString "/*" >>. (charsTillString "*/" true 8096)
    let commentEol = skipString "--" >>. skipRestOfLine true

    spaces .>>
    optional comment .>>
    optional commentEol .>>
    spaces

let stringOrFail = function
    | Expr.StringLiteral(value) -> value
    | _ -> failwith "not a string"

opp.AddOperator(InfixOperator("AND", spacesOrComment, 7, Associativity.Left, fun left right -> Expr.And(left, right)))
opp.AddOperator(InfixOperator("AS", spacesOrComment, 6, Associativity.Left, fun left right -> Expr.As(left, right)))
opp.AddOperator(InfixOperator("as", spacesOrComment, 6, Associativity.Left, fun left right -> Expr.As(left, right)))
opp.AddOperator(InfixOperator("OR", notFollowedBy (text "DER BY"), 6, Associativity.Left, fun left right -> Expr.Or(left, right)))
opp.AddOperator(InfixOperator("IN", spacesOrComment, 8, Associativity.Left, fun left right -> Expr.In(left, right)))
opp.AddOperator(InfixOperator(">", spaces, 9, Associativity.Left, fun left right -> Expr.GreaterThan(left, right)))
opp.AddOperator(InfixOperator("<", spaces, 9, Associativity.Left, fun left right -> Expr.LessThan(left, right)))
opp.AddOperator(InfixOperator("<=", spaces, 9, Associativity.Left, fun left right -> Expr.LessThanOrEqual(left, right)))
opp.AddOperator(InfixOperator(">=", spaces, 9, Associativity.Left, fun left right -> Expr.GreaterThanOrEqual(left, right)))
opp.AddOperator(InfixOperator("=", spaces, 9, Associativity.Left, fun left right -> Expr.Equals(left, right)))
opp.AddOperator(InfixOperator("<>", spaces, 9, Associativity.Left, fun left right -> Expr.Not(Expr.Equals(left, right))))
opp.AddOperator(InfixOperator("||", spaces, 9, Associativity.Left, fun left right -> Expr.StringConcat(left, right)))
opp.AddOperator(InfixOperator("::", spaces, 9, Associativity.Left, fun left right -> Expr.TypeCast(left, right)))
opp.AddOperator(InfixOperator("->>", spaces, 9, Associativity.Left, fun left right -> Expr.JsonIndex(left, right)))

opp.AddOperator(PostfixOperator("IS NULL", spacesOrComment, 8, false, fun value -> Expr.Equals(Expr.Null, value)))
opp.AddOperator(PostfixOperator("IS NOT NULL", spacesOrComment, 8, false, fun value -> Expr.Not(Expr.Equals(Expr.Null, value))))

opp.TermParser <- choice [
    (attempt updateQuery)
    (attempt insertQuery)
    (attempt deleteQuery)
    (attempt selectQuery)
    (attempt setQuery)
    (attempt declareQuery)
    (attempt functionExpr)
    (text "(") >>. expr .>> (text ")")
    star
    integer
    boolean
    number
    date
    timestamp
    stringLiteral
    identifier
    parameter
]

let fullParser = spacesOrComment >>. expr .>> (spacesOrComment <|> (text ";" |>> ignore))

let parse (input: string) : Result<Expr, string> =
    match run fullParser input with
    | Success(result,_,_) -> Result.Ok result
    | Failure(errMsg,_,_) -> Result.Error errMsg

let parseUnsafe query =
    match parse query with
    | Result.Ok output -> output
    | Result.Error errorMsg -> failwith errorMsg
