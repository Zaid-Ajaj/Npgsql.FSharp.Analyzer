[<RequireQualifiedAccess>]
module NpgsqlFSharpParser.Parser

open FParsec

let simpleIdentifier : Parser<string, unit> =
    let isIdentifierFirstChar token = isLetter token
    let isIdentifierChar token = isLetter token || isDigit token || token = '.' || token = '_'
    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> spaces

let identifier : Parser<Expr, unit> =
    let isIdentifierFirstChar token = isLetter token
    let isIdentifierChar token = isLetter token || isDigit token || token = '.' || token = '_'
    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> spaces
    |>> Expr.Ident

let parameter : Parser<Expr, unit> =
    let isIdentifierFirstChar token = token = '@'
    let isIdentifierChar token = isLetter token || isDigit token || token = '_'
    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> spaces
    |>> Expr.Parameter

let text value : Parser<string, unit> =
    (optional spaces) >>. pstringCI value .>> (optional spaces)

let star : Parser<Expr, unit> =
    text "*" |>> fun _ -> Expr.Star

let opp = new OperatorPrecedenceParser<Expr, unit, unit>()

let expr = opp.ExpressionParser

let parens parser = between (text "(") (text ")") parser

let commaSeparatedExprs = sepBy expr (text ",")

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

let optionalDistinct =
    optional (attempt (text "DISTINCT ON") <|> attempt (text "DISTINCT"))

let optionalWhereExpr =
    ((attempt (text "WHERE" >>. expr)) |>> Some)
    <|> preturn None

let optionalFromExpr =
    ((attempt (text "FROM" >>. identifier)) |>> Some)
    <|> preturn None

let selectFromWhere =
    text "SELECT" >>= fun _ ->
    optionalDistinct >>= fun _ -> 
    selections >>= fun selections ->
    optionalFromExpr >>= fun tableName ->
    joinExpr >>= fun joinExprs -> 
    optionalWhereExpr |>> fun whereExpr ->
        let query =
            { SelectExpr.Default with
                Columns = selections
                From = tableName
                Where = whereExpr
                Joins = joinExprs }

        Expr.Query (TopLevelExpr.Select query)

opp.AddOperator(InfixOperator("AND", spaces, 7, Associativity.Left, fun left right -> Expr.And(left, right)))
opp.AddOperator(InfixOperator("OR", spaces, 7, Associativity.Left, fun left right -> Expr.Or(left, right)))
opp.AddOperator(InfixOperator("IN", spaces, 8, Associativity.Left, fun left right -> Expr.In(left, right)))
opp.AddOperator(InfixOperator(">", spaces, 9, Associativity.Left, fun left right -> Expr.GreaterThan(left, right)))
opp.AddOperator(InfixOperator("<", spaces, 9, Associativity.Left, fun left right -> Expr.LessThan(left, right)))
opp.AddOperator(InfixOperator("<=", spaces, 9, Associativity.Left, fun left right -> Expr.LessThanOrEqual(left, right)))
opp.AddOperator(InfixOperator(">=", spaces, 9, Associativity.Left, fun left right -> Expr.GreaterThanOrEqual(left, right)))
opp.AddOperator(InfixOperator("=", spaces, 9, Associativity.Left, fun left right -> Expr.Equals(left, right)))
opp.AddOperator(PostfixOperator("IS NULL", spaces, 8, false, fun value -> Expr.Equals(Expr.Null, value)))
opp.AddOperator(PostfixOperator("IS NOT NULL", spaces, 8, false, fun value -> Expr.Not(Expr.Equals(Expr.Null, value))))

opp.TermParser <- choice [
    star
    (text "(") >>. expr .>> (text ")")
    (attempt selectFromWhere)
    (attempt functionExpr)
    identifier
    parameter
]

let fullParser = (optional spaces) >>. expr .>> (optional spaces <|> (text ";" |>> fun _ -> ()))

let parse (input: string) : Result<Expr, string> = 
    match run fullParser input with
    | Success(result,_,_) -> Result.Ok result
    | Failure(errMsg,_,_) -> Result.Error errMsg
