namespace rec NpgsqlFSharpParser

[<RequireQualifiedAccess>]
type TopLevelExpr =
    | Select of SelectExpr
    | Insert of InsertExpr
    | Delete of DeleteExpr
    | Update of UpdateExpr

[<RequireQualifiedAccess>]
type Expr =
    | Star
    | Ident of string
    | Parameter of string
    | Boolean of bool
    | Integer of int
    | Float of float
    | Null
    | Function of name:string * arguments:Expr list
    | And of left:Expr * right:Expr
    | Or of left:Expr * right:Expr
    | In of left:Expr * right:Expr
    | Not of expr:Expr
    | Equals of left:Expr * right:Expr
    | GreaterThan of left:Expr * right:Expr
    | LessThan of left:Expr * right:Expr
    | GreaterThanOrEqual of left:Expr * right:Expr
    | LessThanOrEqual of left:Expr * right:Expr
    | Between of value:Expr * leftBound:Expr * rightBound:Expr
    | Query of expr:TopLevelExpr

type Ordering = {
    Column : Expr
    ASC : bool
    DESC : bool
    NullFirst : bool
    NullLast : bool
}

type JoinExpr =
    | InnerJoin of tableName:string * on:Expr
    | LeftJoin of tableName:string * on:Expr
    | RightJoin of tableName:string * on:Expr
    | FullJoin of tableName:string * on:Expr

type SelectExpr = {
    Columns : Expr list
    From : Expr option
    Joins : JoinExpr list
    Where : Expr option
    OrderBy : Ordering list
    GroupBy : Expr list
}
  with
    static member Default = {
        Columns  = [ ]
        From = None
        Where = None
        OrderBy = [ ]
        GroupBy = [ ]
        Joins = [ ]
    }

type UpdateExpr = {
    Table : string
    Assignments : Map<string, Expr list>
    ConflictResolution : Map<string, Expr list>
    Returning : Expr list
}

type DeleteExpr = {
    Table : string
    Where : Expr option
}

type InsertExpr = {
    Table: string
    Columns : string list
    Values : Expr list
    ConflictResolution : Map<string, Expr list>
    Returning : Expr list
}
