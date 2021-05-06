namespace rec NpgsqlFSharpParser

[<RequireQualifiedAccess>]
type Expr =
    | Null
    | Star
    | Ident of string
    | Parameter of string
    | Boolean of bool
    | StringLiteral of string
    | Integer of int
    | Float of float
    | Date of string
    | Timestamp of string
    | Function of name:string * arguments:Expr list
    | And of left:Expr * right:Expr
    | Or of left:Expr * right:Expr
    | In of left:Expr * right:Expr
    | As of left:Expr * right:Expr
    | StringConcat of left:Expr * right:Expr
    | JsonIndex of left:Expr * right:Expr
    | TypeCast of left:Expr * right:Expr
    | Not of expr:Expr
    | Equals of left:Expr * right:Expr
    | GreaterThan of left:Expr * right:Expr
    | LessThan of left:Expr * right:Expr
    | GreaterThanOrEqual of left:Expr * right:Expr
    | LessThanOrEqual of left:Expr * right:Expr
    | Between of value:Expr * leftBound:Expr * rightBound:Expr
    | SelectQuery of expr:SelectExpr
    | DeleteQuery of expr:DeleteExpr
    | InsertQuery of expr: InsertExpr
    | UpdateQuery of expr: UpdateExpr
    | SetQuery of expr: SetExpr
    | DeclareQuery of expr: DeclareExpr

type Ordering =
    | Asc of columnName:string
    | Desc of columnName:string
    | AscNullsFirst of columnName:string
    | AscNullsLast of columnName:string
    | DescNullsFirst of columnName:string
    | DescNullsLast of columnName:string

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
    Having : Expr option
    Limit : Expr option
    Offset : Expr option
}
  with
    static member Default = {
        Columns  = [ ]
        From = None
        Where = None
        Having = None
        Limit = None
        Offset = None
        OrderBy = [ ]
        GroupBy = [ ]
        Joins = [ ]
    }

type UpdateExpr = {
    Table : string
    Where : Expr option
    Assignments : Expr list
    ConflictResolution : Expr list
    Returning : Expr list
} with
    static member Default = {
        Table = ""
        Where = None
        Returning = [ ]
        Assignments = [ ]
        ConflictResolution = [ ]
    }

type DeleteExpr = {
    Table : string
    Where : Expr option
    Returning : Expr list
} with
    static member Default =
        {
            Table = "";
            Where = None
            Returning = [ ]
        }

type InsertExpr = {
    Table: string
    Columns : string list
    Values : Expr list
    ConflictResolution : (string * Expr) list
    Returning : Expr list
} with
    static member Default =
        {
            Table = "";
            Columns = [ ]
            Values = [ ]
            ConflictResolution = [ ]
            Returning = [ ]
        }

type Scope =
    | Local
    | Session

type SetExpr = {
    Parameter: string
    Value: Expr option
    Scope: Scope
} with
    static member Default =
        {
            Parameter = "";
            Value = None
            Scope = Session
        }

type CursorDeclaration = {
    Parameter: string
    Query: Expr
} with
    static member Default =
        {
            Parameter = "";
            Query = Expr.Null
        }

type DeclareExpr =
    | Cursor of CursorDeclaration
