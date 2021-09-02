namespace rec NpgsqlFSharpParser

[<RequireQualifiedAccess>]
type Expr =
    | Array of Expr list
    | List of Expr list
    | Null
    | Star
    | Ident of string
    | Parameter of string
    | Boolean of bool
    | StringLiteral of string
    | Integer of int64
    | Float of float
    | Date of string
    | Timestamp of string
    | Function of name:string * arguments:Expr list
    | Like of left:Expr * right:Expr
    | And of left:Expr * right:Expr
    | Or of left:Expr * right:Expr
    | In of left:Expr * right:Expr
    | As of left:Expr * right:Expr
    | StringConcat of left:Expr * right:Expr
    | JsonIndex of left:Expr * right:Expr
    | TypeCast of left:Expr * right:Expr
    | DataType of DataType
    | Not of expr:Expr
    | Any of expr:Expr
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
    | FetchQuery of expr: FetchExpr

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

[<RequireQualifiedAccess>]
type Direction =
    /// Fetch next row. Same as Forward
    | Next
    /// Fetch prior row. Same as Backward
    | Prior
    | Absolute of int // First = Absolute 1, Last = Absolute -1
    | Relative of int
    | Forward of int // Same as count
    | Backward of int
    /// Fetch all remaining rows. Same as ForwardAll
    | All
    | BackwardAll

type FetchExpr = {
    // An open cursor name.
    CursorName: string
    // Defines the fetch direction.
    Direction: Direction
} with
    static member Default =
        {
            CursorName = ""
            Direction = Direction.Next
        }

[<RequireQualifiedAccess>]
type DataType =
    | Integer
    | BigInt
    | SmallInt
    | Real
    | Double
    | Array of dataType:DataType * size:int option

    static member TryFromString(valueType: string, ?isArray: bool, ?arraySize: int) : DataType option =
        let dType =
            match valueType.ToUpper () with
            | "INT2"
            | "SMALLINT" -> Some SmallInt
            | "INT"
            | "INT4"
            | "INTEGER" -> Some Integer
            | "INT8"
            | "BIGINT" -> Some BigInt
            | "FLOAT4"
            | "REAL" -> Some Real
            | "FLOAT8"
            | "DOUBLE PRECISION" -> Some Double
            | _ -> None

        let isArray = isArray |> Option.bind (function | false -> None | _ -> Some true) // Note: Some false -> None.

        dType
        |> Option.bind (fun t -> isArray |> Option.map (fun _ -> Array(dataType=t, size=arraySize)))
        |> Option.orElse dType
