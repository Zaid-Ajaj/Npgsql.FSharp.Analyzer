namespace Npgsql.FSharp.Analyzers

open System
open System.Data
open System.Data.Common
open System.Collections.Generic

open FSharp.Quotations

open Npgsql
open Npgsql.PostgresTypes
open NpgsqlTypes

open System.Collections
open System.Net

module InformationSchema =
    type internal NpgsqlDataReader with

        member cursor.GetValueOrDefault(name: string, defaultValue) =    
            let i = cursor.GetOrdinal(name)
            if cursor.IsDBNull( i) then defaultValue else cursor.GetFieldValue( i)

    type internal Type with
        member this.PartiallyQualifiedName = 
            sprintf "%s, %s" this.FullName (this.Assembly.GetName().Name)

    //https://www.postgresql.org/docs/current/static/datatype.html#DATATYPE-TABLE
    let private builtins = [
        "boolean", typeof<bool>; "bool", typeof<bool>

        "smallint", typeof<int16>; "int2", typeof<int16>
        "integer", typeof<int32>; "int", typeof<int32>; "int4", typeof<int32>
        "bigint", typeof<int64>; "int8", typeof<int64>

        "real", typeof<single>; "float4", typeof<single>
        "double precision", typeof<double>; "float8", typeof<double>

        "numeric", typeof<decimal>; "decimal", typeof<decimal>
        "money", typeof<decimal>
        "text", typeof<string>

        "character varying", typeof<string>; "varchar", typeof<string>
        "character", typeof<string>; "char", typeof<string>

        "citext", typeof<string>
        "jsonb", typeof<string>
        "json", typeof<string>
        "xml", typeof<string>
        "point", typeof<NpgsqlPoint>
        "lseg", typeof<NpgsqlLSeg>
        "path", typeof<NpgsqlPath>
        "polygon", typeof<NpgsqlPolygon>
        "line", typeof<NpgsqlLine>
        "circle", typeof<NpgsqlCircle>
        "box", typeof<bool>

        "bit", typeof<BitArray>; "bit(n)", typeof<BitArray>; "bit varying", typeof<BitArray>; "varbit", typeof<BitArray>

        "hstore", typeof<IDictionary>
        "uuid", typeof<Guid>
        "cidr", typeof<ValueTuple<IPAddress, int>>
        "inet", typeof<IPAddress>
        "macaddr", typeof<NetworkInformation.PhysicalAddress>
        "tsquery", typeof<NpgsqlTsQuery>
        "tsvector", typeof<NpgsqlTsVector>

        "date", typeof<DateTime>
        "interval", typeof<TimeSpan>
        "timestamp without time zone", typeof<DateTime>; "timestamp", typeof<DateTime>   
        "timestamp with time zone", typeof<DateTime>; "timestamptz", typeof<DateTime>
        "time without time zone", typeof<TimeSpan>; "time", typeof<TimeSpan>
        "time with time zone", typeof<DateTimeOffset>; "timetz", typeof<DateTimeOffset>

        "bytea", typeof<byte[]>
        "oid", typeof<UInt32>
        "xid", typeof<UInt32>
        "cid", typeof<UInt32>
        "oidvector", typeof<UInt32[]>
        "name", typeof<string>
        "char", typeof<string>
        //"range", typeof<NpgsqlRange>, NpgsqlDbType.Range)
    ]

    let getTypeMapping = 
        let allMappings = dict builtins
        fun datatype -> 
            let exists, value = allMappings.TryGetValue(datatype)
            if exists then value else failwithf "Unsupported datatype %s." datatype 

    type PostgresType with    
        member this.ToClrType() = 
            match this with
            | :? PostgresBaseType as x -> 
                getTypeMapping x.Name
            | :? PostgresEnumType ->
                typeof<string>
            | :? PostgresDomainType as x -> 
                x.BaseType.ToClrType()
            | :? PostgresTypes.PostgresArrayType as arr ->
                arr.Element.ToClrType().MakeArrayType()
            | _ -> 
                typeof<obj>
        
    type DataType = {
        Name: string
        Schema: string
        ClrType: Type
    }   with    
        member this.FullName = sprintf "%s.%s" this.Schema this.Name
        member this.IsUserDefinedType = this.Schema <> "pg_catalog"
        member this.IsFixedLength = this.ClrType.IsValueType
        member this.UdtTypeName = 
            if this.ClrType.IsArray 
            then
                let withoutTrailingBrackets = this.Name.Substring(0, this.Name.Length - 2) // my_enum[] -> my_enum
                sprintf "%s.%s" this.Schema withoutTrailingBrackets
            else this.FullName

        static member Create(x: PostgresTypes.PostgresType) = 
            { 
                Name = x.Name
                Schema = x.Namespace
                ClrType = x.ToClrType()
            }

    type Schema =
        { OID : string
          Name : string }
    
    type Table =
        { OID : string
          Name : string
          Description : string option }
    
    type Column =
        { ColumnAttributeNumber : int16
          Name: string
          DataType: DataType
          Nullable: bool
          MaxLength: int
          ReadOnly: bool
          AutoIncrement: bool
          DefaultConstraint: string
          Description: string
          UDT: Type option Lazy // must be lazt due to late binding of columns with user defined types.
          PartOfPrimaryKey: bool
          BaseSchemaName: string
          BaseTableName: string }
        with

        member this.ClrType = this.DataType.ClrType

        member this.ClrTypeConsideringNullability = 
            if this.Nullable
            then typedefof<_ option>.MakeGenericType this.DataType.ClrType
            else this.DataType.ClrType

        member this.HasDefaultConstraint = string this.DefaultConstraint <> ""
        member this.OptionalForInsert = this.Nullable || this.HasDefaultConstraint || this.AutoIncrement

    type DbEnum =
        { Name: string
          Values: string list }

    type DbSchemaLookupItem =
        { Schema : Schema
          Tables : Dictionary<Table, HashSet<Column>>
          Enums : Map<string, DbEnum> }
    
    type ColumnLookupKey = { TableOID : string; ColumnAttributeNumber : int16 }
    
    type DbSchemaLookups =
        { Schemas : Dictionary<string, DbSchemaLookupItem>
          Columns : Dictionary<ColumnLookupKey, Column>
          Enums : Map<string, DbEnum> }
    
    type Parameter =
        { Name: string
          NpgsqlDbType: NpgsqlTypes.NpgsqlDbType
          Direction: ParameterDirection 
          MaxLength: int
          Precision: byte
          Scale : byte
          Optional: bool
          DataType: DataType }
        with
   
        member this.Size = this.MaxLength

    let inline openConnection connectionString =  
        let conn = new NpgsqlConnection(connectionString)
        conn.Open()
        conn

    let extractParametersAndOutputColumns(connectionString, commandText, allParametersOptional, dbSchemaLookups : DbSchemaLookups) =
        use conn = openConnection(connectionString)
    
        use cmd = new NpgsqlCommand(commandText, conn)
        NpgsqlCommandBuilder.DeriveParameters(cmd)
        for p in cmd.Parameters do p.Value <- DBNull.Value
        let cols = 
            use cursor = cmd.ExecuteReader(CommandBehavior.SchemaOnly)
            if cursor.FieldCount = 0 then [] else [ for c in cursor.GetColumnSchema() -> c ]
    
        let outputColumns =
            [ for column in cols ->
                let columnAttributeNumber = column.ColumnAttributeNumber.GetValueOrDefault(-1s)
            
                if column.TableOID <> 0u then
                    let lookupKey = { TableOID = string column.TableOID
                                      ColumnAttributeNumber = columnAttributeNumber }
                    { dbSchemaLookups.Columns.[lookupKey] with Name = column.ColumnName }
                else
                    let dataType = DataType.Create(column.PostgresType)

                    {
                        ColumnAttributeNumber = columnAttributeNumber
                        Name = column.ColumnName
                        DataType = dataType
                        Nullable = column.AllowDBNull.GetValueOrDefault(true)
                        MaxLength = column.ColumnSize.GetValueOrDefault(-1)
                        ReadOnly = true
                        AutoIncrement = column.IsIdentity.GetValueOrDefault(false)
                        DefaultConstraint = column.DefaultValue
                        Description = ""
                        UDT = lazy None
                        PartOfPrimaryKey = column.IsKey.GetValueOrDefault(false)
                        BaseSchemaName = column.BaseSchemaName
                        BaseTableName = column.BaseTableName
                    }  ]


        let parameters = 
            [ for p in cmd.Parameters ->
                assert (p.Direction = ParameterDirection.Input)
                { Name = p.ParameterName
                  NpgsqlDbType = 
                    match p.PostgresType with
                    | :? PostgresArrayType as x when (x.Element :? PostgresEnumType) -> 
                        //probably array of custom type (enum or composite)
                        NpgsqlDbType.Array ||| NpgsqlDbType.Text
                    | _ -> p.NpgsqlDbType
                  Direction = p.Direction
                  MaxLength = p.Size
                  Precision = p.Precision
                  Scale = p.Scale
                  Optional = allParametersOptional 
                  DataType = DataType.Create(p.PostgresType) } ]
    
        let enums =  
            outputColumns 
            |> Seq.choose (fun c ->
                if c.DataType.IsUserDefinedType && dbSchemaLookups.Enums.ContainsKey(c.DataType.UdtTypeName) then
                    Some (c.DataType.UdtTypeName, dbSchemaLookups.Enums.[c.DataType.UdtTypeName])
                else
                    None)
            |> Seq.append [ 
                for p in parameters do
                    if p.DataType.IsUserDefinedType && dbSchemaLookups.Enums.ContainsKey(p.DataType.UdtTypeName)
                    then 
                        yield p.DataType.UdtTypeName, dbSchemaLookups.Enums.[p.DataType.UdtTypeName]
            ]
            |> Seq.distinct
            |> Map.ofSeq

        parameters, outputColumns, enums

    let getDbSchemaLookups(connectionString) =
        use conn = openConnection(connectionString)
    
        use cmd = conn.CreateCommand()
        cmd.CommandText <- """
            SELECT
              n.nspname              AS schema,
              t.typname              AS name,
              array_agg(e.enumlabel) AS values
            FROM pg_type t
              JOIN pg_enum e ON t.oid = e.enumtypid
              JOIN pg_catalog.pg_namespace n ON n.oid = t.typnamespace
            GROUP BY
              schema, name
        """
    
        let enumsLookup =
            [
                use cursor = cmd.ExecuteReader()
                while cursor.Read() do
                    let schema = cursor.GetString(0)
                    let name = cursor.GetString(1)
                    let values: string[] = cursor.GetValue(2) :?> _
                    let t = { Name = name; Values = List.ofArray values }
                    yield schema, name, t
            ]
            |> Seq.groupBy (fun (schema, _, _) -> schema)
            |> Seq.map (fun (schema, types) ->
                schema, types |> Seq.map (fun (_, name, t) -> name, t) |> Map.ofSeq
            )
            |> Map.ofSeq
    
        //https://stackoverflow.com/questions/12445608/psql-list-all-tables#12455382
        use cmd = conn.CreateCommand()
        cmd.CommandText <- """
            SELECT
                 ns.OID AS schema_oid,
                 ns.nspname AS schema_name,
                 attr.attrelid AS table_oid,
                 cls.relname AS table_name,
                 pg_catalog.obj_description(attr.attrelid) AS table_description,
                 attr.attnum AS col_number,
                 attr.attname AS col_name,
                 col.udt_name AS col_udt_name,
                 col.data_type AS col_data_type,
                 attr.attnotnull AS col_not_null,
                 col.character_maximum_length AS col_max_length,
                 CASE WHEN col.is_updatable = 'YES' THEN true ELSE false END AS col_is_updatable,
                 CASE WHEN col.is_identity = 'YES' THEN true else false END AS col_is_identity,
                 CASE WHEN attr.atthasdef THEN (SELECT pg_get_expr(adbin, cls.oid) FROM pg_attrdef WHERE adrelid = cls.oid AND adnum = attr.attnum) ELSE NULL END AS col_default,
                 pg_catalog.col_description(attr.attrelid, attr.attnum) AS col_description,
                 typ.oid AS col_typoid,
                 EXISTS (
                   SELECT * FROM pg_index
                   WHERE pg_index.indrelid = cls.oid AND
                         pg_index.indisprimary AND
                         attnum = ANY (indkey)
                 ) AS col_part_of_primary_key

            FROM pg_class AS cls
            INNER JOIN pg_namespace AS ns ON ns.oid = cls.relnamespace

            LEFT JOIN pg_attribute AS attr ON attr.attrelid = cls.oid AND attr.atttypid <> 0 AND attr.attnum > 0 AND NOT attr.attisdropped
            LEFT JOIN pg_type AS typ ON typ.oid = attr.atttypid
            LEFT JOIN information_schema.columns AS col ON col.table_schema = ns.nspname AND
               col.table_name = relname AND
               col.column_name = attname
            WHERE
               cls.relkind IN ('r', 'v', 'm') AND
               ns.nspname !~ '^pg_' AND
               ns.nspname <> 'information_schema'
            ORDER BY nspname, relname;
        """
    
        let schemas = Dictionary<string, DbSchemaLookupItem>()
        let columns = Dictionary<ColumnLookupKey, Column>()
    
        use row = cmd.ExecuteReader()
        while row.Read() do
            let schema : Schema =
                { OID = string row.["schema_oid"]
                  Name = string row.["schema_name"] }
        
            if not <| schemas.ContainsKey(schema.Name) then
            
                schemas.Add(schema.Name, { Schema = schema
                                           Tables = Dictionary();
                                           Enums = enumsLookup.TryFind schema.Name |> Option.defaultValue Map.empty })
        
            match row.["table_oid"] |> Option.ofObj with
            | None -> ()
            | Some oid ->
                let table =
                    { OID = string oid
                      Name = string row.["table_name"]
                      Description = row.["table_description"] |> Option.ofObj |> Option.map string }
            
                if not <| schemas.[schema.Name].Tables.ContainsKey(table) then
                    schemas.[schema.Name].Tables.Add(table, HashSet())
            
                match row.GetValueOrDefault("col_number", -1s) with
                | -1s -> ()
                | attnum ->
                    let udtName = string row.["col_udt_name"]
                    let isUdt =
                        schemas.[schema.Name].Enums
                        |> Map.tryFind udtName
                        |> Option.isSome
                
                    let clrType =
                        match string row.["col_data_type"] with
                        | "ARRAY" ->
                            let elemType = getTypeMapping(udtName.TrimStart('_') ) 
                            elemType.MakeArrayType()
                        | "USER-DEFINED" ->
                            if isUdt then typeof<string> else typeof<obj>
                        | dataType -> 
                            getTypeMapping(dataType)
                
                    let column =
                        { ColumnAttributeNumber = attnum
                          Name = string row.["col_name"]
                          DataType = { Name = udtName
                                       Schema = schema.Name
                                       ClrType = clrType }
                          Nullable = row.["col_not_null"] |> unbox |> not
                          MaxLength = row.GetValueOrDefault("col_max_length", -1)
                          ReadOnly = row.["col_is_updatable"] |> unbox |> not
                          AutoIncrement = unbox row.["col_is_identity"]
                          DefaultConstraint = row.GetValueOrDefault("col_default", "")
                          Description = row.GetValueOrDefault("col_description", "")
                          UDT = lazy None
                          PartOfPrimaryKey = unbox row.["col_part_of_primary_key"]
                          BaseSchemaName = schema.Name
                          BaseTableName = string row.["table_name"] }
                 
                    if not <| schemas.[schema.Name].Tables.[table].Contains(column) then
                        schemas.[schema.Name].Tables.[table].Add(column) |> ignore
                
                    let lookupKey = { TableOID = table.OID
                                      ColumnAttributeNumber = column.ColumnAttributeNumber }
                    if not <| columns.ContainsKey(lookupKey) then
                        columns.Add(lookupKey, column)

        { Schemas = schemas
          Columns = columns
          Enums = enumsLookup
                  |> Seq.map (fun s ->
                    let schemaName = s.Key
                    s.Value |> Seq.map (fun x ->
                        let enumName = x.Key
                        sprintf "%s.%s" schemaName enumName, x.Value))
                  |> Seq.concat
                  |> Map.ofSeq }
