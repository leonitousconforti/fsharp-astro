namespace FSharp.Astro.Fits

open System
open System.Collections.Concurrent
open System.Numerics
open System.Reflection
open Microsoft.FSharp.Reflection

/// Names the header keyword a record field maps to, when it is not the field name in upper case.
[<AttributeUsage(AttributeTargets.Property ||| AttributeTargets.Field, AllowMultiple = false)>]
type KeywordAttribute(name: string) =
    inherit Attribute()

    /// The keyword.
    member _.Name: string = name

/// Names the table column a record field maps to, when it is not the field name.
[<AttributeUsage(AttributeTargets.Property ||| AttributeTargets.Field, AllowMultiple = false)>]
type ColumnAttribute(name: string) =
    inherit Attribute()

    /// The column name, matched ignoring case.
    member _.Name: string = name

/// Maps F# records onto headers and table rows by field name, with attributes to override names.
/// Headers: each field reads the keyword named by [<Keyword>] or the field name in upper case.
/// Tables: each field reads the column named by [<Column>] or the field name. Option fields accept
/// a missing keyword, an undefined logical, or a TNULL value. Float fields of integer columns get
/// physical values with TSCAL and TZERO applied. Array fields take multi-element cells.
[<RequireQualifiedAccess>]
module Record =
    let private isOption (t: Type) =
        t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>

    let private someCase (t: Type) =
        FSharpType.GetUnionCases t |> Array.find (fun c -> c.Name = "Some")

    let private noneCase (t: Type) =
        FSharpType.GetUnionCases t |> Array.find (fun c -> c.Name = "None")

    let private makeOption (t: Type) (value: obj option) : obj =
        match value with
        | Some v -> FSharpValue.MakeUnion(someCase t, [| v |])
        | None -> FSharpValue.MakeUnion(noneCase t, [||])

    let private attributeName<'A when 'A :> Attribute> (pick: 'A -> string) (property: PropertyInfo) : string option =
        let attribute = property.GetCustomAttribute<'A>()
        if isNull (box attribute) then
            None
        else
            Some(pick attribute)

    let private recordFields (t: Type) : PropertyInfo[] =
        if not (FSharpType.IsRecord(t, true)) then
            invalidOp $"{t.FullName} is not an F# record"

        FSharpType.GetRecordFields(t, true)

    // Headers

    let rec private fieldDecoder (field: string) (t: Type) (keyword: string) : Decoder<obj> =
        if t = typeof<string> then
            Decode.string keyword |> Decode.map box
        elif t = typeof<float> then
            Decode.float keyword |> Decode.map box
        elif t = typeof<float32> then
            Decode.float keyword |> Decode.map (float32 >> box)
        elif t = typeof<int> then
            Decode.int keyword |> Decode.map box
        elif t = typeof<int64> then
            Decode.int64 keyword |> Decode.map box
        elif t = typeof<bool> then
            Decode.bool keyword |> Decode.map box
        elif t = typeof<Complex> then
            Decode.complex keyword |> Decode.map box
        elif t = typeof<CardValue> then
            Decode.value keyword |> Decode.map box
        elif isOption t then
            fieldDecoder field (t.GetGenericArguments()[0]) keyword
            |> Decode.optional
            |> Decode.map (makeOption t)
        else
            invalidOp $"field '{field}' has type {t.Name}, which cannot be read from a header keyword"

    let private headerDecoders = ConcurrentDictionary<Type, obj>()

    /// A decoder for a record type, built once per type from its fields.
    let decoder<'T> () : Decoder<'T> =
        headerDecoders.GetOrAdd(
            typeof<'T>,
            fun t ->
                let fields = recordFields t
                let construct = FSharpValue.PreComputeRecordConstructor(t, true)

                let decoders =
                    fields
                    |> Array.map (fun f ->
                        let keyword =
                            attributeName<KeywordAttribute> (fun a -> a.Name) f
                            |> Option.defaultValue (f.Name.ToUpperInvariant())

                        fieldDecoder f.Name f.PropertyType keyword
                    )

                let decoder: Decoder<'T> =
                    fun header ->
                        let results = decoders |> Array.map (fun d -> d header)

                        let issues =
                            results
                            |> Array.collect (fun r ->
                                match r with
                                | Error e -> Array.ofList e
                                | Ok _ -> [||]
                            )

                        if issues.Length > 0 then
                            Error(List.ofArray issues)
                        else
                            let values =
                                results
                                |> Array.map (fun r ->
                                    match r with
                                    | Ok v -> v
                                    | Error _ -> null
                                )

                            Ok(construct values :?> 'T)

                box decoder
        )
        :?> Decoder<'T>

    /// Reads a record from a header.
    let tryOfHeader<'T> (header: Header) : Result<'T, Issue list> = Decode.run (decoder<'T>()) header

    /// Reads a record from a header, throwing FitsException when a field cannot be filled.
    let ofHeader<'T> (header: Header) : 'T =
        match tryOfHeader<'T> header with
        | Ok v -> v
        | Error issues -> raise (FitsException(Violations issues))

    // Tables

    let private mismatch (column: ColumnInfo) (t: Type) =
        Issue.create (Violation.TypeMismatch(column.Name, t.Name, TForm.format column.Form))

    /// Copies a flat cell into an array of the rank and lengths TDIMn declares. The flat values
    /// are already in C order, which is the order a multidimensional array is laid out in, so this
    /// walks both in step.
    let private reshape (element: Type) (shape: int[]) (flat: Array) : Array =
        let result = Array.CreateInstance(element, shape)
        let index = Array.zeroCreate<int> shape.Length

        for k in 0 .. flat.Length - 1 do
            result.SetValue(flat.GetValue k, index)
            let mutable axis = shape.Length - 1
            let mutable carry = true

            while carry && axis >= 0 do
                index[axis] <- index[axis] + 1

                if index[axis] >= shape[axis] then
                    index[axis] <- 0
                    axis <- axis - 1
                else
                    carry <- false

        result

    /// A field typed as unsigned, or as a signed byte, over a column written with the matching
    /// TZEROn offset convention. Nothing else in the mapping looks at TZEROn this way, because for
    /// any other value it is a real scaling that belongs to the physical value.
    let private unsignedCells (t: Type) (repeat: int) (values: 'A[]) : (int -> obj) option =
        if t = typeof<'A> then
            if repeat = 1 then Some(fun i -> box values[i]) else None
        elif t = typeof<'A[]> then
            Some(fun i -> box (Array.sub values (i * repeat) repeat))
        else
            None

    let private unsignedReader (t: Type) (column: Column) : (int -> obj) option =
        let repeat = column.Info.Form.Repeat

        if Table.isInt8 column then
            unsignedCells t repeat (Table.toInt8 column)
        elif Table.isUInt16 column then
            unsignedCells t repeat (Table.toUInt16 column)
        elif Table.isUInt32 column then
            unsignedCells t repeat (Table.toUInt32 column)
        elif Table.isUInt64 column then
            unsignedCells t repeat (Table.toUInt64 column)
        else
            None

    /// Builds a function from row index to the boxed value of one field, or an issue when the
    /// field type does not fit the column.
    let rec private cellReader (t: Type) (column: Column) : Result<int -> obj, Issue> =
        let info = column.Info
        let repeat = info.Form.Repeat
        let wrong () = Error(mismatch info t)

        // A field of rank two or more is a cell shaped by TDIMn. Read it flat and fold it up.
        if t.IsArray && t.GetArrayRank() > 1 then
            let element = t.GetElementType()
            let shape = Table.cellShape info

            if shape.Length <> t.GetArrayRank() then
                wrong ()
            else
                match cellReader (element.MakeArrayType()) column with
                | Error issue -> Error issue
                | Ok read -> Ok(fun i -> box (reshape element shape (read i :?> Array)))
        else

        match unsignedReader t column with
        | Some read -> Ok read
        | None ->

        let scalar (values: 'A[]) (convert: 'A -> obj) : Result<int -> obj, Issue> =
            if repeat = 1 then
                Ok(fun i -> convert values[i])
            else
                wrong ()

        let slice (values: 'A[]) : Result<int -> obj, Issue> =
            Ok(fun i -> box (Array.sub values (i * repeat) repeat))

        let physical () =
            if repeat = 1 then
                let values = Table.toFloat64 column
                Ok(fun i -> box values[i])
            else
                wrong ()

        match column.Data with
        | ColumnData.Variable entries ->
            if t = typeof<string> then
                Ok(fun i ->
                    match entries[i] with
                    | ColumnData.Strings [| s |] -> box s
                    | _ -> box ""
                )
            elif t.IsArray then
                let element = t.GetElementType()

                Ok(fun i ->
                    let entry = {
                        column with
                            Data = entries[i]
                            Info = {
                                info with
                                    Form = TForm.Fixed(entries[i].Length, info.Form.Type)
                            }
                    }

                    match cellReader t entry with
                    | Ok read -> read 0
                    | Error issue -> raise (FitsException(Violations [ issue ]))
                )
                |> fun r ->
                    // Validate the element type against the column type up front.
                    match element, info.Form.Type with
                    | e, ColumnType.Float32 when e = typeof<float32> -> r
                    | e, ColumnType.Float64 when e = typeof<float> -> r
                    | e, ColumnType.Int16 when e = typeof<int16> -> r
                    | e, ColumnType.Int32 when e = typeof<int> -> r
                    | e, ColumnType.Int64 when e = typeof<int64> -> r
                    | e, ColumnType.UInt8 when e = typeof<byte> -> r
                    | e, ColumnType.Bit when e = typeof<bool> -> r
                    | e, ColumnType.Logical when e = typeof<bool option> -> r
                    | e, (ColumnType.Complex32 | ColumnType.Complex64) when e = typeof<Complex> -> r
                    | _ -> wrong ()
            else
                wrong ()
        | ColumnData.Strings values ->
            if t = typeof<string> then
                Ok(fun i -> box values[i])
            else
                wrong ()
        | ColumnData.Logicals values ->
            if t = typeof<bool option> then
                scalar values box
            elif t = typeof<bool option[]> then
                slice values
            elif t = typeof<bool> then
                scalar
                    values
                    (fun v ->
                        match v with
                        | Some b -> box b
                        | None ->
                            raise (
                                FitsException(
                                    Violations [
                                        Issue.create (Violation.TypeMismatch(info.Name, "a logical", "undefined"))
                                    ]
                                )
                            )
                    )
            else
                wrong ()
        | ColumnData.Bits values -> if t = typeof<bool[]> then slice values else wrong ()
        | ColumnData.UInt8s values ->
            if t = typeof<byte> then
                scalar values box
            elif t = typeof<int> then
                scalar values (int >> box)
            elif t = typeof<int64> then
                scalar values (int64 >> box)
            elif t = typeof<float> || t = typeof<float32> then
                physical ()
                |> Result.map (fun read ->
                    if t = typeof<float32> then
                        (fun i -> box (float32 (unbox<float>(read i))))
                    else
                        read
                )
            elif t = typeof<byte[]> then
                slice values
            else
                wrong ()
        | ColumnData.Int16s values ->
            if t = typeof<int16> then
                scalar values box
            elif t = typeof<int> then
                scalar values (int >> box)
            elif t = typeof<int64> then
                scalar values (int64 >> box)
            elif t = typeof<float> || t = typeof<float32> then
                physical ()
                |> Result.map (fun read ->
                    if t = typeof<float32> then
                        (fun i -> box (float32 (unbox<float>(read i))))
                    else
                        read
                )
            elif t = typeof<int16[]> then
                slice values
            else
                wrong ()
        | ColumnData.Int32s values ->
            if t = typeof<int> then
                scalar values box
            elif t = typeof<int64> then
                scalar values (int64 >> box)
            elif t = typeof<float> || t = typeof<float32> then
                physical ()
                |> Result.map (fun read ->
                    if t = typeof<float32> then
                        (fun i -> box (float32 (unbox<float>(read i))))
                    else
                        read
                )
            elif t = typeof<int[]> then
                slice values
            else
                wrong ()
        | ColumnData.Int64s values ->
            if t = typeof<int64> then
                scalar values box
            elif t = typeof<float> || t = typeof<float32> then
                physical ()
                |> Result.map (fun read ->
                    if t = typeof<float32> then
                        (fun i -> box (float32 (unbox<float>(read i))))
                    else
                        read
                )
            elif t = typeof<int64[]> then
                slice values
            else
                wrong ()
        | ColumnData.Float32s values ->
            if t = typeof<float32> then scalar values box
            elif t = typeof<float> then physical ()
            elif t = typeof<float32[]> then slice values
            else wrong ()
        | ColumnData.Float64s values ->
            if t = typeof<float> then
                physical ()
            elif t = typeof<float32> then
                physical () |> Result.map (fun read i -> box (float32 (unbox<float>(read i))))
            elif t = typeof<float[]> then
                slice values
            else
                wrong ()
        | ColumnData.Complex32s values
        | ColumnData.Complex64s values ->
            if t = typeof<Complex> then scalar values box
            elif t = typeof<Complex[]> then slice values
            else wrong ()

    /// Wraps a reader so that TNULL integers, NaN reals and undefined logicals become None.
    let private optionalReader (t: Type) (column: Column) : Result<int -> obj, Issue> =
        let inner = t.GetGenericArguments()[0]

        let isNull: (int -> bool) option =
            match column.Data, column.Info.Null with
            | ColumnData.UInt8s a, Some blank -> Some(fun i -> int64 a[i] = blank)
            | ColumnData.Int16s a, Some blank -> Some(fun i -> int64 a[i] = blank)
            | ColumnData.Int32s a, Some blank -> Some(fun i -> int64 a[i] = blank)
            | ColumnData.Int64s a, Some blank -> Some(fun i -> a[i] = blank)
            | ColumnData.Float32s a, _ -> Some(fun i -> Single.IsNaN a[i])
            | ColumnData.Float64s a, _ -> Some(fun i -> Double.IsNaN a[i])
            | ColumnData.Logicals a, _ -> Some(fun i -> a[i].IsNone)
            | _ -> None

        if inner = typeof<bool> && column.Info.Form.Repeat = 1 then
            match column.Data with
            | ColumnData.Logicals a -> Ok(fun i -> makeOption t (a[i] |> Option.map box))
            | _ -> Error(mismatch column.Info t)
        else
            cellReader inner column
            |> Result.map (fun read ->
                match isNull with
                | Some isNull -> fun i -> makeOption t (if isNull i then None else Some(read i))
                | None -> fun i -> makeOption t (Some(read i))
            )

    /// Reads one record per row, asking for a column by name only when a field needs it. Both
    /// kinds of table go through here, since decoding either gives back the same Column type.
    let private ofLookup<'T> (rows: int) (find: string -> Column option) : Result<'T[], Issue list> =
        let t = typeof<'T>
        let fields = recordFields t
        let construct = FSharpValue.PreComputeRecordConstructor(t, true)

        let readers =
            fields
            |> Array.map (fun f ->
                let name =
                    attributeName<ColumnAttribute> (fun a -> a.Name) f |> Option.defaultValue f.Name

                match find name with
                | None -> Error(Issue.create (Violation.MissingColumn name))
                | Some column ->
                    if isOption f.PropertyType then
                        optionalReader f.PropertyType column
                    else
                        cellReader f.PropertyType column
            )

        let issues =
            readers
            |> Array.choose (fun r ->
                match r with
                | Error e -> Some e
                | Ok _ -> None
            )

        if issues.Length > 0 then
            Error(List.ofArray issues)
        else
            let readers =
                readers
                |> Array.map (fun r ->
                    match r with
                    | Ok read -> read
                    | Error _ -> fun _ -> null
                )

            Ok(Array.init rows (fun i -> construct (readers |> Array.map (fun read -> read i)) :?> 'T))

    /// Reads one record per row from columns already decoded, matching fields to columns by name.
    let tryOfColumns<'T> (rows: int) (columns: Column list) : Result<'T[], Issue list> =
        ofLookup<'T>
            rows
            (fun name ->
                columns
                |> List.tryFind (fun c -> String.Equals(c.Info.Name, name, StringComparison.OrdinalIgnoreCase))
            )

    /// Reads one record per row of a binary table. Only the columns the record names are decoded.
    let tryOfTable<'T> (table: BinaryTable) : Result<'T[], Issue list> =
        ofLookup<'T> table.Rows (fun name -> Table.tryColumn name table)

    /// Reads one record per row of a binary table, throwing FitsException when a field cannot be
    /// filled.
    let ofTable<'T> (table: BinaryTable) : 'T[] =
        match tryOfTable<'T> table with
        | Ok rows -> rows
        | Error issues -> raise (FitsException(Violations issues))

    /// Reads one record per row of an ASCII table. Only the fields the record names are decoded.
    let tryOfAsciiTable<'T> (table: AsciiTable) : Result<'T[], Issue list> =
        ofLookup<'T> table.Rows (fun name -> Ascii.tryColumn name table)

    /// Reads one record per row of an ASCII table, throwing FitsException when a field cannot be
    /// filled.
    let ofAsciiTable<'T> (table: AsciiTable) : 'T[] =
        match tryOfAsciiTable<'T> table with
        | Ok rows -> rows
        | Error issues -> raise (FitsException(Violations issues))
