namespace FSharp.Astro.Fits.TypeProvider

open System
open System.IO
open System.Numerics
open System.Reflection
open FSharp.Core.CompilerServices
open FSharp.Quotations
open ProviderImplementation.ProvidedTypes
open FSharp.Astro.Fits
open FSharp.Astro.Fits.TypeProvider.Runtime

/// An erased type provider over FITS files. `FitsProvider<"sample.fits">` yields a type with one
/// property per HDU, each with one property per keyword and, for tables, per column, all typed
/// from the sample.
[<TypeProvider>]
type FitsProviderImplementation(config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(config, addDefaultProbingLocation = true)

    let ns = "FSharp.Astro.Fits.Typed"

    // Provided types belong to the runtime assembly, not to this one. A consumer references that
    // assembly; this one exists only while the compiler is running.
    let asm = typeof<FitsHandle>.Assembly

    let resolve (resolutionFolder: string) (path: string) : string =
        if Path.IsPathRooted path then
            path
        else
            let folder =
                if resolutionFolder = "" then
                    config.ResolutionFolder
                else
                    resolutionFolder

            Path.GetFullPath(Path.Combine(folder, path))

    let valueType (kind: ValueKind) : Type =
        match kind with
        | ValueKind.Logical -> typeof<bool>
        | ValueKind.Integer -> typeof<int64>
        | ValueKind.Real -> typeof<float>
        | ValueKind.String -> typeof<string>
        | ValueKind.Complex -> typeof<Complex>
        | ValueKind.Any -> typeof<CardValue>

    let optionOf (t: Type) : Type = typedefof<option<_>>.MakeGenericType t

    let elementType (t: ColumnType) : Type =
        match t with
        | ColumnType.Logical -> typeof<bool option>
        | ColumnType.Bit -> typeof<bool>
        | ColumnType.UInt8 -> typeof<byte>
        | ColumnType.Int16 -> typeof<int16>
        | ColumnType.Int32 -> typeof<int>
        | ColumnType.Int64 -> typeof<int64>
        | ColumnType.Char -> typeof<string>
        | ColumnType.Float32 -> typeof<float32>
        | ColumnType.Float64 -> typeof<float>
        | ColumnType.Complex32
        | ColumnType.Complex64 -> typeof<Complex>

    let pixelType (bitPix: BitPix) : Type =
        match bitPix with
        | BitPix.UInt8 -> typeof<byte>
        | BitPix.Int16 -> typeof<int16>
        | BitPix.Int32 -> typeof<int>
        | BitPix.Int64 -> typeof<int64>
        | BitPix.Float32 -> typeof<float32>
        | BitPix.Float64 -> typeof<float>

    let genericCall (target: Expr) (holder: Type) (name: string) (typeArgument: Type) (args: Expr list) : Expr =
        let method = holder.GetMethod(name).MakeGenericMethod typeArgument
        Expr.Call(target, method, args)

    /// A property for a keyword, typed from the schema.
    let keywordProperty (schema: KeywordSchema) : ProvidedProperty =
        let keyword = schema.Keyword
        let baseType = valueType schema.Kind
        let propertyType = if schema.Optional then optionOf baseType else baseType

        let getter (args: Expr list) : Expr =
            let h = args[0]

            match schema.Kind, schema.Optional with
            | ValueKind.Logical, false -> <@@ (%%h: HduHandle).Bool keyword @@>
            | ValueKind.Logical, true -> <@@ (%%h: HduHandle).TryBool keyword @@>
            | ValueKind.Integer, false -> <@@ (%%h: HduHandle).Int keyword @@>
            | ValueKind.Integer, true -> <@@ (%%h: HduHandle).TryInt keyword @@>
            | ValueKind.Real, false -> <@@ (%%h: HduHandle).Float keyword @@>
            | ValueKind.Real, true -> <@@ (%%h: HduHandle).TryFloat keyword @@>
            | ValueKind.String, false -> <@@ (%%h: HduHandle).String keyword @@>
            | ValueKind.String, true -> <@@ (%%h: HduHandle).TryString keyword @@>
            | ValueKind.Complex, false -> <@@ (%%h: HduHandle).Complex keyword @@>
            | ValueKind.Complex, true -> <@@ (%%h: HduHandle).TryComplex keyword @@>
            | ValueKind.Any, false -> <@@ (%%h: HduHandle).Value keyword @@>
            | ValueKind.Any, true -> <@@ (%%h: HduHandle).TryValue keyword @@>

        let property =
            ProvidedProperty(Naming.identifier keyword, propertyType, getterCode = getter)

        let doc =
            match schema.Comment with
            | Some comment -> $"{keyword}: {comment}"
            | None -> keyword

        property.AddXmlDoc(
            doc
            + (if schema.Optional then
                   " (absent from some samples)"
               else
                   "")
        )
        property

    /// Properties for one column: one on the row type for a cell, one on the HDU type for the whole
    /// column. A cell is read through RowHandle whichever kind of table it came from, since both
    /// decode to the same Column; only the whole-column accessor knows the difference.
    let columnProperties (schema: ColumnSchema) : ProvidedProperty * ProvidedProperty =
        let index = schema.Index
        let name = Naming.identifier schema.Name

        // TUNITn describes the physical value, so a column that TSCALn and TZEROn still have to be
        // applied to is not in that unit as it stands. The provider hands back stored values.
        let measure (t: Type) =
            if schema.Scale = 1.0 && schema.Zero = 0.0 then
                Measures.annotate t schema.Unit
            else
                t

        let scalarCell (element: Type) (args: Expr list) =
            genericCall <@@ (%%args[0]: RowHandle) @@> typeof<RowHandle> "Scalar" element [ Expr.Value index ]

        let arrayCell (element: Type) (args: Expr list) =
            genericCall <@@ (%%args[0]: RowHandle) @@> typeof<RowHandle> "Array" element [ Expr.Value index ]

        let stringCell (args: Expr list) =
            <@@ (%%args[0]: RowHandle).String index @@>

        let binaryColumn (element: Type) (args: Expr list) =
            genericCall <@@ (%%args[0]: HduHandle).Table @@> typeof<TableHandle> "Column" element [ Expr.Value index ]

        let asciiColumn (element: Type) (args: Expr list) =
            genericCall <@@ (%%args[0]: HduHandle).AsciiTable @@> typeof<AsciiTableHandle> "Column" element [
                Expr.Value index
            ]

        let cellType, cellGetter, columnType, columnGetter =
            match schema.Form with
            | ColumnForm.Ascii(AsciiForm.Char _) ->
                typeof<string>, stringCell, typeof<string[]>, asciiColumn typeof<string>
            | ColumnForm.Ascii(AsciiForm.Integer _) ->
                // An Iw field of any width decodes as int64, and a real form as float.
                typeof<int64>, scalarCell typeof<int64>, typeof<int64[]>, asciiColumn typeof<int64>
            | ColumnForm.Ascii _ ->
                let measured = measure typeof<float>
                measured, scalarCell typeof<float>, measured.MakeArrayType(), asciiColumn typeof<float>
            | ColumnForm.Binary form ->
                let element = elementType form.Type

                let measured =
                    match form.Type with
                    | ColumnType.Float32
                    | ColumnType.Float64 -> measure element
                    | _ -> element

                match form with
                | TForm.Fixed(_, ColumnType.Char) ->
                    typeof<string>, stringCell, typeof<string[]>, binaryColumn typeof<string>
                | TForm.Fixed(1, t) when t <> ColumnType.Bit ->
                    measured, scalarCell element, measured.MakeArrayType(), binaryColumn element
                | TForm.Fixed _ ->
                    measured.MakeArrayType(), arrayCell element, measured.MakeArrayType(), binaryColumn element
                | TForm.Variable(ColumnType.Char, _, _) ->
                    typeof<string>,
                    (fun (args: Expr list) -> <@@ (%%args[0]: RowHandle).VariableString index @@>),
                    typeof<string[]>,
                    (fun (args: Expr list) -> <@@ (%%args[0]: HduHandle).Table.VariableStrings index @@>)
                | TForm.Variable _ ->
                    measured.MakeArrayType(),
                    (fun (args: Expr list) ->
                        genericCall <@@ (%%args[0]: RowHandle) @@> typeof<RowHandle> "Variable" element [
                            Expr.Value index
                        ]
                    ),
                    measured.MakeArrayType().MakeArrayType(),
                    (fun (args: Expr list) ->
                        genericCall <@@ (%%args[0]: HduHandle).Table @@> typeof<TableHandle> "VariableColumn" element [
                            Expr.Value index
                        ]
                    )

        let doc =
            $"Column {schema.Index + 1} '{schema.Name}', TFORM {schema.Form.Text}"
            + (schema.Unit |> Option.map (fun u -> $", unit {u}") |> Option.defaultValue "")

        let cell = ProvidedProperty(name, cellType, getterCode = cellGetter)
        cell.AddXmlDoc doc
        let column = ProvidedProperty(name, columnType, getterCode = columnGetter)
        column.AddXmlDoc(doc + ", every row")
        cell, column

    /// The provided type for one HDU, erased to HduHandle.
    let hduType (schema: HduSchema) : ProvidedTypeDefinition =
        let t =
            ProvidedTypeDefinition(
                schema.PropertyName,
                Some typeof<HduHandle>,
                hideObjectMethods = true,
                isErased = true
            )
        t.AddXmlDoc $"HDU {schema.Index} ({schema.Kind})"

        t.AddMembersDelayed(fun () ->
            [
                ProvidedProperty(
                    "Header",
                    typeof<Header>,
                    getterCode = fun args -> <@@ (%%args[0]: HduHandle).Header @@>
                )
                ProvidedProperty("Info", typeof<HduInfo>, getterCode = fun args -> <@@ (%%args[0]: HduHandle).Info @@>)
                ProvidedProperty(
                    "Comments",
                    typeof<string list>,
                    getterCode = fun args -> <@@ (%%args[0]: HduHandle).Comments @@>
                )
                ProvidedProperty(
                    "History",
                    typeof<string list>,
                    getterCode = fun args -> <@@ (%%args[0]: HduHandle).History @@>
                )
            ]
            @ (schema.Keywords |> List.map keywordProperty)
        )

        match schema.BitPix with
        | Some bitPix ->
            let pixel = pixelType bitPix
            t.AddMember(
                ProvidedProperty("Shape", typeof<int[]>, getterCode = fun args -> <@@ (%%args[0]: HduHandle).Shape @@>)
            )
            t.AddMember(
                ProvidedProperty("Image", typeof<Image>, getterCode = fun args -> <@@ (%%args[0]: HduHandle).Image @@>)
            )
            let pixels =
                ProvidedProperty(
                    "Pixels",
                    (Measures.annotate typeof<float> schema.BUnit).MakeArrayType(),
                    getterCode = fun args -> <@@ (%%args[0]: HduHandle).Pixels @@>
                )

            pixels.AddXmlDoc(
                "Physical pixel values, BZERO + BSCALE * stored, in C order"
                + (schema.BUnit |> Option.map (fun u -> $", unit {u}") |> Option.defaultValue "")
            )

            t.AddMember pixels

            let data =
                ProvidedProperty(
                    "Data",
                    pixel.MakeArrayType(),
                    getterCode =
                        fun args -> genericCall <@@ (%%args[0]: HduHandle) @@> typeof<HduHandle> "Data" pixel []
                )

            data.AddXmlDoc $"Stored pixels as BITPIX {bitPix.Code} declares, in C order"
            t.AddMember data
        | None -> ()

        // Both kinds of table get the same shape of members. They differ in the handle the whole
        // table goes through, and so in the type the untyped Table property hands back.
        match schema.Kind with
        | HduKind.BinaryTable
        | HduKind.AsciiTable ->
            let ascii = schema.Kind = HduKind.AsciiTable

            let row =
                ProvidedTypeDefinition("Row", Some typeof<RowHandle>, hideObjectMethods = true, isErased = true)

            row.AddXmlDoc "One row of the table"

            row.AddMember(
                ProvidedProperty("Index", typeof<int>, getterCode = fun args -> <@@ (%%args[0]: RowHandle).Index @@>)
            )

            t.AddMember row

            let cells, columns = schema.Columns |> List.map columnProperties |> List.unzip
            row.AddMembers cells
            t.AddMembers columns

            let table =
                if ascii then
                    ProvidedProperty(
                        "Table",
                        typeof<AsciiTable>,
                        getterCode = fun args -> <@@ (%%args[0]: HduHandle).AsciiTable.Table @@>
                    )
                else
                    ProvidedProperty(
                        "Table",
                        typeof<BinaryTable>,
                        getterCode = fun args -> <@@ (%%args[0]: HduHandle).Table.Table @@>
                    )

            table.AddXmlDoc "The untyped table"
            t.AddMember table

            t.AddMember(
                ProvidedProperty(
                    "Count",
                    typeof<int>,
                    getterCode =
                        fun args ->
                            if ascii then
                                <@@ (%%args[0]: HduHandle).AsciiTable.Count @@>
                            else
                                <@@ (%%args[0]: HduHandle).Table.Count @@>
                )
            )

            t.AddMember(
                ProvidedProperty(
                    "Rows",
                    row.MakeArrayType(),
                    getterCode =
                        fun args ->
                            if ascii then
                                <@@ (%%args[0]: HduHandle).AsciiTable.Rows @@>
                            else
                                <@@ (%%args[0]: HduHandle).Table.Rows @@>
                )
            )

            t.AddMember(
                ProvidedMethod(
                    "Row",
                    [ ProvidedParameter("index", typeof<int>) ],
                    row,
                    invokeCode =
                        fun args ->
                            if ascii then
                                <@@ (%%args[0]: HduHandle).AsciiTable.Row(%%args[1]: int) @@>
                            else
                                <@@ (%%args[0]: HduHandle).Table.Row(%%args[1]: int) @@>
                )
            )
        | _ -> ()

        t

    /// The provided type for a file, erased to FitsHandle.
    let fileType
        (typeName: string)
        (schemas: HduSchema list)
        (lenient: bool)
        (sample: string option)
        : ProvidedTypeDefinition =
        let t =
            ProvidedTypeDefinition(
                asm,
                ns,
                typeName,
                Some typeof<FitsHandle>,
                hideObjectMethods = true,
                isErased = true
            )
        t.AddXmlDoc "A FITS file with the HDUs and keywords of the sample"

        let names = schemas |> List.map (fun s -> s.PropertyName) |> Naming.unique

        for schema, name in List.zip schemas names do
            let schema = { schema with PropertyName = name }
            let hdu = hduType schema
            t.AddMember hdu
            let extName = schema.ExtName
            let index = schema.Index

            let property =
                ProvidedProperty(
                    name,
                    hdu,
                    getterCode = fun args -> <@@ (%%args[0]: FitsHandle).Hdu(extName, index) @@>
                )

            property.AddXmlDoc($"HDU {index}" + (if extName <> "" then $" EXTNAME '{extName}'" else ""))
            t.AddMember property

        t.AddMember(
            ProvidedProperty("File", typeof<FitsFile>, getterCode = fun args -> <@@ (%%args[0]: FitsHandle).File @@>)
        )

        let loadPath =
            ProvidedMethod(
                "Load",
                [ ProvidedParameter("path", typeof<string>) ],
                t,
                isStatic = true,
                invokeCode = fun args -> <@@ FitsHandle.Load((%%args[0]: string), lenient) @@>
            )

        loadPath.AddXmlDoc "Opens a file by path, or downloads a URL into memory and opens that."
        t.AddMember loadPath

        let loadStream =
            ProvidedMethod(
                "Load",
                [ ProvidedParameter("stream", typeof<Stream>) ],
                t,
                isStatic = true,
                invokeCode = fun args -> <@@ FitsHandle.Load((%%args[0]: Stream), lenient) @@>
            )

        loadStream.AddXmlDoc "Reads a stream, taking ownership of it. A stream that cannot seek is copied into memory."
        t.AddMember loadStream

        let asyncLoad =
            ProvidedMethod(
                "AsyncLoad",
                [ ProvidedParameter("path", typeof<string>) ],
                ProvidedTypeBuilder.MakeGenericType(typedefof<Async<_>>, [ t ]),
                isStatic = true,
                invokeCode = fun args -> <@@ FitsHandle.AsyncLoad((%%args[0]: string), lenient) @@>
            )

        asyncLoad.AddXmlDoc "Opens a file by path, or downloads a URL, without blocking."
        t.AddMember asyncLoad

        let parse =
            ProvidedMethod(
                "Parse",
                [ ProvidedParameter("bytes", typeof<byte[]>) ],
                t,
                isStatic = true,
                invokeCode = fun args -> <@@ FitsHandle.Parse((%%args[0]: byte[]), lenient) @@>
            )

        parse.AddXmlDoc "Opens bytes already in memory."
        t.AddMember parse

        match sample with
        | Some path ->
            let getSample =
                ProvidedMethod(
                    "GetSample",
                    [],
                    t,
                    isStatic = true,
                    invokeCode = fun _ -> <@@ FitsHandle.Load(path, lenient) @@>
                )

            getSample.AddXmlDoc $"Opens the sample this type was generated from: {path}"
            t.AddMember getSample
        | None -> ()

        t

    let root =
        ProvidedTypeDefinition(asm, ns, "FitsProvider", Some typeof<obj>, isErased = true)

    do
        root.AddXmlDoc
            "Typed access to FITS files shaped like a sample. Sample is a path to a FITS file or a text header dump, or several separated by semicolons. Lenient opens non-conforming samples. Schema adds or overrides keywords as \"HDU.KEYWORD: type [option]\". ResolutionFolder is the base for relative paths."

        root.DefineStaticParameters(
            [
                ProvidedStaticParameter("Sample", typeof<string>)
                ProvidedStaticParameter("Lenient", typeof<bool>, false)
                ProvidedStaticParameter("Schema", typeof<string>, "")
                ProvidedStaticParameter("ResolutionFolder", typeof<string>, "")
            ],
            fun typeName args ->
                let sample = args[0] :?> string
                let lenient = args[1] :?> bool
                let overrides = args[2] :?> string
                let folder = args[3] :?> string

                let paths =
                    sample.Split([| ';' |], StringSplitOptions.RemoveEmptyEntries)
                    |> Array.map (fun path -> resolve folder (path.Trim()))

                let schemas =
                    paths
                    |> Array.map (Schema.ofSample lenient)
                    |> List.ofArray
                    |> Schema.merge
                    |> Schema.applyOverrides overrides

                // GetSample needs a real file to open, so it is offered only when one sample is one.
                fileType typeName schemas lenient (paths |> Array.tryFind Schema.isFitsFile)
        )

        this.AddNamespace(ns, [ root ])
