/// Types the provided types erase to. Every property the provider generates compiles into a call
/// on one of these, so the sample file is never needed at runtime.
namespace FSharp.Astro.Fits.TypeProvider.Runtime

open System
open System.IO
open System.Numerics
open FSharp.Astro.Fits

/// Extracts typed arrays from column and image data. The provider only asks for the type the data
/// actually has, so the casts cannot fail for files that match the sample.
module Cells =
    /// The underlying array of a column as the element type the provider inferred.
    let array<'T> (data: ColumnData) : 'T[] =
        let boxed =
            match data with
            | ColumnData.Logicals a -> box a
            | ColumnData.Bits a -> box a
            | ColumnData.UInt8s a -> box a
            | ColumnData.Int16s a -> box a
            | ColumnData.Int32s a -> box a
            | ColumnData.Int64s a -> box a
            | ColumnData.Strings a -> box a
            | ColumnData.Float32s a -> box a
            | ColumnData.Float64s a -> box a
            | ColumnData.Complex32s a -> box a
            | ColumnData.Complex64s a -> box a
            | ColumnData.Variable a -> box a

        match boxed with
        | :? ('T[]) as typed -> typed
        | _ -> invalidOp $"column holds {data.Type} data, not {typeof<'T>.Name}"

    /// The underlying array of an image as the element type the provider inferred.
    let pixels<'T> (data: ImageData) : 'T[] =
        let boxed =
            match data with
            | ImageData.UInt8 a -> box a
            | ImageData.Int16 a -> box a
            | ImageData.Int32 a -> box a
            | ImageData.Int64 a -> box a
            | ImageData.Float32 a -> box a
            | ImageData.Float64 a -> box a

        match boxed with
        | :? ('T[]) as typed -> typed
        | _ -> invalidOp $"image holds {data.BitPix} data, not {typeof<'T>.Name}"

/// One row of a table. Cells are read from columns decoded once for the whole table.
type RowHandle(columns: Column[], index: int) =
    /// Zero-based row number.
    member _.Index: int = index

    /// A single-element cell.
    member _.Scalar<'T>(column: int) : 'T =
        (Cells.array<'T> columns[column].Data)[index]

    /// A string cell.
    member _.String(column: int) : string =
        match columns[column].Data with
        | ColumnData.Strings a -> a[index]
        | _ -> invalidOp "not a string column"

    /// A multi-element cell.
    member _.Array<'T>(column: int) : 'T[] =
        let repeat = columns[column].Info.Form.Repeat
        Array.sub (Cells.array<'T> columns[column].Data) (index * repeat) repeat

    /// A variable-length array cell.
    member _.Variable<'T>(column: int) : 'T[] =
        match columns[column].Data with
        | ColumnData.Variable entries -> Cells.array<'T> entries[index]
        | _ -> invalidOp "not a variable-length column"

    /// A variable-length string cell.
    member _.VariableString(column: int) : string =
        match columns[column].Data with
        | ColumnData.Variable entries ->
            match entries[index] with
            | ColumnData.Strings [| s |] -> s
            | _ -> ""
        | _ -> invalidOp "not a variable-length column"

    override _.ToString() = $"row {index}"

/// A decoded binary table.
type TableHandle(table: BinaryTable) =
    let columns = lazy (Table.columns table |> Array.ofList)

    /// The untyped table.
    member _.Table: BinaryTable = table

    /// Number of rows.
    member _.Count: int = table.Rows

    /// Every row.
    member _.Rows: RowHandle[] =
        Array.init table.Rows (fun i -> RowHandle(columns.Value, i))

    /// One row.
    member _.Row(index: int) : RowHandle = RowHandle(columns.Value, index)

    /// A whole fixed-width column, rows times repeat elements.
    member _.Column<'T>(column: int) : 'T[] =
        Cells.array<'T> columns.Value[column].Data

    /// A whole variable-length column, one array per row.
    member _.VariableColumn<'T>(column: int) : 'T[][] =
        match columns.Value[column].Data with
        | ColumnData.Variable entries -> entries |> Array.map Cells.array<'T>
        | _ -> invalidOp "not a variable-length column"

    /// A whole variable-length string column.
    member _.VariableStrings(column: int) : string[] =
        match columns.Value[column].Data with
        | ColumnData.Variable entries ->
            entries
            |> Array.map (fun e ->
                match e with
                | ColumnData.Strings [| s |] -> s
                | _ -> ""
            )
        | _ -> invalidOp "not a variable-length column"

/// A decoded ASCII table. Decoding a field gives back the same Column a binary table does, so the
/// rows here are the same RowHandle, and only the whole-field accessors differ.
type AsciiTableHandle(table: AsciiTable) =
    let columns = lazy (Ascii.columns table |> Array.ofList)

    /// The untyped table.
    member _.Table: AsciiTable = table

    /// Number of rows.
    member _.Count: int = table.Rows

    /// Every row.
    member _.Rows: RowHandle[] =
        Array.init table.Rows (fun i -> RowHandle(columns.Value, i))

    /// One row.
    member _.Row(index: int) : RowHandle = RowHandle(columns.Value, index)

    /// A whole field, one value per row.
    member _.Column<'T>(column: int) : 'T[] =
        Cells.array<'T> columns.Value[column].Data

/// One HDU of an open file.
type HduHandle(file: FitsFile, info: HduInfo) =
    let image = lazy (Fits.readImage file info.Index)
    let table = lazy (TableHandle(Fits.readTable file info.Index))
    let asciiTable = lazy (AsciiTableHandle(Fits.readAsciiTable file info.Index))

    /// The file this HDU belongs to.
    member _.File: FitsFile = file

    /// The untyped HDU.
    member _.Info: HduInfo = info

    /// The header.
    member _.Header: Header = info.Header

    /// Text of every COMMENT card.
    member _.Comments: string list = Header.comments info.Header

    /// Text of every HISTORY card.
    member _.History: string list = Header.history info.Header

    member _.String(keyword: string) : string = Header.getString keyword info.Header
    member _.Float(keyword: string) : float = Header.getFloat keyword info.Header
    member _.Int(keyword: string) : int64 = Header.getInt keyword info.Header
    member _.Bool(keyword: string) : bool = Header.getBool keyword info.Header
    member _.Value(keyword: string) : CardValue = info.Header[keyword]

    member _.Complex(keyword: string) : Complex =
        match info.Header[keyword] with
        | CardValue.ComplexInt(re, im) -> Complex(float re, float im)
        | CardValue.ComplexReal(re, im) -> Complex(re, im)
        | _ -> raise (Collections.Generic.KeyNotFoundException $"keyword '{keyword}' is not complex")

    member _.TryString(keyword: string) : string option = Header.tryString keyword info.Header
    member _.TryFloat(keyword: string) : float option = Header.tryFloat keyword info.Header
    member _.TryInt(keyword: string) : int64 option = Header.tryInt keyword info.Header
    member _.TryBool(keyword: string) : bool option = Header.tryBool keyword info.Header
    member _.TryValue(keyword: string) : CardValue option = Header.tryValue keyword info.Header

    member this.TryComplex(keyword: string) : Complex option =
        if Header.contains keyword info.Header then
            Some(this.Complex keyword)
        else
            None

    /// The image, read on first use.
    member _.Image: Image = image.Value

    /// Axis lengths in C order.
    member _.Shape: int[] = Image.shapeOfAxes info.Axes

    /// Stored pixel values as the type BITPIX declares.
    member _.Data<'T>() : 'T[] = Cells.pixels<'T> image.Value.Data

    /// Physical pixel values with scaling applied.
    member _.Pixels: float[] = Image.toFloat64 image.Value

    /// The table, decoded on first use.
    member _.Table: TableHandle = table.Value

    /// The ASCII table, decoded on first use.
    member _.AsciiTable: AsciiTableHandle = asciiTable.Value

    override _.ToString() = $"HDU {info.Index} ({info.Kind})"

/// An open file behind a provided type.
type FitsHandle(file: FitsFile) =
    static let http = lazy (new Net.Http.HttpClient())

    static member private Http: Net.Http.HttpClient = http.Value

    /// The untyped file.
    member _.File: FitsFile = file

    /// Finds an HDU by EXTNAME, falling back to the index it had in the sample.
    member _.Hdu(name: string, index: int) : HduHandle =
        let info =
            match (if name = "" then None else Fits.tryFindByName name file) with
            | Some info -> info
            | None when index < file.Count -> file[index]
            | None ->
                raise (
                    Collections.Generic.KeyNotFoundException $"no HDU named '{name}' and fewer than {index + 1} HDUs"
                )

        HduHandle(file, info)

    static member private Options(lenient: bool) : FitsOptions =
        if lenient then FitsOptions.Lenient else FitsOptions.Default

    /// True for http and https URLs.
    static member IsUrl(path: string) : bool =
        path.StartsWith("http://", StringComparison.OrdinalIgnoreCase)
        || path.StartsWith("https://", StringComparison.OrdinalIgnoreCase)

    /// Opens a file by path, or downloads a URL into memory and opens that.
    static member Load(path: string, lenient: bool) : FitsHandle =
        if FitsHandle.IsUrl path then
            FitsHandle.AsyncLoad(path, lenient) |> Async.RunSynchronously
        else
            new FitsHandle(Fits.openFileWith (FitsHandle.Options lenient) path)

    /// Reads a stream, taking ownership of it. A stream that cannot seek is copied into memory.
    static member Load(stream: Stream, lenient: bool) : FitsHandle =
        new FitsHandle(Fits.openStreamWith (FitsHandle.Options lenient) false stream)

    /// Opens bytes already in memory.
    static member Parse(bytes: byte[], lenient: bool) : FitsHandle =
        new FitsHandle(Fits.openBytesWith (FitsHandle.Options lenient) bytes)

    /// Opens a file by path, or downloads a URL, without blocking.
    static member AsyncLoad(path: string, lenient: bool) : Async<FitsHandle> = async {
        if FitsHandle.IsUrl path then
            let! bytes = FitsHandle.Http.GetByteArrayAsync path |> Async.AwaitTask
            return FitsHandle.Parse(bytes, lenient)
        else
            return new FitsHandle(Fits.openFileWith (FitsHandle.Options lenient) path)
    }

    interface IDisposable with
        member _.Dispose() = (file :> IDisposable).Dispose()

// The compiler reads this from the runtime assembly and loads the named assembly from
// typeproviders/fsharp41/ to get FitsProvider itself.
[<assembly: CompilerServices.TypeProviderAssembly("FSharp.Astro.Fits.TypeProvider.DesignTime")>]
do ()
