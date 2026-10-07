namespace FSharp.Astro.Fits

open System
open System.Buffers.Binary
open System.Numerics
open System.Text
open System.Text.RegularExpressions

/// The element type of a binary table column, as declared by the TFORM type code.
[<RequireQualifiedAccess>]
type ColumnType =
    | Logical
    | Bit
    | UInt8
    | Int16
    | Int32
    | Int64
    | Char
    | Float32
    | Float64
    | Complex32
    | Complex64

    /// The TFORM type code.
    member this.Code: char =
        match this with
        | Logical -> 'L'
        | Bit -> 'X'
        | UInt8 -> 'B'
        | Int16 -> 'I'
        | Int32 -> 'J'
        | Int64 -> 'K'
        | Char -> 'A'
        | Float32 -> 'E'
        | Float64 -> 'D'
        | Complex32 -> 'C'
        | Complex64 -> 'M'

    /// Bytes per element. Bits pack eight to a byte and report zero here.
    member this.Size: int =
        match this with
        | Logical
        | UInt8
        | Char -> 1
        | Bit -> 0
        | Int16 -> 2
        | Int32
        | Float32 -> 4
        | Int64
        | Float64
        | Complex32 -> 8
        | Complex64 -> 16

    /// Bytes occupied by a run of elements.
    member this.BytesFor(count: int) : int =
        match this with
        | Bit -> (count + 7) / 8
        | _ -> count * this.Size

    /// The type for a TFORM code.
    static member TryOfCode(code: char) : ColumnType option =
        match code with
        | 'L' -> Some Logical
        | 'X' -> Some Bit
        | 'B' -> Some UInt8
        | 'I' -> Some Int16
        | 'J' -> Some Int32
        | 'K' -> Some Int64
        | 'A' -> Some Char
        | 'E' -> Some Float32
        | 'D' -> Some Float64
        | 'C' -> Some Complex32
        | 'M' -> Some Complex64
        | _ -> None

/// A TFORM value: a fixed-width cell, or a descriptor pointing at an array in the heap.
[<RequireQualifiedAccess>]
type TForm =
    | Fixed of repeat: int * ColumnType
    /// A P (32-bit) or Q (64-bit, wide) descriptor of an array of the given type.
    | Variable of ColumnType * maxLength: int option * wide: bool

    /// The element type.
    member this.Type: ColumnType =
        match this with
        | Fixed(_, t) -> t
        | Variable(t, _, _) -> t

    /// Bytes each cell occupies in a row.
    member this.Width: int =
        match this with
        | Fixed(repeat, t) -> t.BytesFor repeat
        | Variable(_, _, wide) -> if wide then 16 else 8

    /// Elements per cell for fixed forms. One for variable forms, whose cell is the descriptor.
    member this.Repeat: int =
        match this with
        | Fixed(repeat, _) -> repeat
        | Variable _ -> 1

/// Parsing and formatting of TFORM values.
[<RequireQualifiedAccess>]
module TForm =
    let private pattern =
        Regex(@"^([0-9]*)([LXBIJKAEDCMPQ])(.*)$", RegexOptions.CultureInvariant)

    let private variablePattern =
        Regex(@"^([LXBIJKAEDCM])(?:\(([0-9]+)\))?", RegexOptions.CultureInvariant)

    /// Parses a TFORM value. Characters after a fixed form are ignored, as the standard allows.
    let tryParse (text: string) : TForm option =
        let m = pattern.Match(text.Trim())

        if not m.Success then
            None
        else
            let repeat =
                if m.Groups[1].Value = "" then
                    Some 1
                else
                    match Int32.TryParse m.Groups[1].Value with
                    | true, r -> Some r
                    | _ -> None

            let code = m.Groups[2].Value[0]

            match repeat, code with
            | None, _ -> None
            | Some repeat, ('P' | 'Q') ->
                let vm = variablePattern.Match m.Groups[3].Value

                if not vm.Success || repeat <> 1 then
                    None
                else
                    let elementType = (ColumnType.TryOfCode vm.Groups[1].Value[0]).Value

                    let maxLength =
                        if vm.Groups[2].Success then
                            Some(int vm.Groups[2].Value)
                        else
                            None

                    Some(TForm.Variable(elementType, maxLength, code = 'Q'))
            | Some repeat, code -> ColumnType.TryOfCode code |> Option.map (fun t -> TForm.Fixed(repeat, t))

    /// The TFORM value for a form.
    let format (form: TForm) : string =
        match form with
        | TForm.Fixed(repeat, t) -> $"{repeat}{t.Code}"
        | TForm.Variable(t, maxLength, wide) ->
            let suffix =
                match maxLength with
                | Some m -> $"({m})"
                | None -> ""

            (if wide then "1Q" else "1P") + string t.Code + suffix

/// What the header says about one column.
type ColumnInfo = {
    /// One-based column number, as in TFORMn.
    Number: int
    /// TTYPEn, or empty when absent.
    Name: string
    Form: TForm
    /// TUNITn.
    Unit: string option
    /// TNULLn, the stored integer that means undefined.
    Null: int64 option
    /// TSCALn.
    Scale: float
    /// TZEROn.
    Zero: float
    /// TDIMn, the dimensions of a cell in FITS order.
    Dim: int64[] option
    /// TDISPn.
    Display: string option
}

/// The values of a column, one array across every row. For a fixed form with repeat r the arrays
/// hold rows * r elements, row-major. Strings hold one entry per row.
[<RequireQualifiedAccess>]
type ColumnData =
    /// Logical cells. None is the undefined value.
    | Logicals of bool option[]
    /// Bit cells, unpacked.
    | Bits of bool[]
    | UInt8s of byte[]
    | Int16s of int16[]
    | Int32s of int32[]
    | Int64s of int64[]
    /// Character cells, one string per row, with trailing spaces and NULs removed.
    | Strings of string[]
    | Float32s of float32[]
    | Float64s of float[]
    | Complex32s of Complex[]
    | Complex64s of Complex[]
    /// Variable-length arrays, one entry per row, each holding that row's elements.
    | Variable of ColumnData[]

    /// Number of entries in the underlying array.
    member this.Length: int =
        match this with
        | Logicals a -> a.Length
        | Bits a -> a.Length
        | UInt8s a -> a.Length
        | Int16s a -> a.Length
        | Int32s a -> a.Length
        | Int64s a -> a.Length
        | Strings a -> a.Length
        | Float32s a -> a.Length
        | Float64s a -> a.Length
        | Complex32s a -> a.Length
        | Complex64s a -> a.Length
        | Variable a -> a.Length

    /// The element type, or None for variable-length data.
    member this.Type: ColumnType option =
        match this with
        | Logicals _ -> Some ColumnType.Logical
        | Bits _ -> Some ColumnType.Bit
        | UInt8s _ -> Some ColumnType.UInt8
        | Int16s _ -> Some ColumnType.Int16
        | Int32s _ -> Some ColumnType.Int32
        | Int64s _ -> Some ColumnType.Int64
        | Strings _ -> Some ColumnType.Char
        | Float32s _ -> Some ColumnType.Float32
        | Float64s _ -> Some ColumnType.Float64
        | Complex32s _ -> Some ColumnType.Complex32
        | Complex64s _ -> Some ColumnType.Complex64
        | Variable _ -> None

/// A column with its values.
type Column = {
    Info: ColumnInfo
    Data: ColumnData
} with

    /// Number of rows.
    member this.Rows: int =
        match this.Data, this.Info.Form with
        | ColumnData.Strings a, _ -> a.Length
        | ColumnData.Variable a, _ -> a.Length
        | data, TForm.Fixed(repeat, _) -> if repeat = 0 then 0 else data.Length / repeat
        | data, TForm.Variable _ -> data.Length

/// A binary table with its data unit loaded. Columns are decoded on request.
type BinaryTable = {
    Header: Header
    /// NAXIS2.
    Rows: int
    /// NAXIS1, bytes per row.
    RowLength: int
    Columns: ColumnInfo[]
    /// Byte offset of each column within a row.
    Offsets: int[]
    /// Offset of the heap from the start of the data unit.
    HeapOffset: int
    /// The entire data unit: rows, any gap, and the heap.
    Bytes: byte[]
}

/// Reading, building and encoding of binary tables.
[<RequireQualifiedAccess>]
module Table =
    let private dimPattern =
        Regex(@"^\(([0-9]+(?:,[0-9]+)*)\)$", RegexOptions.CultureInvariant)

    /// Reads one column's descriptors from a header.
    let private columnOfHeader (header: Header) (locate: Violation -> Issue) (n: int) : ColumnInfo option * Issue seq =
        let form, formIssues =
            match Header.tryString $"TFORM{n}" header with
            | Some text ->
                match TForm.tryParse text with
                | Some form -> Some form, Seq.empty
                | None -> None, Seq.singleton (locate (Violation.InvalidTForm(n, text)))
            | None -> None, Seq.singleton (locate (Violation.MissingKeyword $"TFORM{n}"))

        let dim, dimParseIssues =
            match Header.tryString $"TDIM{n}" header with
            | Some text ->
                let m = dimPattern.Match(text.Trim())

                if m.Success then
                    Some(m.Groups[1].Value.Split ',' |> Array.map int64), Seq.empty
                else
                    None,
                    Seq.singleton (
                        locate (Violation.InvalidKeywordValue($"TDIM{n}", $"'{text}' is not a dimension list"))
                    )
            | None -> None, Seq.empty

        // TDIMn has to multiply out to the repeat count, whatever the column holds. For a
        // character column the fastest axis is the string width, so the rule is the same.
        let dimIssues =
            match form, dim with
            | Some(TForm.Fixed(repeat, _)), Some d when (d |> Array.fold (*) 1L) <> int64 repeat ->
                Seq.singleton (
                    locate (
                        Violation.InvalidKeywordValue(
                            $"TDIM{n}",
                            $"the dimensions multiply out to {d |> Array.fold (*) 1L}, not the repeat count {repeat}"
                        )
                    )
                )
            | _ -> Seq.empty

        let column =
            form
            |> Option.map (fun form -> {
                Number = n
                Name = defaultArg (Header.tryString $"TTYPE{n}" header) ""
                Form = form
                Unit = Header.tryString $"TUNIT{n}" header
                Null = Header.tryInt $"TNULL{n}" header
                Scale = defaultArg (Header.tryFloat $"TSCAL{n}" header) 1.0
                Zero = defaultArg (Header.tryFloat $"TZERO{n}" header) 0.0
                Dim = dim
                Display = Header.tryString $"TDISP{n}" header
            })

        column, Seq.concat [ formIssues; dimParseIssues; dimIssues ]

    /// Reads the column descriptions from a header with the given TFIELDS.
    let private columnsOfHeader
        (strictness: Strictness)
        (hduIndex: int)
        (header: Header)
        : Result<ColumnInfo[] * Issue list, Issue list> =
        let locate (violation: Violation) = { Issue.create violation with Hdu = Some hduIndex }

        let fields, fieldsIssues =
            match Header.tryInt "TFIELDS" header with
            | Some n when n >= 0L && n <= 999L -> int n, Seq.empty
            | Some n -> 0, Seq.singleton (locate (Violation.InvalidKeywordValue("TFIELDS", $"{n} is outside 0..999")))
            | None -> 0, Seq.singleton (locate (Violation.MissingKeyword "TFIELDS"))

        let columns = [ for n in 1..fields -> columnOfHeader header locate n ]
        let issues = Seq.append fieldsIssues (columns |> Seq.collect snd) |> List.ofSeq
        let fatal = issues |> List.exists Issue.isFatal
        let complete = columns |> List.forall (fst >> Option.isSome)

        if complete && not fatal && (strictness = Lenient || issues.IsEmpty) then
            Ok(columns |> List.map (fst >> Option.get) |> Array.ofList, issues)
        else
            Error issues

    /// Interprets the data unit of a BINTABLE HDU. Under strict handling any violation is an
    /// error. Under lenient handling recoverable violations come back as warnings.
    let tryOfHdu
        (strictness: Strictness)
        (info: HduInfo)
        (bytes: byte[])
        : Result<BinaryTable * Issue list, Issue list> =
        if info.Kind <> HduKind.BinaryTable then
            invalidOp $"HDU {info.Index} is a {info.Kind}, not a binary table"

        let locate (violation: Violation) = { Issue.create violation with Hdu = Some info.Index }

        match columnsOfHeader strictness info.Index info.Header with
        | Error issues -> Error issues
        | Ok(columns, warnings) ->
            let rowLength = int info.Axes[0]
            let rows = int info.Axes[1]
            let offsets = columns |> Array.scan (fun offset c -> offset + c.Form.Width) 0
            let occupied = int64 offsets[columns.Length]
            let tableBytes = int64 rowLength * int64 rows

            let heapOffset, heapIssues =
                match Header.tryInt "THEAP" info.Header with
                | Some h when h >= tableBytes && h <= int64 bytes.Length -> int h, Seq.empty
                | Some h ->
                    int tableBytes,
                    Seq.singleton (
                        locate (Violation.InvalidKeywordValue("THEAP", $"{h} is outside the data unit after the rows"))
                    )
                | None -> int tableBytes, Seq.empty

            let issues =
                seq {
                    yield! warnings

                    if occupied <> int64 rowLength then
                        yield locate (Violation.RowLengthMismatch(int64 rowLength, occupied))

                    yield! heapIssues
                }
                |> List.ofSeq

            let fatal = issues |> List.exists Issue.isFatal

            if fatal || (strictness = Strict && not issues.IsEmpty) then
                Error issues
            else
                Ok(
                    {
                        Header = info.Header
                        Rows = rows
                        RowLength = rowLength
                        Columns = columns
                        Offsets = Array.sub offsets 0 columns.Length
                        HeapOffset = heapOffset
                        Bytes = bytes
                    },
                    issues
                )

    /// Decodes a string cell, dropping trailing spaces and NULs.
    let private decodeString (bytes: byte[]) (at: int) (width: int) : string =
        let mutable last = at + width

        while last > at && (bytes[last - 1] = 0uy || bytes[last - 1] = 0x20uy) do
            last <- last - 1

        Encoding.Latin1.GetString(bytes, at, last - at)

    /// Decodes `rows` cells of `repeat` elements each, where cell r starts at offset + r * stride.
    let private decodeCells
        (bytes: byte[])
        (rows: int)
        (stride: int)
        (offset: int)
        (repeat: int)
        (t: ColumnType)
        : ColumnData =

        match t with
        | ColumnType.Logical ->
            let arr = Array.zeroCreate<bool option>(rows * repeat)

            for r in 0 .. rows - 1 do
                let at = r * stride + offset

                for i in 0 .. repeat - 1 do
                    arr[r * repeat + i] <-
                        match bytes[at + i] with
                        | 0x54uy -> Some true
                        | 0x46uy -> Some false
                        | _ -> None

            ColumnData.Logicals arr
        | ColumnType.Bit ->
            let arr = Array.zeroCreate<bool>(rows * repeat)

            for r in 0 .. rows - 1 do
                let at = r * stride + offset

                for i in 0 .. repeat - 1 do
                    arr[r * repeat + i] <- (bytes[at + i / 8] >>> (7 - i % 8)) &&& 1uy = 1uy

            ColumnData.Bits arr
        | ColumnType.UInt8 ->
            let arr = Array.zeroCreate<byte>(rows * repeat)

            for r in 0 .. rows - 1 do
                Array.blit bytes (r * stride + offset) arr (r * repeat) repeat

            ColumnData.UInt8s arr
        | ColumnType.Int16 ->
            let arr = Array.zeroCreate<int16>(rows * repeat)

            for r in 0 .. rows - 1 do
                let at = r * stride + offset

                for i in 0 .. repeat - 1 do
                    arr[r * repeat + i] <- BinaryPrimitives.ReadInt16BigEndian(ReadOnlySpan<byte>(bytes, at + 2 * i, 2))

            ColumnData.Int16s arr
        | ColumnType.Int32 ->
            let arr = Array.zeroCreate<int32>(rows * repeat)

            for r in 0 .. rows - 1 do
                let at = r * stride + offset

                for i in 0 .. repeat - 1 do
                    arr[r * repeat + i] <- BinaryPrimitives.ReadInt32BigEndian(ReadOnlySpan<byte>(bytes, at + 4 * i, 4))

            ColumnData.Int32s arr
        | ColumnType.Int64 ->
            let arr = Array.zeroCreate<int64>(rows * repeat)

            for r in 0 .. rows - 1 do
                let at = r * stride + offset

                for i in 0 .. repeat - 1 do
                    arr[r * repeat + i] <- BinaryPrimitives.ReadInt64BigEndian(ReadOnlySpan<byte>(bytes, at + 8 * i, 8))

            ColumnData.Int64s arr
        | ColumnType.Char ->
            ColumnData.Strings(Array.init rows (fun r -> decodeString bytes (r * stride + offset) repeat))
        | ColumnType.Float32 ->
            let arr = Array.zeroCreate<float32>(rows * repeat)

            for r in 0 .. rows - 1 do
                let at = r * stride + offset

                for i in 0 .. repeat - 1 do
                    arr[r * repeat + i] <-
                        BinaryPrimitives.ReadSingleBigEndian(ReadOnlySpan<byte>(bytes, at + 4 * i, 4))

            ColumnData.Float32s arr
        | ColumnType.Float64 ->
            let arr = Array.zeroCreate<float>(rows * repeat)

            for r in 0 .. rows - 1 do
                let at = r * stride + offset

                for i in 0 .. repeat - 1 do
                    arr[r * repeat + i] <-
                        BinaryPrimitives.ReadDoubleBigEndian(ReadOnlySpan<byte>(bytes, at + 8 * i, 8))

            ColumnData.Float64s arr
        | ColumnType.Complex32 ->
            let arr = Array.zeroCreate<Complex>(rows * repeat)

            for r in 0 .. rows - 1 do
                let at = r * stride + offset

                for i in 0 .. repeat - 1 do
                    let re =
                        BinaryPrimitives.ReadSingleBigEndian(ReadOnlySpan<byte>(bytes, at + 8 * i, 4))
                    let im =
                        BinaryPrimitives.ReadSingleBigEndian(ReadOnlySpan<byte>(bytes, at + 8 * i + 4, 4))
                    arr[r * repeat + i] <- Complex(float re, float im)

            ColumnData.Complex32s arr
        | ColumnType.Complex64 ->
            let arr = Array.zeroCreate<Complex>(rows * repeat)

            for r in 0 .. rows - 1 do
                let at = r * stride + offset

                for i in 0 .. repeat - 1 do
                    let re =
                        BinaryPrimitives.ReadDoubleBigEndian(ReadOnlySpan<byte>(bytes, at + 16 * i, 8))
                    let im =
                        BinaryPrimitives.ReadDoubleBigEndian(ReadOnlySpan<byte>(bytes, at + 16 * i + 8, 8))
                    arr[r * repeat + i] <- Complex(re, im)

            ColumnData.Complex64s arr

    /// Decodes the column at a zero-based index.
    let column (index: int) (table: BinaryTable) : Column =
        let info = table.Columns[index]
        let offset = table.Offsets[index]

        let data =
            match info.Form with
            | TForm.Fixed(repeat, t) -> decodeCells table.Bytes table.Rows table.RowLength offset repeat t
            | TForm.Variable(t, _, wide) ->
                let items =
                    Array.init
                        table.Rows
                        (fun r ->
                            let at = r * table.RowLength + offset

                            let count, heapOffset =
                                if wide then
                                    BinaryPrimitives.ReadInt64BigEndian(ReadOnlySpan<byte>(table.Bytes, at, 8)),
                                    BinaryPrimitives.ReadInt64BigEndian(ReadOnlySpan<byte>(table.Bytes, at + 8, 8))
                                else
                                    int64 (BinaryPrimitives.ReadInt32BigEndian(ReadOnlySpan<byte>(table.Bytes, at, 4))),
                                    int64 (
                                        BinaryPrimitives.ReadInt32BigEndian(ReadOnlySpan<byte>(table.Bytes, at + 4, 4))
                                    )

                            let start = int64 table.HeapOffset + heapOffset

                            if
                                count < 0L
                                || heapOffset < 0L
                                || count > int64 Int32.MaxValue
                                || start + int64 (t.BytesFor(int count)) > int64 table.Bytes.Length
                            then
                                raise (
                                    FitsException(
                                        Violations [ Issue.create (Violation.InvalidDescriptor(info.Number, r)) ]
                                    )
                                )

                            decodeCells table.Bytes 1 0 (int start) (int count) t
                        )

                ColumnData.Variable items

        { Info = info; Data = data }

    /// The column with this name, ignoring case.
    let tryColumn (name: string) (table: BinaryTable) : Column option =
        table.Columns
        |> Array.tryFindIndex (fun c -> String.Equals(c.Name, name, StringComparison.OrdinalIgnoreCase))
        |> Option.map (fun i -> column i table)

    /// Every column, decoded.
    let columns (table: BinaryTable) : Column list = [ for i in 0 .. table.Columns.Length - 1 -> column i table ]

    /// Physical values of a numeric column as doubles: TZERO + TSCAL * stored, with TNULL mapped to NaN.
    let toFloat64 (column: Column) : float[] =
        let zero = column.Info.Zero
        let scale = column.Info.Scale
        let inline scaled (x: float) = zero + scale * x

        let nullable (toInt64: 'T -> int64) (toFloat: 'T -> float) (values: 'T[]) =
            match column.Info.Null with
            | Some blank ->
                values
                |> Array.map (fun v -> if toInt64 v = blank then nan else scaled (toFloat v))
            | None -> values |> Array.map (fun v -> scaled (toFloat v))

        match column.Data with
        | ColumnData.UInt8s a -> nullable int64 float a
        | ColumnData.Int16s a -> nullable int64 float a
        | ColumnData.Int32s a -> nullable int64 float a
        | ColumnData.Int64s a -> nullable id float a
        | ColumnData.Float32s a -> a |> Array.map (fun v -> scaled (float v))
        | ColumnData.Float64s a -> a |> Array.map scaled
        | _ -> invalidOp $"column '{column.Info.Name}' is not numeric"

    /// The shape of one cell in C order. TDIMn declares the dimensions fastest first, the way
    /// NAXISn does, so this is their reverse; without TDIMn a cell is one run of the repeat count.
    /// For a character column the fastest axis is the string width.
    let cellShape (info: ColumnInfo) : int[] =
        match info.Dim with
        | Some dim when dim.Length > 0 -> dim |> Array.rev |> Array.map int
        | _ -> [| info.Form.Repeat |]

    /// One cell of a numeric column as an image: its values with the shape TDIMn gives them and
    /// the scaling TSCALn and TZEROn declare, so the image functions apply to a table cell the
    /// same way they do to an image HDU.
    let cellImage (row: int) (column: Column) : Image =
        let info = column.Info
        let shape = cellShape info
        let repeat = info.Form.Repeat

        let data =
            match column.Data with
            | ColumnData.UInt8s a -> ImageData.UInt8(Array.sub a (row * repeat) repeat)
            | ColumnData.Int16s a -> ImageData.Int16(Array.sub a (row * repeat) repeat)
            | ColumnData.Int32s a -> ImageData.Int32(Array.sub a (row * repeat) repeat)
            | ColumnData.Int64s a -> ImageData.Int64(Array.sub a (row * repeat) repeat)
            | ColumnData.Float32s a -> ImageData.Float32(Array.sub a (row * repeat) repeat)
            | ColumnData.Float64s a -> ImageData.Float64(Array.sub a (row * repeat) repeat)
            | _ -> invalidOp $"column '{info.Name}' does not hold numbers a cell image can be made of"

        {
            Shape = shape
            Data = data
            BZero = info.Zero
            BScale = info.Scale
            Blank = info.Null
        }

    // The standard has no unsigned integer or signed byte TFORM. Table 18 instead writes the
    // value offset by half the range and says so with TZEROn, which is what these recognise.

    let private offsetBy (t: ColumnType) (zero: float) (column: Column) =
        column.Data.Type = Some t && column.Info.Zero = zero && column.Info.Scale = 1.0

    /// True when the descriptors describe signed bytes stored as unsigned, TZERO = -128.
    let isInt8 (column: Column) : bool = offsetBy ColumnType.UInt8 -128.0 column

    /// True when the descriptors describe unsigned 16-bit integers stored as signed.
    let isUInt16 (column: Column) : bool =
        offsetBy ColumnType.Int16 32768.0 column

    /// True when the descriptors describe unsigned 32-bit integers stored as signed.
    let isUInt32 (column: Column) : bool =
        offsetBy ColumnType.Int32 2147483648.0 column

    /// True when the descriptors describe unsigned 64-bit integers stored as signed.
    let isUInt64 (column: Column) : bool =
        offsetBy ColumnType.Int64 9223372036854775808.0 column

    /// The signed byte values of a column stored with TZERO = -128.
    let toInt8 (column: Column) : sbyte[] =
        match column.Data with
        | ColumnData.UInt8s a when isInt8 column -> a |> Array.map (fun v -> sbyte (int v - 128))
        | _ -> invalidOp $"column '{column.Info.Name}' is not stored as signed bytes"

    /// The unsigned 16-bit values of a column stored with TZERO = 32768.
    let toUInt16 (column: Column) : uint16[] =
        match column.Data with
        | ColumnData.Int16s a when isUInt16 column -> a |> Array.map (fun v -> uint16 (int v + 32768))
        | _ -> invalidOp $"column '{column.Info.Name}' is not stored as unsigned 16-bit integers"

    /// The unsigned 32-bit values of a column stored with TZERO = 2147483648.
    let toUInt32 (column: Column) : uint32[] =
        match column.Data with
        | ColumnData.Int32s a when isUInt32 column -> a |> Array.map (fun v -> uint32 (int64 v + 2147483648L))
        | _ -> invalidOp $"column '{column.Info.Name}' is not stored as unsigned 32-bit integers"

    /// The unsigned 64-bit values of a column stored with TZERO = 9223372036854775808. The offset
    /// is exactly 2^63, so flipping the sign bit is the whole conversion.
    let toUInt64 (column: Column) : uint64[] =
        match column.Data with
        | ColumnData.Int64s a when isUInt64 column -> a |> Array.map (fun v -> uint64 v ^^^ 0x8000000000000000UL)
        | _ -> invalidOp $"column '{column.Info.Name}' is not stored as unsigned 64-bit integers"

    // Building

    /// A column with the given form and data and no other descriptors.
    let create (name: string) (form: TForm) (data: ColumnData) : Column = {
        Info = {
            Number = 0
            Name = name
            Form = form
            Unit = None
            Null = None
            Scale = 1.0
            Zero = 0.0
            Dim = None
            Display = None
        }
        Data = data
    }

    /// A column of one scalar per row.
    let private scalar (name: string) (t: ColumnType) (data: ColumnData) = create name (TForm.Fixed(1, t)) data

    let ofLogicals (name: string) (values: bool option[]) : Column =
        scalar name ColumnType.Logical (ColumnData.Logicals values)
    let ofUInt8s (name: string) (values: byte[]) : Column =
        scalar name ColumnType.UInt8 (ColumnData.UInt8s values)
    let ofInt16s (name: string) (values: int16[]) : Column =
        scalar name ColumnType.Int16 (ColumnData.Int16s values)
    let ofInt32s (name: string) (values: int32[]) : Column =
        scalar name ColumnType.Int32 (ColumnData.Int32s values)
    let ofInt64s (name: string) (values: int64[]) : Column =
        scalar name ColumnType.Int64 (ColumnData.Int64s values)
    let ofFloat32s (name: string) (values: float32[]) : Column =
        scalar name ColumnType.Float32 (ColumnData.Float32s values)
    let ofFloat64s (name: string) (values: float[]) : Column =
        scalar name ColumnType.Float64 (ColumnData.Float64s values)

    /// A column of strings padded to the given width.
    let ofStrings (name: string) (width: int) (values: string[]) : Column =
        create name (TForm.Fixed(width, ColumnType.Char)) (ColumnData.Strings values)

    /// A column of variable-length arrays of one type, one per row.
    let ofVariable (name: string) (t: ColumnType) (rows: ColumnData[]) : Column =
        create name (TForm.Variable(t, None, false)) (ColumnData.Variable rows)

    let withUnit (unit: string) (column: Column) : Column = { column with Info = { column.Info with Unit = Some unit } }
    let withNull (value: int64) (column: Column) : Column = {
        column with
            Info = { column.Info with Null = Some value }
    }
    let withScaling (zero: float) (scale: float) (column: Column) : Column = {
        column with
            Info = { column.Info with Zero = zero; Scale = scale }
    }
    let withDim (dim: int64[]) (column: Column) : Column = { column with Info = { column.Info with Dim = Some dim } }
    let withDisplay (display: string) (column: Column) : Column = {
        column with
            Info = { column.Info with Display = Some display }
    }

    /// A column of signed bytes, stored as unsigned with TZERO = -128.
    let ofInt8s (name: string) (values: sbyte[]) : Column =
        let stored = values |> Array.map (fun v -> byte (int v + 128))
        scalar name ColumnType.UInt8 (ColumnData.UInt8s stored)
        |> withScaling -128.0 1.0

    /// A column of unsigned 16-bit values, stored as signed with TZERO = 32768.
    let ofUInt16s (name: string) (values: uint16[]) : Column =
        let stored = values |> Array.map (fun v -> int16 (int v - 32768))
        scalar name ColumnType.Int16 (ColumnData.Int16s stored)
        |> withScaling 32768.0 1.0

    /// A column of unsigned 32-bit values, stored as signed with TZERO = 2147483648.
    let ofUInt32s (name: string) (values: uint32[]) : Column =
        let stored = values |> Array.map (fun v -> int32 (int64 v - 2147483648L))
        scalar name ColumnType.Int32 (ColumnData.Int32s stored)
        |> withScaling 2147483648.0 1.0

    /// A column of unsigned 64-bit values, stored as signed with TZERO = 9223372036854775808.
    let ofUInt64s (name: string) (values: uint64[]) : Column =
        let stored = values |> Array.map (fun v -> int64 (v ^^^ 0x8000000000000000UL))

        scalar name ColumnType.Int64 (ColumnData.Int64s stored)
        |> withScaling 9223372036854775808.0 1.0

    /// The TFORMn, TTYPEn and related cards for columns, numbered from one in order.
    let descriptorCards (columns: ColumnInfo seq) : Card list = [
        for i, c in Seq.indexed columns do
            let n = i + 1
            Card.Value($"TFORM{n}", CardValue.String(TForm.format c.Form), Some "data format of field")

            if c.Name <> "" then
                Card.Value($"TTYPE{n}", CardValue.String c.Name, Some "label for field")

            match c.Unit with
            | Some u -> Card.Value($"TUNIT{n}", CardValue.String u, Some "physical unit of field")
            | None -> ()

            match c.Null with
            | Some v -> Card.Value($"TNULL{n}", CardValue.Integer v, Some "undefined value for field")
            | None -> ()

            if c.Scale <> 1.0 then
                Card.Value($"TSCAL{n}", CardValue.Real c.Scale, Some "physical = TZERO + TSCAL * stored")

            if c.Zero <> 0.0 then
                Card.Value($"TZERO{n}", CardValue.Real c.Zero, Some "physical = TZERO + TSCAL * stored")

            match c.Dim with
            | Some d ->
                Card.Value($"TDIM{n}", CardValue.String("(" + String.Join(",", d) + ")"), Some "dimensions of field")
            | None -> ()

            match c.Display with
            | Some d -> Card.Value($"TDISP{n}", CardValue.String d, Some "display format of field")
            | None -> ()
    ]

    /// Writes `count` elements of data starting at element `start` into bytes at `at`.
    let private encodeElements (data: ColumnData) (start: int) (count: int) (bytes: byte[]) (at: int) =

        match data with
        | ColumnData.Logicals a ->
            for i in 0 .. count - 1 do
                bytes[at + i] <-
                    match a[start + i] with
                    | Some true -> 0x54uy
                    | Some false -> 0x46uy
                    | None -> 0uy
        | ColumnData.Bits a ->
            for i in 0 .. count - 1 do
                if a[start + i] then
                    bytes[at + i / 8] <- bytes[at + i / 8] ||| (0x80uy >>> (i % 8))
        | ColumnData.UInt8s a -> Array.blit a start bytes at count
        | ColumnData.Int16s a ->
            for i in 0 .. count - 1 do
                BinaryPrimitives.WriteInt16BigEndian(Span<byte>(bytes, at + 2 * i, 2), a[start + i])
        | ColumnData.Int32s a ->
            for i in 0 .. count - 1 do
                BinaryPrimitives.WriteInt32BigEndian(Span<byte>(bytes, at + 4 * i, 4), a[start + i])
        | ColumnData.Int64s a ->
            for i in 0 .. count - 1 do
                BinaryPrimitives.WriteInt64BigEndian(Span<byte>(bytes, at + 8 * i, 8), a[start + i])
        | ColumnData.Strings a ->
            // One string fills the whole run of `count` characters, space padded.
            let encoded = Encoding.Latin1.GetBytes a[start]

            if encoded.Length > count then
                invalidArg (nameof data) $"string '{a[start]}' is longer than the column width {count}"

            Array.blit encoded 0 bytes at encoded.Length
            Array.fill bytes (at + encoded.Length) (count - encoded.Length) 0x20uy
        | ColumnData.Float32s a ->
            for i in 0 .. count - 1 do
                BinaryPrimitives.WriteSingleBigEndian(Span<byte>(bytes, at + 4 * i, 4), a[start + i])
        | ColumnData.Float64s a ->
            for i in 0 .. count - 1 do
                BinaryPrimitives.WriteDoubleBigEndian(Span<byte>(bytes, at + 8 * i, 8), a[start + i])
        | ColumnData.Complex32s a ->
            for i in 0 .. count - 1 do
                BinaryPrimitives.WriteSingleBigEndian(Span<byte>(bytes, at + 8 * i, 4), float32 a[start + i].Real)
                BinaryPrimitives.WriteSingleBigEndian(
                    Span<byte>(bytes, at + 8 * i + 4, 4),
                    float32 a[start + i].Imaginary
                )
        | ColumnData.Complex64s a ->
            for i in 0 .. count - 1 do
                BinaryPrimitives.WriteDoubleBigEndian(Span<byte>(bytes, at + 16 * i, 8), a[start + i].Real)
                BinaryPrimitives.WriteDoubleBigEndian(Span<byte>(bytes, at + 16 * i + 8, 8), a[start + i].Imaginary)
        | ColumnData.Variable _ -> invalidOp "variable-length data is encoded through the heap"

    /// Number of elements an entry of data holds, as the heap stores them.
    let private elementCount (data: ColumnData) : int =
        match data with
        | ColumnData.Strings [| s |] -> s.Length
        | ColumnData.Strings _ -> invalidArg (nameof data) "a variable-length string entry holds exactly one string"
        | ColumnData.Variable _ -> invalidArg (nameof data) "variable-length entries cannot nest"
        | data -> data.Length

    /// Checks that every column's data matches its form and that all columns agree on the row count.
    let private validate (columns: Column list) : int =
        let rows =
            columns
            |> List.map (fun c ->
                match c.Data.Type, c.Info.Form with
                | Some t, TForm.Fixed(_, expected) when t <> expected ->
                    invalidArg (nameof columns) $"column '{c.Info.Name}' holds {t} data but its form is {expected}"
                | Some t, TForm.Variable _ ->
                    invalidArg (nameof columns) $"column '{c.Info.Name}' has a variable form but holds fixed {t} data"
                | None, TForm.Fixed _ ->
                    invalidArg
                        (nameof columns)
                        $"column '{c.Info.Name}' has a fixed form but holds variable-length data"
                | _ -> ()

                match c.Info.Dim, c.Info.Form with
                | Some dim, TForm.Fixed(repeat, _) when (dim |> Array.fold (*) 1L) <> int64 repeat ->
                    invalidArg
                        (nameof columns)
                        $"column '{c.Info.Name}' has dimensions multiplying out to {dim |> Array.fold (*) 1L}, not its repeat count {repeat}"
                | _ -> ()

                match c.Data, c.Info.Form with
                | ColumnData.Variable entries, TForm.Variable(expected, _, _) ->
                    for entry in entries do
                        if entry.Type <> Some expected then
                            invalidArg (nameof columns) $"column '{c.Info.Name}' has an entry of the wrong type"
                | ColumnData.Strings _, _ -> ()
                | data, TForm.Fixed(repeat, _) when repeat > 0 && data.Length % repeat <> 0 ->
                    invalidArg
                        (nameof columns)
                        $"column '{c.Info.Name}' has {data.Length} elements, not a multiple of its repeat {repeat}"
                | _ -> ()

                // A zero-repeat cell carries no elements, so such a column cannot tell the row count.
                match c.Data, c.Info.Form with
                | ColumnData.Strings _, _
                | ColumnData.Variable _, _ -> Some c.Rows
                | _, TForm.Fixed(0, _) -> None
                | _ -> Some c.Rows
            )
            |> List.choose id
            |> List.distinct

        match rows with
        | [] -> 0
        | [ rows ] -> rows
        | _ -> invalidArg (nameof columns) $"columns disagree on the row count: %A{rows}"

    /// Encodes columns into row bytes and heap bytes, numbering the columns from one. Returns the
    /// descriptors, the row count, the row length, the row bytes and the heap bytes. Columns whose
    /// cells hold no elements cannot tell the row count, so a table made only of those has no rows.
    let encode (columns: Column list) : ColumnInfo[] * int * int * byte[] * byte[] =
        let rows = validate columns
        let infos =
            columns |> List.mapi (fun i c -> { c.Info with Number = i + 1 }) |> Array.ofList
        let widths = infos |> Array.map (fun c -> c.Form.Width)
        let offsets = widths |> Array.scan (+) 0
        let rowLength = offsets[widths.Length]
        let bytes = Array.zeroCreate<byte>(rows * rowLength)
        let heap = ResizeArray<byte>()

        columns
        |> List.iteri (fun i c ->
            let offset = offsets[i]

            match c.Data, c.Info.Form with
            | ColumnData.Variable entries, TForm.Variable(t, _, wide) ->
                for r in 0 .. rows - 1 do
                    let count = elementCount entries[r]
                    let size = t.BytesFor count
                    let heapOffset = heap.Count
                    let chunk = Array.zeroCreate<byte> size
                    encodeElements entries[r] 0 count chunk 0
                    heap.AddRange chunk
                    let at = r * rowLength + offset

                    if wide then
                        BinaryPrimitives.WriteInt64BigEndian(Span<byte>(bytes, at, 8), int64 count)
                        BinaryPrimitives.WriteInt64BigEndian(Span<byte>(bytes, at + 8, 8), int64 heapOffset)
                    else
                        BinaryPrimitives.WriteInt32BigEndian(Span<byte>(bytes, at, 4), count)
                        BinaryPrimitives.WriteInt32BigEndian(Span<byte>(bytes, at + 4, 4), heapOffset)
            | ColumnData.Strings _, TForm.Fixed(width, _) ->
                for r in 0 .. rows - 1 do
                    encodeElements c.Data r width bytes (r * rowLength + offset)
            | data, TForm.Fixed(repeat, _) ->
                for r in 0 .. rows - 1 do
                    encodeElements data (r * repeat) repeat bytes (r * rowLength + offset)
            | _ -> ()
        )

        infos, rows, rowLength, bytes, heap.ToArray()
