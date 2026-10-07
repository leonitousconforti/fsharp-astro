namespace FSharp.Astro.Fits

open System
open System.Globalization
open System.Text
open System.Text.RegularExpressions

// ASCII tables, XTENSION = 'TABLE', as section 7.2 of the standard describes them. A row is a line
// of printable characters; TBCOLn says which byte a field starts at and TFORMn is a Fortran edit
// descriptor saying how wide it is and how to read it.
//
// Decoding gives back the same Column type a binary table does, so everything downstream, from
// Table.toFloat64 to record mapping to Quantity.column, works on both kinds of table.

/// A TFORMn value of an ASCII table: a Fortran edit descriptor.
[<RequireQualifiedAccess>]
type AsciiForm =
    /// Aw, a character string w bytes wide.
    | Char of width: int
    /// Iw, a whole number right justified in w bytes.
    | Integer of width: int
    /// Fw.d, a fixed point number with d digits after the point.
    | Fixed of width: int * decimals: int
    /// Ew.d, a single precision number in exponential form.
    | Exponential of width: int * decimals: int
    /// Dw.d, a double precision number in exponential form, whose exponent may be written with D.
    | Double of width: int * decimals: int

    /// The edit descriptor letter.
    member this.Code: char =
        match this with
        | Char _ -> 'A'
        | Integer _ -> 'I'
        | Fixed _ -> 'F'
        | Exponential _ -> 'E'
        | Double _ -> 'D'

    /// Bytes the field occupies in a row.
    member this.Width: int =
        match this with
        | Char w
        | Integer w
        | Fixed(w, _)
        | Exponential(w, _)
        | Double(w, _) -> w

    /// Digits after the decimal point, for the forms that have them.
    member this.Decimals: int option =
        match this with
        | Char _
        | Integer _ -> None
        | Fixed(_, d)
        | Exponential(_, d)
        | Double(_, d) -> Some d

/// Parsing and formatting of ASCII table TFORM values.
[<RequireQualifiedAccess>]
module AsciiForm =
    let private pattern =
        Regex(@"^([AIFED])([0-9]+)(?:\.([0-9]+))?$", RegexOptions.CultureInvariant)

    /// Parses a TFORM value such as "I6", "F8.2" or "A20". Returns None for anything else.
    let tryParse (text: string) : AsciiForm option =
        let m = pattern.Match(text.Trim())

        if not m.Success then
            None
        else
            let width = int m.Groups[2].Value

            let decimals =
                if m.Groups[3].Success then
                    Some(int m.Groups[3].Value)
                else
                    None

            if width <= 0 then
                None
            else
                match m.Groups[1].Value[0], decimals with
                | 'A', None -> Some(AsciiForm.Char width)
                | 'I', None -> Some(AsciiForm.Integer width)
                | 'F', Some d when d < width -> Some(AsciiForm.Fixed(width, d))
                | 'E', Some d when d < width -> Some(AsciiForm.Exponential(width, d))
                | 'D', Some d when d < width -> Some(AsciiForm.Double(width, d))
                | _ -> None

    /// The TFORM value for a form.
    let format (form: AsciiForm) : string =
        match form.Decimals with
        | Some d -> $"{form.Code}{form.Width}.{d}"
        | None -> $"{form.Code}{form.Width}"

/// What the header says about one field of an ASCII table.
type AsciiColumnInfo = {
    /// One-based field number, as in TFORMn.
    Number: int
    /// TTYPEn, or empty when absent.
    Name: string
    /// TBCOLn, the one-based byte of the row the field starts at.
    Start: int
    Form: AsciiForm
    /// TUNITn.
    Unit: string option
    /// TNULLn, the exact characters that mean undefined. A field of all blanks is undefined too.
    Null: string option
    /// TSCALn.
    Scale: float
    /// TZEROn.
    Zero: float
    /// TDISPn.
    Display: string option
}

/// An ASCII table with its data unit loaded. Fields are decoded on request.
type AsciiTable = {
    Header: Header
    /// NAXIS2.
    Rows: int
    /// NAXIS1, bytes per row.
    RowLength: int
    Columns: AsciiColumnInfo[]
    /// The entire data unit, one row of characters after another.
    Bytes: byte[]
}

/// A field with its values. Strings hold one entry per row, integers and reals one value per row.
type AsciiColumn = {
    Info: AsciiColumnInfo
    Data: ColumnData
} with

    /// Number of rows.
    member this.Rows: int = this.Data.Length

/// Reading, building and encoding of ASCII tables.
[<RequireQualifiedAccess>]
module Ascii =
    /// The value an undefined integer decodes to. An ASCII table has no in-band integer null, so
    /// the decoder picks one and records it as the TNULL of the Column it hands back, which is
    /// what makes an option field and Table.toFloat64 treat the cell as undefined.
    [<Literal>]
    let NullInteger = Int64.MinValue

    /// Reads one field's descriptors from a header.
    let private columnOfHeader
        (header: Header)
        (locate: Violation -> Issue)
        (rowLength: int)
        (n: int)
        : AsciiColumnInfo option * Issue seq =
        let form, formIssues =
            match Header.tryString $"TFORM{n}" header with
            | Some text ->
                match AsciiForm.tryParse text with
                | Some form -> Some form, Seq.empty
                | None -> None, Seq.singleton (locate (Violation.InvalidTForm(n, text)))
            | None -> None, Seq.singleton (locate (Violation.MissingKeyword $"TFORM{n}"))

        let start, startIssues =
            match Header.tryInt $"TBCOL{n}" header with
            | Some value when value >= 1L && value <= int64 Int32.MaxValue -> Some(int value), Seq.empty
            | Some value ->
                None,
                Seq.singleton (locate (Violation.InvalidKeywordValue($"TBCOL{n}", $"{value} is not a byte position")))
            | None -> None, Seq.singleton (locate (Violation.MissingKeyword $"TBCOL{n}"))

        let fitIssues =
            match form, start with
            | Some form, Some start when start - 1 + form.Width > rowLength ->
                Seq.singleton (
                    locate (
                        Violation.InvalidKeywordValue(
                            $"TBCOL{n}",
                            $"a {form.Width} byte field at {start} runs past the {rowLength} byte row"
                        )
                    )
                )
            | _ -> Seq.empty

        let column =
            match form, start with
            | Some form, Some start ->
                Some {
                    Number = n
                    Name = defaultArg (Header.tryString $"TTYPE{n}" header) ""
                    Start = start
                    Form = form
                    Unit = Header.tryString $"TUNIT{n}" header
                    Null = Header.tryString $"TNULL{n}" header
                    Scale = defaultArg (Header.tryFloat $"TSCAL{n}" header) 1.0
                    Zero = defaultArg (Header.tryFloat $"TZERO{n}" header) 0.0
                    Display = Header.tryString $"TDISP{n}" header
                }
            | _ -> None

        column, Seq.concat [ formIssues; startIssues; fitIssues ]

    /// Reads the field descriptions from a header with the given TFIELDS.
    let private columnsOfHeader
        (strictness: Strictness)
        (hduIndex: int)
        (rowLength: int)
        (header: Header)
        : Result<AsciiColumnInfo[] * Issue list, Issue list> =
        let locate (violation: Violation) = { Issue.create violation with Hdu = Some hduIndex }

        let fields, fieldsIssues =
            match Header.tryInt "TFIELDS" header with
            | Some n when n >= 0L && n <= 999L -> int n, Seq.empty
            | Some n -> 0, Seq.singleton (locate (Violation.InvalidKeywordValue("TFIELDS", $"{n} is outside 0..999")))
            | None -> 0, Seq.singleton (locate (Violation.MissingKeyword "TFIELDS"))

        let columns = [ for n in 1..fields -> columnOfHeader header locate rowLength n ]

        // The standard forbids fields that overlap. Those that do would still decode, so this is
        // recoverable rather than fatal.
        let overlapIssues =
            columns
            |> List.choose fst
            |> List.sortBy (fun c -> c.Start)
            |> List.pairwise
            |> List.choose (fun (a, b) ->
                if a.Start + a.Form.Width > b.Start then
                    Some(
                        locate (
                            Violation.InvalidKeywordValue(
                                $"TBCOL{b.Number}",
                                $"field {b.Number} overlaps field {a.Number}"
                            )
                        )
                    )
                else
                    None
            )

        let issues =
            Seq.concat [ fieldsIssues; columns |> Seq.collect snd; Seq.ofList overlapIssues ]
            |> List.ofSeq

        let fatal = issues |> List.exists Issue.isFatal
        let complete = columns |> List.forall (fst >> Option.isSome)

        if complete && not fatal && (strictness = Lenient || issues.IsEmpty) then
            Ok(columns |> List.map (fst >> Option.get) |> Array.ofList, issues)
        else
            Error issues

    /// Interprets the data unit of a TABLE HDU. Under strict handling any violation is an error.
    /// Under lenient handling recoverable violations come back as warnings.
    let tryOfHdu
        (strictness: Strictness)
        (info: HduInfo)
        (bytes: byte[])
        : Result<AsciiTable * Issue list, Issue list> =
        if info.Kind <> HduKind.AsciiTable then
            invalidOp $"HDU {info.Index} is a {info.Kind}, not an ASCII table"

        let rowLength = int info.Axes[0]
        let rows = int info.Axes[1]

        match columnsOfHeader strictness info.Index rowLength info.Header with
        | Error issues -> Error issues
        | Ok(columns, warnings) ->
            Ok(
                {
                    Header = info.Header
                    Rows = rows
                    RowLength = rowLength
                    Columns = columns
                    Bytes = bytes
                },
                warnings
            )

    /// The characters of one field of one row, with the surrounding blanks removed.
    let private field (table: AsciiTable) (info: AsciiColumnInfo) (row: int) : string =
        let at = row * table.RowLength + info.Start - 1
        let width = min info.Form.Width (max 0 (table.Bytes.Length - at))

        if at < 0 || width <= 0 then
            ""
        else
            Encoding.Latin1.GetString(table.Bytes, at, width)

    /// True when a field is undefined: all blanks, or exactly the TNULLn string once both are
    /// trimmed, which is how files that pad TNULL differently from the field still work.
    let private isNull (info: AsciiColumnInfo) (text: string) : bool =
        let trimmed = text.Trim()

        trimmed = ""
        || (
            match info.Null with
            | Some n -> trimmed = n.Trim()
            | None -> false
        )

    /// Reads a number written in any of the forms Fortran accepts, including a D exponent and an
    /// exponent with no letter at all, as in "1.5+10".
    let private tryNumber (text: string) : float option =
        let cleaned = text.Trim().Replace("D", "E").Replace("d", "e")

        let withExponent =
            // A sign after a digit and no E before it is Fortran's exponent with the letter left out.
            if cleaned.IndexOfAny([| 'e'; 'E' |]) < 0 then
                let signAt =
                    seq { 1 .. cleaned.Length - 1 }
                    |> Seq.tryFind (fun i -> (cleaned[i] = '+' || cleaned[i] = '-') && Char.IsDigit cleaned[i - 1])

                match signAt with
                | Some i -> cleaned.Insert(i, "E")
                | None -> cleaned
            else
                cleaned

        match Double.TryParse(withExponent, NumberStyles.Float, CultureInfo.InvariantCulture) with
        | true, value -> Some value
        | _ -> None

    /// Decodes the field at a zero-based index into the same Column a binary table would give.
    let column (index: int) (table: AsciiTable) : Column =
        let info = table.Columns[index]
        let text row = field table info row

        let data =
            match info.Form with
            | AsciiForm.Char _ -> ColumnData.Strings(Array.init table.Rows (fun r -> (text r).TrimEnd(' ', '\000')))
            | AsciiForm.Integer _ ->
                ColumnData.Int64s(
                    Array.init
                        table.Rows
                        (fun r ->
                            let value = text r

                            if isNull info value then
                                NullInteger
                            else
                                match
                                    Int64.TryParse(value.Trim(), NumberStyles.Integer, CultureInfo.InvariantCulture)
                                with
                                | true, parsed -> parsed
                                | _ -> NullInteger
                        )
                )
            | AsciiForm.Fixed(_, decimals)
            | AsciiForm.Exponential(_, decimals)
            | AsciiForm.Double(_, decimals) ->
                ColumnData.Float64s(
                    Array.init
                        table.Rows
                        (fun r ->
                            let value = text r

                            if isNull info value then
                                nan
                            else
                                match tryNumber value with
                                | Some parsed ->
                                    // Fortran takes the last d digits of a number written without a
                                    // decimal point as the fraction.
                                    if decimals > 0 && not (value.Contains '.') then
                                        parsed / (10.0 ** float decimals)
                                    else
                                        parsed
                                | None -> nan
                        )
                )

        let form =
            match info.Form with
            | AsciiForm.Char width -> TForm.Fixed(width, ColumnType.Char)
            | AsciiForm.Integer _ -> TForm.Fixed(1, ColumnType.Int64)
            | _ -> TForm.Fixed(1, ColumnType.Float64)

        {
            Info = {
                Number = info.Number
                Name = info.Name
                Form = form
                Unit = info.Unit
                Null =
                    match info.Form with
                    | AsciiForm.Integer _ -> Some NullInteger
                    | _ -> None
                Scale = info.Scale
                Zero = info.Zero
                Dim = None
                Display = info.Display
            }
            Data = data
        }

    /// The field with this name, ignoring case.
    let tryColumn (name: string) (table: AsciiTable) : Column option =
        table.Columns
        |> Array.tryFindIndex (fun c -> String.Equals(c.Name, name, StringComparison.OrdinalIgnoreCase))
        |> Option.map (fun i -> column i table)

    /// Every field, decoded.
    let columns (table: AsciiTable) : Column list = [ for i in 0 .. table.Columns.Length - 1 -> column i table ]

    // Building

    /// A field with the given form and data and no other descriptors. TBCOLn is assigned when the
    /// table is encoded, so the position given here is a placeholder.
    let create (name: string) (form: AsciiForm) (data: ColumnData) : AsciiColumn = {
        Info = {
            Number = 0
            Name = name
            Start = 1
            Form = form
            Unit = None
            Null = None
            Scale = 1.0
            Zero = 0.0
            Display = None
        }
        Data = data
    }

    /// A field of whole numbers w bytes wide.
    let ofInt64s (name: string) (width: int) (values: int64[]) : AsciiColumn =
        create name (AsciiForm.Integer width) (ColumnData.Int64s values)

    /// A field of fixed point numbers, w bytes wide with d digits after the point.
    let ofFloat64s (name: string) (width: int) (decimals: int) (values: float[]) : AsciiColumn =
        create name (AsciiForm.Fixed(width, decimals)) (ColumnData.Float64s values)

    /// A field of numbers in exponential form, w bytes wide with d digits of mantissa.
    let ofExponentials (name: string) (width: int) (decimals: int) (values: float[]) : AsciiColumn =
        create name (AsciiForm.Exponential(width, decimals)) (ColumnData.Float64s values)

    /// A field of strings w bytes wide.
    let ofStrings (name: string) (width: int) (values: string[]) : AsciiColumn =
        create name (AsciiForm.Char width) (ColumnData.Strings values)

    let withUnit (unit: string) (column: AsciiColumn) : AsciiColumn = {
        column with
            Info = { column.Info with Unit = Some unit }
    }

    /// The characters this field writes for an undefined value, which must fit its width.
    let withNull (text: string) (column: AsciiColumn) : AsciiColumn = {
        column with
            Info = { column.Info with Null = Some text }
    }

    let withScaling (zero: float) (scale: float) (column: AsciiColumn) : AsciiColumn = {
        column with
            Info = { column.Info with Zero = zero; Scale = scale }
    }

    let withDisplay (display: string) (column: AsciiColumn) : AsciiColumn = {
        column with
            Info = { column.Info with Display = Some display }
    }

    /// The TFORMn, TBCOLn, TTYPEn and related cards for fields, numbered from one in order.
    let descriptorCards (columns: AsciiColumnInfo seq) : Card list = [
        for c in columns do
            let n = c.Number
            Card.Value($"TBCOL{n}", CardValue.Integer(int64 c.Start), Some "beginning column of field")
            Card.Value($"TFORM{n}", CardValue.String(AsciiForm.format c.Form), Some "data format of field")

            if c.Name <> "" then
                Card.Value($"TTYPE{n}", CardValue.String c.Name, Some "label for field")

            match c.Unit with
            | Some u -> Card.Value($"TUNIT{n}", CardValue.String u, Some "physical unit of field")
            | None -> ()

            match c.Null with
            | Some v -> Card.Value($"TNULL{n}", CardValue.String v, Some "undefined value for field")
            | None -> ()

            if c.Scale <> 1.0 then
                Card.Value($"TSCAL{n}", CardValue.Real c.Scale, Some "physical = TZERO + TSCAL * stored")

            if c.Zero <> 0.0 then
                Card.Value($"TZERO{n}", CardValue.Real c.Zero, Some "physical = TZERO + TSCAL * stored")

            match c.Display with
            | Some d -> Card.Value($"TDISP{n}", CardValue.String d, Some "display format of field")
            | None -> ()
    ]

    /// The exponent form writes a mantissa of d digits and a two digit exponent, as Fortran does.
    let private exponential (decimals: int) (value: float) : string =
        let digits = String('0', decimals)
        let text = value.ToString($"0.{digits}E+00", CultureInfo.InvariantCulture)
        if decimals = 0 then text.Replace(".", "") else text

    /// Renders one value, right justified for numbers and left justified for strings.
    let private render (info: AsciiColumnInfo) (data: ColumnData) (row: int) : string =
        let width = info.Form.Width

        let blank () = String(' ', width)

        let justify (text: string) =
            if text.Length > width then
                invalidArg
                    (nameof data)
                    $"field '{info.Name}' needs {text.Length} bytes for '{text}' but is {width} wide"
            else
                text.PadLeft width

        match info.Form, data with
        | AsciiForm.Char _, ColumnData.Strings values ->
            let text = values[row]

            if text.Length > width then
                invalidArg (nameof data) $"field '{info.Name}' is {width} wide but '{text}' is longer"
            else
                text.PadRight width
        | AsciiForm.Integer _, ColumnData.Int64s values ->
            if values[row] = NullInteger then
                match info.Null with
                | Some text -> justify text
                | None -> blank ()
            else
                justify (values[row].ToString(CultureInfo.InvariantCulture))
        | (AsciiForm.Fixed _ | AsciiForm.Exponential _ | AsciiForm.Double _), ColumnData.Float64s values ->
            let value = values[row]

            if Double.IsNaN value then
                match info.Null with
                | Some text -> justify text
                | None -> blank ()
            else
                match info.Form with
                | AsciiForm.Fixed(_, d) -> justify (value.ToString($"F{d}", CultureInfo.InvariantCulture))
                | AsciiForm.Exponential(_, d) -> justify (exponential d value)
                | _ -> justify ((exponential (info.Form.Decimals |> Option.defaultValue 0) value).Replace("E", "D"))
        | form, data ->
            invalidArg (nameof data) $"field '{info.Name}' has form {AsciiForm.format form} but holds {data.Type} data"

    /// Checks that every field holds data its form can write and that all agree on the row count.
    let private validate (columns: AsciiColumn list) : int =
        let rows =
            columns
            |> List.map (fun c ->
                match c.Info.Form, c.Data with
                | AsciiForm.Char _, ColumnData.Strings _
                | AsciiForm.Integer _, ColumnData.Int64s _
                | AsciiForm.Fixed _, ColumnData.Float64s _
                | AsciiForm.Exponential _, ColumnData.Float64s _
                | AsciiForm.Double _, ColumnData.Float64s _ -> ()
                | form, data ->
                    invalidArg
                        (nameof columns)
                        $"field '{c.Info.Name}' has form {AsciiForm.format form} but holds {data.Type} data"

                c.Rows
            )
            |> List.distinct

        match rows with
        | [] -> 0
        | [ rows ] -> rows
        | _ -> invalidArg (nameof columns) $"fields disagree on the row count: %A{rows}"

    /// Encodes fields into row bytes, numbering them from one and laying them out left to right
    /// with one blank between, which is what makes the rows readable. Returns the descriptors with
    /// TBCOLn filled in, the row count, the row length and the bytes.
    let encode (columns: AsciiColumn list) : AsciiColumnInfo[] * int * int * byte[] =
        let rows = validate columns

        let infos =
            let mutable at = 1

            columns
            |> List.mapi (fun i c ->
                let start = at
                at <- at + c.Info.Form.Width + 1
                { c.Info with Number = i + 1; Start = start }
            )
            |> Array.ofList

        let rowLength =
            if infos.Length = 0 then
                0
            else
                let last = infos[infos.Length - 1]
                last.Start - 1 + last.Form.Width

        let bytes = Array.create<byte> (rows * rowLength) 0x20uy

        columns
        |> List.iteri (fun i c ->
            let info = infos[i]

            for r in 0 .. rows - 1 do
                let text = render info c.Data r
                let at = r * rowLength + info.Start - 1
                Encoding.Latin1.GetBytes(text, 0, text.Length, bytes, at) |> ignore
        )

        infos, rows, rowLength, bytes
