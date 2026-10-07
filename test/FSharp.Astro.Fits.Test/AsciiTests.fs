module AsciiTests

open System
open System.Text
open Xunit
open FSharp.Astro.Fits
open FSharp.Astro.Units
open Generators

/// An HduInfo for a hand-built ASCII table data unit, so the reader can be driven directly.
let private hdu (cards: (string * CardValue) list) (rowLength: int) (rows: int) : HduInfo = {
    Index = 1
    Kind = HduKind.AsciiTable
    Header = Header.ofCards (cards |> List.map (fun (k, v) -> Card.Value(k, v, None)))
    BitPix = BitPix.UInt8
    Axes = [| int64 rowLength; int64 rows |]
    PCount = 0L
    GCount = 1L
    HeaderOffset = 0L
    HeaderLength = int64 Block.Size
    DataOffset = int64 Block.Size
    DataLength = int64 (rowLength * rows)
}

let private rows (lines: string list) : byte[] =
    Encoding.Latin1.GetBytes(String.concat "" lines)

[<Fact>]
let ``parses Fortran edit descriptors`` () =
    Assert.Equal(Some(AsciiForm.Char 20), AsciiForm.tryParse "A20")
    Assert.Equal(Some(AsciiForm.Integer 6), AsciiForm.tryParse " I6 ")
    Assert.Equal(Some(AsciiForm.Fixed(8, 2)), AsciiForm.tryParse "F8.2")
    Assert.Equal(Some(AsciiForm.Exponential(12, 5)), AsciiForm.tryParse "E12.5")
    Assert.Equal(Some(AsciiForm.Double(20, 12)), AsciiForm.tryParse "D20.12")
    // A width must be positive, a decimal count must fit inside the width, and only AIFED exist.
    Assert.Equal(None, AsciiForm.tryParse "A0")
    Assert.Equal(None, AsciiForm.tryParse "F4.4")
    Assert.Equal(None, AsciiForm.tryParse "I6.2")
    Assert.Equal(None, AsciiForm.tryParse "F8")
    Assert.Equal(None, AsciiForm.tryParse "J6")
    Assert.Equal(None, AsciiForm.tryParse "")

[<Fact>]
let ``formats edit descriptors back`` () =
    Assert.Equal("A20", AsciiForm.format (AsciiForm.Char 20))
    Assert.Equal("I6", AsciiForm.format (AsciiForm.Integer 6))
    Assert.Equal("F8.2", AsciiForm.format (AsciiForm.Fixed(8, 2)))
    Assert.Equal("E12.5", AsciiForm.format (AsciiForm.Exponential(12, 5)))
    Assert.Equal("D20.12", AsciiForm.format (AsciiForm.Double(20, 12)))

    for text in [ "A20"; "I6"; "F8.2"; "E12.5"; "D20.12" ] do
        Assert.Equal(Some text, AsciiForm.tryParse text |> Option.map AsciiForm.format)

let private catalog = [
    Ascii.ofStrings "NAME" 6 [| "alpha"; "beta" |]
    Ascii.ofInt64s "ID" 4 [| 1L; -22L |]
    Ascii.ofFloat64s "RA" 8 3 [| 10.5; 266.417 |] |> Ascii.withUnit "deg"
    Ascii.ofExponentials "FLUX" 11 4 [| 1.5e-3; 2.0 |] |> Ascii.withUnit "Jy"
]

[<Fact>]
let ``writes an ASCII table that reads back`` () =
    let bytes =
        Fits.toBytes [
            Fits.emptyPrimary Header.empty
            Fits.asciiTable (Header.set "EXTNAME" (CardValue.String "CAT") Header.empty) catalog
        ]

    use file = Fits.openBytes bytes
    let info = file[1]
    Assert.Equal(HduKind.AsciiTable, info.Kind)
    Assert.Equal(BitPix.UInt8, info.BitPix)
    // 6 + 1 + 4 + 1 + 8 + 1 + 11, one blank between fields.
    Assert.Equal<int64[]>([| 32L; 2L |], info.Axes)
    Assert.Equal(0L, info.PCount)

    let table = Fits.readAsciiTable file 1
    Assert.Equal(2, table.Rows)
    Assert.Equal(32, table.RowLength)
    Assert.Equal<int[]>([| 1; 8; 13; 22 |], table.Columns |> Array.map (fun c -> c.Start))

    match (Ascii.tryColumn "NAME" table).Value.Data with
    | ColumnData.Strings values -> Assert.Equal<string[]>([| "alpha"; "beta" |], values)
    | other -> failwith $"expected strings but got %A{other}"

    match (Ascii.tryColumn "ID" table).Value.Data with
    | ColumnData.Int64s values -> Assert.Equal<int64[]>([| 1L; -22L |], values)
    | other -> failwith $"expected integers but got %A{other}"

    Assert.Equal<float[]>([| 10.5; 266.417 |], Table.toFloat64 (Ascii.tryColumn "RA" table).Value)
    Assert.Equal<float[]>([| 1.5e-3; 2.0 |], Table.toFloat64 (Ascii.tryColumn "FLUX" table).Value)

[<Fact>]
let ``the written rows are readable text`` () =
    let bytes =
        Fits.toBytes [ Fits.emptyPrimary Header.empty; Fits.asciiTable Header.empty catalog ]

    use file = Fits.openBytes bytes
    let table = Fits.readAsciiTable file 1
    let line r =
        Encoding.Latin1.GetString(table.Bytes, r * table.RowLength, table.RowLength)
    Assert.Equal("alpha     1   10.500  1.5000E-03", line 0)
    Assert.Equal("beta    -22  266.417  2.0000E+00", line 1)

[<Fact>]
let ``field descriptors are written including TBCOL`` () =
    let bytes =
        Fits.toBytes [ Fits.emptyPrimary Header.empty; Fits.asciiTable Header.empty catalog ]

    use file = Fits.openBytes bytes
    let header = file[1].Header
    Assert.Equal(Some "TABLE", Header.tryString "XTENSION" header)
    Assert.Equal(Some 4L, Header.tryInt "TFIELDS" header)
    Assert.Equal(Some 1L, Header.tryInt "TBCOL1" header)
    Assert.Equal(Some 13L, Header.tryInt "TBCOL3" header)
    Assert.Equal(Some "F8.3", Header.tryString "TFORM3" header)
    Assert.Equal(Some "RA", Header.tryString "TTYPE3" header)
    Assert.Equal(Some "deg", Header.tryString "TUNIT3" header)

[<Fact>]
let ``a blank field and a TNULL field are undefined`` () =
    let columns = [
        Ascii.ofInt64s "N" 4 [| 7L; Ascii.NullInteger; Ascii.NullInteger |]
        |> Ascii.withNull "----"
        Ascii.ofFloat64s "X" 6 1 [| 1.5; nan; 2.5 |]
    ]

    let bytes =
        Fits.toBytes [ Fits.emptyPrimary Header.empty; Fits.asciiTable Header.empty columns ]

    use file = Fits.openBytes bytes
    let table = Fits.readAsciiTable file 1

    match (Ascii.tryColumn "N" table).Value.Data with
    | ColumnData.Int64s values -> Assert.Equal<int64[]>([| 7L; Ascii.NullInteger; Ascii.NullInteger |], values)
    | other -> failwith $"expected integers but got %A{other}"

    // The Column carries that null as its TNULL, so the usual scaling maps it to NaN.
    let scaled = Table.toFloat64 (Ascii.tryColumn "N" table).Value
    Assert.Equal(7.0, scaled[0])
    Assert.True(Double.IsNaN scaled[1])

    let x = Table.toFloat64 (Ascii.tryColumn "X" table).Value
    Assert.Equal<float[]>([| 1.5; 2.5 |], [| x[0]; x[2] |])
    Assert.True(Double.IsNaN x[1])

[<Fact>]
let ``reads the number forms Fortran writes`` () =
    let info =
        hdu
            [
                "XTENSION", CardValue.String "TABLE"
                "BITPIX", CardValue.Integer 8L
                "NAXIS", CardValue.Integer 2L
                "NAXIS1", CardValue.Integer 14L
                "NAXIS2", CardValue.Integer 4L
                "PCOUNT", CardValue.Integer 0L
                "GCOUNT", CardValue.Integer 1L
                "TFIELDS", CardValue.Integer 1L
                "TBCOL1", CardValue.Integer 1L
                "TFORM1", CardValue.String "D14.4"
            ]
            14
            4

    let data =
        rows [
            "    1.5000D+02"
            "    1.5000E+02"
            "       1.50+02"
            "        123456" // no decimal point, so the last four digits are the fraction
        ]

    let table, _ = Ascii.tryOfHdu Strict info data |> expectOk
    let values = Table.toFloat64 (Ascii.column 0 table)
    Assert.Equal<float[]>([| 150.0; 150.0; 150.0; 12.3456 |], values)

[<Fact>]
let ``fields may sit anywhere in the row and leave gaps`` () =
    let info =
        hdu
            [
                "XTENSION", CardValue.String "TABLE"
                "BITPIX", CardValue.Integer 8L
                "NAXIS", CardValue.Integer 2L
                "NAXIS1", CardValue.Integer 12L
                "NAXIS2", CardValue.Integer 2L
                "PCOUNT", CardValue.Integer 0L
                "GCOUNT", CardValue.Integer 1L
                "TFIELDS", CardValue.Integer 2L
                "TBCOL1", CardValue.Integer 9L
                "TFORM1", CardValue.String "I4"
                "TTYPE1", CardValue.String "ID"
                "TBCOL2", CardValue.Integer 1L
                "TFORM2", CardValue.String "A3"
                "TTYPE2", CardValue.String "TAG"
            ]
            12
            2

    let table, _ =
        Ascii.tryOfHdu Strict info (rows [ "ab      1234"; "cd      5678" ]) |> expectOk

    match (Ascii.tryColumn "TAG" table).Value.Data with
    | ColumnData.Strings values -> Assert.Equal<string[]>([| "ab"; "cd" |], values)
    | other -> failwith $"expected strings but got %A{other}"

    match (Ascii.tryColumn "ID" table).Value.Data with
    | ColumnData.Int64s values -> Assert.Equal<int64[]>([| 1234L; 5678L |], values)
    | other -> failwith $"expected integers but got %A{other}"

let private badLayout (cards: (string * CardValue) list) =
    hdu
        ([
            "XTENSION", CardValue.String "TABLE"
            "BITPIX", CardValue.Integer 8L
            "NAXIS", CardValue.Integer 2L
            "NAXIS1", CardValue.Integer 10L
            "NAXIS2", CardValue.Integer 1L
            "PCOUNT", CardValue.Integer 0L
            "GCOUNT", CardValue.Integer 1L
         ]
         @ cards)
        10
        1

[<Fact>]
let ``a field that runs past the row is a violation`` () =
    let info =
        badLayout [
            "TFIELDS", CardValue.Integer 1L
            "TBCOL1", CardValue.Integer 6L
            "TFORM1", CardValue.String "I8"
        ]

    let issues = Ascii.tryOfHdu Strict info (rows [ "          " ]) |> expectError
    Assert.Contains(issues, fun i -> (Issue.describe i).Contains "TBCOL1")
    // Lenient reads it anyway and reports the violation as a warning.
    let _, warnings = Ascii.tryOfHdu Lenient info (rows [ "          " ]) |> expectOk
    Assert.NotEmpty warnings

[<Fact>]
let ``overlapping fields are a violation`` () =
    let info =
        badLayout [
            "TFIELDS", CardValue.Integer 2L
            "TBCOL1", CardValue.Integer 1L
            "TFORM1", CardValue.String "I4"
            "TBCOL2", CardValue.Integer 3L
            "TFORM2", CardValue.String "I4"
        ]

    let issues = Ascii.tryOfHdu Strict info (rows [ "          " ]) |> expectError
    Assert.Contains(issues, fun i -> (Issue.describe i).Contains "overlaps")

[<Fact>]
let ``a missing or unreadable descriptor is a violation`` () =
    let missing =
        badLayout [ "TFIELDS", CardValue.Integer 1L; "TFORM1", CardValue.String "I4" ]

    let issues = Ascii.tryOfHdu Strict missing (rows [ "          " ]) |> expectError
    Assert.Contains(issues, fun i -> (Issue.describe i).Contains "TBCOL1")

    let unreadable =
        badLayout [
            "TFIELDS", CardValue.Integer 1L
            "TBCOL1", CardValue.Integer 1L
            "TFORM1", CardValue.String "Z4"
        ]

    let issues = Ascii.tryOfHdu Strict unreadable (rows [ "          " ]) |> expectError
    Assert.Contains(issues, fun i -> (Issue.describe i).Contains "TFORM")

/// An Iw field decodes as int64 and the real forms as float, so that is what a record declares.
type Source = { Name: string; Id: int64; Ra: float; Flux: float option }

[<Fact>]
let ``records map over an ASCII table the same as a binary one`` () =
    let columns = [
        Ascii.ofStrings "NAME" 6 [| "alpha"; "beta" |]
        Ascii.ofInt64s "ID" 4 [| 1L; 2L |]
        Ascii.ofFloat64s "RA" 8 3 [| 10.5; 266.417 |]
        Ascii.ofFloat64s "FLUX" 8 2 [| 1.25; nan |]
    ]

    let bytes =
        Fits.toBytes [ Fits.emptyPrimary Header.empty; Fits.asciiTable Header.empty columns ]

    use file = Fits.openBytes bytes
    let sources = Fits.readRecords<Source> file 1
    Assert.Equal(2, sources.Length)
    Assert.Equal({ Name = "alpha"; Id = 1L; Ra = 10.5; Flux = Some 1.25 }, sources[0])
    Assert.Equal({ Name = "beta"; Id = 2L; Ra = 266.417; Flux = None }, sources[1])

[<Fact>]
let ``an ASCII field carries its unit of measure like any other`` () =
    let bytes =
        Fits.toBytes [ Fits.emptyPrimary Header.empty; Fits.asciiTable Header.empty catalog ]

    use file = Fits.openBytes bytes
    let table = Fits.readAsciiTable file 1
    let flux = (Ascii.tryColumn "FLUX" table).Value
    Assert.Equal<float<mJy>[]>([| 1.5<mJy>; 2000.0<mJy> |], Quantity.column Tag.mJy flux)

[<Fact>]
let ``TSCAL and TZERO give physical values`` () =
    let columns = [ Ascii.ofInt64s "C" 6 [| 10L; 20L |] |> Ascii.withScaling 100.0 0.5 ]

    let bytes =
        Fits.toBytes [ Fits.emptyPrimary Header.empty; Fits.asciiTable Header.empty columns ]

    use file = Fits.openBytes bytes
    let table = Fits.readAsciiTable file 1
    Assert.Equal<float[]>([| 105.0; 110.0 |], Table.toFloat64 (Ascii.column 0 table))

[<Fact>]
let ``reading the wrong kind of HDU as an ASCII table fails`` () =
    let bytes =
        Fits.toBytes [
            Fits.emptyPrimary Header.empty
            Fits.binaryTable Header.empty [ Table.ofInt32s "ID" [| 1 |] ]
        ]

    use file = Fits.openBytes bytes
    Assert.True((Fits.tryReadAsciiTable file 1).IsError)
    Assert.Throws<FitsException>(fun () -> Fits.readAsciiTable file 1 |> ignore)
    |> ignore

[<Fact>]
let ``a value too wide for its field is refused`` () =
    let columns = [ Ascii.ofStrings "NAME" 3 [| "far too long" |] ]

    Assert.Throws<ArgumentException>(fun () -> Fits.asciiTable Header.empty columns |> ignore)
    |> ignore

    Assert.Throws<ArgumentException>(fun () ->
        Fits.asciiTable Header.empty [ Ascii.ofInt64s "N" 2 [| 123456L |] ] |> ignore
    )
    |> ignore
