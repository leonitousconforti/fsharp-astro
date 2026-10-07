module TableTests

open System
open System.Numerics
open Xunit
open FsCheck
open FsCheck.FSharp
open FSharp.Astro.Fits
open Generators

[<Fact>]
let ``parses TFORM values`` () =
    Assert.Equal(Some(TForm.Fixed(1, ColumnType.Int32)), TForm.tryParse "1J")
    Assert.Equal(Some(TForm.Fixed(1, ColumnType.Float32)), TForm.tryParse "E")
    Assert.Equal(Some(TForm.Fixed(10, ColumnType.Char)), TForm.tryParse "10A")
    Assert.Equal(Some(TForm.Fixed(3, ColumnType.Int16)), TForm.tryParse "3Iextra")
    Assert.Equal(Some(TForm.Variable(ColumnType.Float32, Some 100, false)), TForm.tryParse "PE(100)")
    Assert.Equal(Some(TForm.Variable(ColumnType.Float64, None, true)), TForm.tryParse "1QD")
    Assert.Equal(None, TForm.tryParse "Z")
    Assert.Equal(None, TForm.tryParse "2PE")
    Assert.Equal(None, TForm.tryParse "")

[<Fact>]
let ``computes cell widths`` () =
    Assert.Equal(1, (TForm.Fixed(3, ColumnType.Bit)).Width)
    Assert.Equal(2, (TForm.Fixed(9, ColumnType.Bit)).Width)
    Assert.Equal(24, (TForm.Fixed(3, ColumnType.Float64)).Width)
    Assert.Equal(8, (TForm.Variable(ColumnType.Int16, None, false)).Width)
    Assert.Equal(16, (TForm.Variable(ColumnType.Int16, None, true)).Width)

[<Fact>]
let ``formats TFORM values`` () =
    Assert.Equal("1J", TForm.format (TForm.Fixed(1, ColumnType.Int32)))
    Assert.Equal("12A", TForm.format (TForm.Fixed(12, ColumnType.Char)))
    Assert.Equal("1PE(7)", TForm.format (TForm.Variable(ColumnType.Float32, Some 7, false)))
    Assert.Equal("1QK", TForm.format (TForm.Variable(ColumnType.Int64, None, true)))

let columns = [
    Table.ofInt32s "ID" [| 1; 2 |]
    Table.ofStrings "NAME" 4 [| "ab"; "cdef" |]
    Table.ofFloat32s "FLUX" [| 1.5f; -2.0f |] |> Table.withUnit "Jy"
]

[<Fact>]
let ``writes a table that reads back`` () =
    let bytes =
        Fits.toBytes [
            Fits.emptyPrimary Header.empty
            Fits.binaryTable (Header.set "EXTNAME" (CardValue.String "CAT") Header.empty) columns
        ]
    use file = Fits.openBytes bytes
    let hdu = file[1]
    Assert.Equal(HduKind.BinaryTable, hdu.Kind)
    Assert.Equal<int64[]>([| 12L; 2L |], hdu.Axes)
    Assert.Equal(0L, hdu.PCount)
    Assert.Equal(24L, hdu.DataLength)

    Assert.Equal<string list>(
        [
            "XTENSION"
            "BITPIX"
            "NAXIS"
            "NAXIS1"
            "NAXIS2"
            "PCOUNT"
            "GCOUNT"
            "TFIELDS"
            "TFORM1"
            "TTYPE1"
            "TFORM2"
            "TTYPE2"
            "TFORM3"
            "TTYPE3"
            "TUNIT3"
            "EXTNAME"
        ],
        Header.keywords hdu.Header
    )

    let table = Fits.readTable file 1
    Assert.Equal(2, table.Rows)
    Assert.Equal(12, table.RowLength)
    Assert.Equal<int[]>([| 0; 4; 8 |], table.Offsets)
    Assert.Equal(24, table.HeapOffset)
    Assert.Equal<byte[]>(
        [|
            0uy
            0uy
            0uy
            1uy
            0x61uy
            0x62uy
            0x20uy
            0x20uy
            0x3Fuy
            0xC0uy
            0uy
            0uy
        |],
        Array.sub table.Bytes 0 12
    )

    Assert.Equal(ColumnData.Int32s [| 1; 2 |], (Table.column 0 table).Data)
    Assert.Equal(ColumnData.Strings [| "ab"; "cdef" |], (Table.column 1 table).Data)
    let flux = (Table.tryColumn "flux" table).Value
    Assert.Equal(ColumnData.Float32s [| 1.5f; -2.0f |], flux.Data)
    Assert.Equal(Some "Jy", flux.Info.Unit)
    Assert.Equal(3, flux.Info.Number)
    Assert.Equal(3, (Table.columns table).Length)

[<Fact>]
let ``applies TSCAL TZERO and TNULL`` () =
    let column =
        Table.ofInt16s "T" [| 0s; 10s; -1s |]
        |> Table.withScaling 100.0 0.5
        |> Table.withNull -1L
    use file =
        Fits.openBytes (Fits.toBytes [ Fits.emptyPrimary Header.empty; Fits.binaryTable Header.empty [ column ] ])
    let read = Table.column 0 (Fits.readTable file 1)
    Assert.Equal(Some -1L, read.Info.Null)
    let values = Table.toFloat64 read
    Assert.Equal(100.0, values[0])
    Assert.Equal(105.0, values[1])
    Assert.True(Double.IsNaN values[2])

[<Fact>]
let ``variable-length arrays go through the heap`` () =
    let column =
        Table.ofVariable "SPEC" ColumnType.Float32 [|
            ColumnData.Float32s [| 1.0f; 2.0f; 3.0f |]
            ColumnData.Float32s [||]
            ColumnData.Float32s [| 4.0f |]
        |]

    let names =
        Table.ofVariable "NOTE" ColumnType.Char [|
            ColumnData.Strings [| "hello" |]
            ColumnData.Strings [| "" |]
            ColumnData.Strings [| "x" |]
        |]
    use file =
        Fits.openBytes (
            Fits.toBytes [
                Fits.emptyPrimary Header.empty
                Fits.binaryTable Header.empty [ column; names ]
            ]
        )
    let hdu = file[1]
    Assert.Equal(16L, hdu.PCount + 0L - 6L)
    Assert.Equal<int64[]>([| 16L; 3L |], hdu.Axes)
    let table = Fits.readTable file 1
    Assert.Equal(48, table.HeapOffset)
    Assert.Equal(column.Data, (Table.column 0 table).Data)
    Assert.Equal(names.Data, (Table.column 1 table).Data)
    Assert.Equal(Some "1PE", Header.tryString "TFORM1" hdu.Header)

[<Fact>]
let ``logicals bits and complex values round trip`` () =
    let columns = [
        Table.ofLogicals "L" [| Some true; None; Some false |]
        Table.create "X" (TForm.Fixed(11, ColumnType.Bit)) (ColumnData.Bits(Array.init 33 (fun i -> i % 3 = 0)))
        Table.create
            "C"
            (TForm.Fixed(2, ColumnType.Complex32))
            (ColumnData.Complex32s [| for i in 0..5 -> Complex(float i, float -i) |])
        Table.create
            "M"
            (TForm.Fixed(1, ColumnType.Complex64))
            (ColumnData.Complex64s [| Complex(1e300, -1e-300); Complex.Zero; Complex.One |])
        Table.ofInt64s "K" [| Int64.MinValue; 0L; Int64.MaxValue |]
        Table.ofUInt8s "B" [| 0uy; 128uy; 255uy |]
        Table.ofFloat64s "D" [| nan; infinity; -0.0 |]
    ]

    use file =
        Fits.openBytes (Fits.toBytes [ Fits.emptyPrimary Header.empty; Fits.binaryTable Header.empty columns ])
    let table = Fits.readTable file 1
    Assert.Equal(1 + 2 + 16 + 16 + 8 + 1 + 8, table.RowLength)

    for i in 0..5 do
        Assert.Equal(columns[i].Data, (Table.column i table).Data)

    match (Table.column 6 table).Data with
    | ColumnData.Float64s [| a; b; c |] ->
        Assert.True(Double.IsNaN a)
        Assert.Equal(infinity, b)
        Assert.True(Double.IsNegative c)
    | other -> failwith $"unexpected %A{other}"

[<Fact>]
let ``user copies of column keywords are replaced`` () =
    let header =
        Header.ofCards [
            Card.Value("TFORM1", CardValue.String "99Z", None)
            Card.Value("TFIELDS", CardValue.Integer 7L, None)
            Card.Value("THEAP", CardValue.Integer 7L, None)
            Card.Value("TOTAL", CardValue.Integer 7L, None)
        ]
    use file =
        Fits.openBytes (Fits.toBytes [ Fits.emptyPrimary Header.empty; Fits.binaryTable header columns ])
    let keywords = Header.keywords file[1].Header
    Assert.Equal(1, keywords |> List.filter (fun k -> k = "TFORM1") |> List.length)
    Assert.Equal(Some 3L, Header.tryInt "TFIELDS" file[1].Header)
    Assert.False(Header.contains "THEAP" file[1].Header)
    Assert.Equal(Some 7L, Header.tryInt "TOTAL" file[1].Header)

[<Fact>]
let ``rejects columns that disagree on rows or types`` () =
    Assert.Throws<ArgumentException>(fun () ->
        Table.encode [ Table.ofInt32s "A" [| 1 |]; Table.ofInt32s "B" [| 1; 2 |] ]
        |> ignore
    )
    |> ignore
    Assert.Throws<ArgumentException>(fun () ->
        Table.encode [
            Table.create "A" (TForm.Fixed(1, ColumnType.Int16)) (ColumnData.Int32s [| 1 |])
        ]
        |> ignore
    )
    |> ignore
    Assert.Throws<ArgumentException>(fun () -> Table.encode [ Table.ofStrings "A" 2 [| "abc" |] ] |> ignore)
    |> ignore
    Assert.Throws<ArgumentException>(fun () ->
        Table.encode [
            Table.create "A" (TForm.Fixed(2, ColumnType.Int16)) (ColumnData.Int16s [| 1s; 2s; 3s |])
        ]
        |> ignore
    )
    |> ignore

[<Fact>]
let ``a bad TFORM is fatal`` () =
    let header = Header.ofCards [ Card.Value("EXTNAME", CardValue.String "T", None) ]
    let bytes =
        Fits.toBytes [ Fits.emptyPrimary Header.empty; Fits.binaryTable header columns ]
    use file = Fits.openBytesWith FitsOptions.Lenient bytes
    let info = file[1]
    let patched = {
        info with
            Header = Header.set "TFORM2" (CardValue.String "4Z") info.Header
    }
    let issues = Table.tryOfHdu Lenient patched (Fits.readData file 1) |> expectError
    Assert.Equal<Violation list>([ Violation.InvalidTForm(2, "4Z") ], issues |> List.map (fun i -> i.Violation))

[<Fact>]
let ``row padding is a strict error and a lenient warning`` () =
    let bytes =
        Fits.toBytes [ Fits.emptyPrimary Header.empty; Fits.binaryTable Header.empty columns ]
    use file = Fits.openBytesWith FitsOptions.Lenient bytes
    let info = file[1]
    let patched = { info with Axes = [| 13L; 2L |] }
    let issues = Table.tryOfHdu Strict patched (Fits.readData file 1) |> expectError
    Assert.Equal<Violation list>([ Violation.RowLengthMismatch(13L, 12L) ], issues |> List.map (fun i -> i.Violation))
    let _, warnings = Table.tryOfHdu Lenient patched (Fits.readData file 1) |> expectOk
    Assert.Equal(1, warnings.Length)
    let tooNarrow = { info with Axes = [| 11L; 2L |] }
    Table.tryOfHdu Lenient tooNarrow (Fits.readData file 1) |> expectError |> ignore

[<Fact>]
let ``reading a non-table HDU is an error`` () =
    use file = Fits.openBytes (Fits.toBytes [ Fits.emptyPrimary Header.empty ])
    Fits.tryReadTable file 0 |> expectError |> ignore

let columnType: Gen<ColumnType> =
    Gen.elements [
        ColumnType.Logical
        ColumnType.Bit
        ColumnType.UInt8
        ColumnType.Int16
        ColumnType.Int32
        ColumnType.Int64
        ColumnType.Char
        ColumnType.Float32
        ColumnType.Float64
        ColumnType.Complex32
        ColumnType.Complex64
    ]

let float32Value: Gen<float32> =
    Gen.choose (Int32.MinValue, Int32.MaxValue)
    |> Gen.map BitConverter.Int32BitsToSingle
    |> Gen.filter (fun f -> not (Single.IsNaN f))

let float64Value: Gen<float> =
    int64Value
    |> Gen.map BitConverter.Int64BitsToDouble
    |> Gen.filter (fun f -> not (Double.IsNaN f))

/// Data for `count` elements of a type. Strings are one entry of at most `count` characters.
let elements (t: ColumnType) (count: int) : Gen<ColumnData> =
    match t with
    | ColumnType.Logical ->
        Gen.arrayOfLength count (Gen.elements [ Some true; Some false; None ])
        |> Gen.map ColumnData.Logicals
    | ColumnType.Bit ->
        Gen.arrayOfLength count (Gen.elements [ true; false ])
        |> Gen.map ColumnData.Bits
    | ColumnType.UInt8 ->
        Gen.arrayOfLength count (Gen.choose (0, 255) |> Gen.map byte)
        |> Gen.map ColumnData.UInt8s
    | ColumnType.Int16 ->
        Gen.arrayOfLength count (Gen.choose (int Int16.MinValue, int Int16.MaxValue) |> Gen.map int16)
        |> Gen.map ColumnData.Int16s
    | ColumnType.Int32 ->
        Gen.arrayOfLength count (Gen.choose (Int32.MinValue, Int32.MaxValue))
        |> Gen.map ColumnData.Int32s
    | ColumnType.Int64 -> Gen.arrayOfLength count int64Value |> Gen.map ColumnData.Int64s
    | ColumnType.Char -> asciiString count |> Gen.map (fun s -> ColumnData.Strings [| s.TrimEnd ' ' |])
    | ColumnType.Float32 -> Gen.arrayOfLength count float32Value |> Gen.map ColumnData.Float32s
    | ColumnType.Float64 -> Gen.arrayOfLength count float64Value |> Gen.map ColumnData.Float64s
    | ColumnType.Complex32 ->
        Gen.arrayOfLength count (Gen.map2 (fun re im -> Complex(float re, float im)) float32Value float32Value)
        |> Gen.map ColumnData.Complex32s
    | ColumnType.Complex64 ->
        Gen.arrayOfLength count (Gen.map2 (fun re im -> Complex(re, im)) float64Value float64Value)
        |> Gen.map ColumnData.Complex64s

let private concat (t: ColumnType) (parts: ColumnData[]) : ColumnData =
    match t with
    | ColumnType.Logical ->
        ColumnData.Logicals(
            parts
            |> Array.collect (
                function
                | ColumnData.Logicals a -> a
                | _ -> [||]
            )
        )
    | ColumnType.Bit ->
        ColumnData.Bits(
            parts
            |> Array.collect (
                function
                | ColumnData.Bits a -> a
                | _ -> [||]
            )
        )
    | ColumnType.UInt8 ->
        ColumnData.UInt8s(
            parts
            |> Array.collect (
                function
                | ColumnData.UInt8s a -> a
                | _ -> [||]
            )
        )
    | ColumnType.Int16 ->
        ColumnData.Int16s(
            parts
            |> Array.collect (
                function
                | ColumnData.Int16s a -> a
                | _ -> [||]
            )
        )
    | ColumnType.Int32 ->
        ColumnData.Int32s(
            parts
            |> Array.collect (
                function
                | ColumnData.Int32s a -> a
                | _ -> [||]
            )
        )
    | ColumnType.Int64 ->
        ColumnData.Int64s(
            parts
            |> Array.collect (
                function
                | ColumnData.Int64s a -> a
                | _ -> [||]
            )
        )
    | ColumnType.Char ->
        ColumnData.Strings(
            parts
            |> Array.collect (
                function
                | ColumnData.Strings a -> a
                | _ -> [||]
            )
        )
    | ColumnType.Float32 ->
        ColumnData.Float32s(
            parts
            |> Array.collect (
                function
                | ColumnData.Float32s a -> a
                | _ -> [||]
            )
        )
    | ColumnType.Float64 ->
        ColumnData.Float64s(
            parts
            |> Array.collect (
                function
                | ColumnData.Float64s a -> a
                | _ -> [||]
            )
        )
    | ColumnType.Complex32 ->
        ColumnData.Complex32s(
            parts
            |> Array.collect (
                function
                | ColumnData.Complex32s a -> a
                | _ -> [||]
            )
        )
    | ColumnType.Complex64 ->
        ColumnData.Complex64s(
            parts
            |> Array.collect (
                function
                | ColumnData.Complex64s a -> a
                | _ -> [||]
            )
        )

let column (rows: int) : Gen<Column> = gen {
    let! name = keyword
    let! t = columnType
    let! variable = Gen.frequency [ 3, Gen.constant false; 1, Gen.constant true ]

    if variable then
        let! wide = Gen.elements [ false; true ]

        let! entries =
            Gen.listOfLength
                rows
                (gen {
                    let! count = Gen.choose (0, 6)
                    return! elements t count
                })

        return Table.create name (TForm.Variable(t, None, wide)) (ColumnData.Variable(Array.ofList entries))
    else
        let! repeat = Gen.choose (0, 5)
        let! parts = Gen.listOfLength rows (elements t repeat)
        return Table.create name (TForm.Fixed(repeat, t)) (concat t (Array.ofList parts))
}

let table: Gen<int * Column list> = gen {
    let! rows = Gen.choose (0, 5)
    let! count = Gen.choose (1, 5)
    let! columns = Gen.listOfLength count (column rows)
    return rows, columns
}

[<Fact>]
let ``tables of every column type survive a file round trip`` () =
    check (
        forAll
            table
            (fun (rows, columns) ->
                let bytes =
                    Fits.toBytes [ Fits.emptyPrimary Header.empty; Fits.binaryTable Header.empty columns ]

                match Fits.tryOpenBytes bytes with
                | Error e -> failwith $"open failed: %A{e}"
                | Ok file ->
                    use file = file
                    let read = Fits.readTable file 1

                    columns
                    |> List.iteri (fun i expected ->
                        let actual = Table.column i read

                        if actual.Data <> expected.Data then
                            failwith
                                $"column {i} %A{expected.Info.Form}: expected %A{expected.Data} but got %A{actual.Data}"

                        if actual.Info.Form <> expected.Info.Form || actual.Info.Name <> expected.Info.Name then
                            failwith $"column {i}: descriptor changed to %A{actual.Info}"
                    )

                    let countable =
                        columns
                        |> List.exists (fun c ->
                            match c.Data, c.Info.Form with
                            | ColumnData.Strings _, _
                            | ColumnData.Variable _, _ -> true
                            | _, TForm.Fixed(repeat, _) -> repeat > 0
                            | _ -> true
                        )

                    read.Rows = (if countable then rows else 0)
            )
    )

// The standard has no unsigned integer or signed byte TFORM; Table 18 offsets the value and
// records the offset in TZEROn.

[<Fact>]
let ``unsigned and signed byte columns round trip through their TZERO conventions`` () =
    let bytes =
        Fits.toBytes [
            Fits.emptyPrimary Header.empty
            Fits.binaryTable Header.empty [
                Table.ofInt8s "I8" [| -128y; 0y; 127y |]
                Table.ofUInt16s "U16" [| 0us; 32768us; 65535us |]
                Table.ofUInt32s "U32" [| 0u; 2147483648u; 4294967295u |]
                Table.ofUInt64s "U64" [| 0UL; 9223372036854775808UL; 18446744073709551615UL |]
            ]
        ]

    use file = Fits.openBytes bytes
    let table = Fits.readTable file 1
    let column name = (Table.tryColumn name table).Value

    Assert.True(Table.isInt8 (column "I8"))
    Assert.True(Table.isUInt16 (column "U16"))
    Assert.True(Table.isUInt32 (column "U32"))
    Assert.True(Table.isUInt64 (column "U64"))

    Assert.Equal<sbyte[]>([| -128y; 0y; 127y |], Table.toInt8 (column "I8"))
    Assert.Equal<uint16[]>([| 0us; 32768us; 65535us |], Table.toUInt16 (column "U16"))
    Assert.Equal<uint32[]>([| 0u; 2147483648u; 4294967295u |], Table.toUInt32 (column "U32"))

    Assert.Equal<uint64[]>([| 0UL; 9223372036854775808UL; 18446744073709551615UL |], Table.toUInt64 (column "U64"))

    // The offset is written as TZEROn, which is what makes the file readable by anything else.
    Assert.Equal(Some -128.0, Header.tryFloat "TZERO1" file[1].Header)
    Assert.Equal(Some 32768.0, Header.tryFloat "TZERO2" file[1].Header)

[<Fact>]
let ``a column without the convention is not read as unsigned`` () =
    let plain = Table.ofInt16s "X" [| 1s |]
    Assert.False(Table.isUInt16 plain)
    Assert.Throws<InvalidOperationException>(fun () -> Table.toUInt16 plain |> ignore)
    |> ignore
    // Scaling other than the pure offset is a real scaling, not the convention.
    let scaled = Table.ofUInt16s "Y" [| 1us |] |> Table.withScaling 32768.0 2.0
    Assert.False(Table.isUInt16 scaled)

// TDIMn: the shape of one cell, declared fastest axis first the way NAXISn is.

[<Fact>]
let ``TDIM gives a cell its shape in C order`` () =
    let plain =
        Table.create "X" (TForm.Fixed(6, ColumnType.Float32)) (ColumnData.Float32s(Array.zeroCreate 6))
    Assert.Equal<int[]>([| 6 |], Table.cellShape plain.Info)

    let shaped = plain |> Table.withDim [| 3L; 2L |]
    Assert.Equal<int[]>([| 2; 3 |], Table.cellShape shaped.Info)

[<Fact>]
let ``a cell of a shaped column reads as an image`` () =
    let column =
        Table.create
            "GRID"
            (TForm.Fixed(6, ColumnType.Int16))
            (ColumnData.Int16s [| 1s; 2s; 3s; 4s; 5s; 6s; 7s; 8s; 9s; 10s; 11s; 12s |])
        |> Table.withDim [| 3L; 2L |]
        |> Table.withScaling 10.0 2.0

    let first = Table.cellImage 0 column
    Assert.Equal<int[]>([| 2; 3 |], first.Shape)
    Assert.Equal<float[]>([| 12.0; 14.0; 16.0; 18.0; 20.0; 22.0 |], Image.toFloat64 first)

    let second = Table.cellImage 1 column

    match second.Data with
    | ImageData.Int16 values -> Assert.Equal<int16[]>([| 7s; 8s; 9s; 10s; 11s; 12s |], values)
    | other -> failwith $"expected int16 but got %A{other}"

    // A cell of strings is not a grid of numbers.
    let names = Table.ofStrings "N" 4 [| "ab" |]
    Assert.Throws<InvalidOperationException>(fun () -> Table.cellImage 0 names |> ignore)
    |> ignore

[<Fact>]
let ``a TDIM that does not multiply out to the repeat is refused on write`` () =
    let column =
        Table.create "G" (TForm.Fixed(6, ColumnType.Int16)) (ColumnData.Int16s(Array.zeroCreate 6))
        |> Table.withDim [| 4L; 2L |]

    Assert.Throws<ArgumentException>(fun () -> Fits.binaryTable Header.empty [ column ] |> ignore)
    |> ignore

[<Fact>]
let ``a TDIM that does not multiply out to the repeat is a violation on read`` () =
    let good =
        Fits.toBytes [
            Fits.emptyPrimary Header.empty
            Fits.binaryTable Header.empty [
                Table.create "G" (TForm.Fixed(6, ColumnType.Int16)) (ColumnData.Int16s(Array.zeroCreate 6))
                |> Table.withDim [| 3L; 2L |]
            ]
        ]

    // Rewrite the card behind the writer's back, the way a file from elsewhere might have it.
    let broken =
        use file = Fits.openBytes good
        let copy = Fits.copy file 1

        Fits.toBytes [
            Fits.copy file 0
            {
                copy with
                    Header = Header.set "TDIM1" (CardValue.String "(4,2)") copy.Header
            }
        ]

    use strict = Fits.openBytes broken

    match Fits.tryReadTable strict 1 |> expectError with
    | Violations issues -> Assert.Contains(issues, fun i -> (Issue.describe i).Contains "TDIM1")
    | other -> failwith $"expected violations but got %A{other}"

    // Lenient reads it anyway and takes the dimensions at their word.
    use lenient = Fits.openBytesWith FitsOptions.Lenient broken
    let table = Fits.readTable lenient 1
    Assert.Equal<int[]>([| 2; 4 |], Table.cellShape table.Columns[0])

type Grid = { Id: int; Values: float32[,] }

[<Fact>]
let ``a record field of rank two is filled from TDIM`` () =
    let bytes =
        Fits.toBytes [
            Fits.emptyPrimary Header.empty
            Fits.binaryTable Header.empty [
                Table.ofInt32s "ID" [| 1; 2 |]
                Table.create "VALUES" (TForm.Fixed(6, ColumnType.Float32)) (ColumnData.Float32s(Array.init 12 float32))
                |> Table.withDim [| 3L; 2L |]
            ]
        ]

    use file = Fits.openBytes bytes
    let grids = Fits.readRecords<Grid> file 1
    Assert.Equal(2, grids.Length)
    Assert.Equal(1, grids[0].Id)
    // TDIM (3,2) is three fastest, so the cell is two rows of three in C order.
    Assert.Equal(2, grids[0].Values.GetLength 0)
    Assert.Equal(3, grids[0].Values.GetLength 1)
    Assert.Equal(0.0f, grids[0].Values[0, 0])
    Assert.Equal(4.0f, grids[0].Values[1, 1])
    Assert.Equal(6.0f, grids[1].Values[0, 0])
    Assert.Equal(11.0f, grids[1].Values[1, 2])

type WrongRank = { Values: float32[,,] }

[<Fact>]
let ``a field whose rank does not match TDIM is reported`` () =
    let bytes =
        Fits.toBytes [
            Fits.emptyPrimary Header.empty
            Fits.binaryTable Header.empty [
                Table.create "VALUES" (TForm.Fixed(6, ColumnType.Float32)) (ColumnData.Float32s(Array.zeroCreate 6))
                |> Table.withDim [| 3L; 2L |]
            ]
        ]

    use file = Fits.openBytes bytes
    let issues = Record.tryOfTable<WrongRank>(Fits.readTable file 1) |> expectError
    Assert.Contains(issues, fun i -> (Issue.describe i).Contains "VALUES")
