module RecordTests

open System
open System.Numerics
open Xunit
open FSharp.Astro.Fits
open Generators

let header =
    Header.ofCards [
        Card.Value("OBJECT", CardValue.String "M31", Some "target")
        Card.Value("EXPTIME", CardValue.Real 30.0, None)
        Card.Value("NAXIS", CardValue.Integer 2L, None)
        Card.Value("SIMPLE", CardValue.Logical true, None)
        Card.Value("BIG", CardValue.Integer 5000000000L, None)
        Card.Value("CPLX", CardValue.ComplexReal(1.5, -2.0), None)
    ]

type Observation = {
    [<Keyword "OBJECT">]
    Target: string
    ExpTime: float
    ExpTime32: float32
    Naxis: int
    Simple: bool
    Big: int64
    Cplx: Complex
    Filter: string option
    Gain: float option
    Raw: CardValue
}

[<Fact>]
let ``fills a record from a header by field name and attribute`` () =
    let obs =
        Record.ofHeader<Observation>(
            Header.set "RAW" (CardValue.Undefined) header
            |> Header.set "EXPTIME32" (CardValue.Real 2.5)
        )
    Assert.Equal("M31", obs.Target)
    Assert.Equal(30.0, obs.ExpTime)
    Assert.Equal(2.5f, obs.ExpTime32)
    Assert.Equal(2, obs.Naxis)
    Assert.True obs.Simple
    Assert.Equal(5000000000L, obs.Big)
    Assert.Equal(Complex(1.5, -2.0), obs.Cplx)
    Assert.Equal(None, obs.Filter)
    Assert.Equal(None, obs.Gain)
    Assert.Equal(CardValue.Undefined, obs.Raw)

type Wrong = { Missing: string; Object: float; Naxis: int }

[<Fact>]
let ``reports every field that cannot be filled`` () =
    let issues =
        Record.tryOfHeader<Wrong> header
        |> expectError
        |> List.map (fun i -> i.Violation)
    Assert.Equal<Violation list>(
        [
            Violation.MissingKeyword "MISSING"
            Violation.TypeMismatch("OBJECT", "a real", "a string")
        ],
        issues
    )
    Assert.Throws<FitsException>(fun () -> Record.ofHeader<Wrong> header |> ignore)
    |> ignore

type Unsupported = { Object: DateTime }

[<Fact>]
let ``rejects field types a keyword cannot hold`` () =
    Assert.Throws<InvalidOperationException>(fun () -> Record.tryOfHeader<Unsupported> header |> ignore)
    |> ignore

let columns = [
    Table.ofInt32s "ID" [| 1; 2; 3 |]
    Table.ofStrings "NAME" 8 [| "alpha"; "beta"; "" |]
    Table.ofFloat32s "FLUX" [| 1.5f; 2.5f; nanf |]
    Table.ofInt16s "MAG" [| 100s; -1s; 300s |]
    |> Table.withNull -1L
    |> Table.withScaling 0.0 0.1
    Table.ofVariable "SPEC" ColumnType.Float32 [|
        ColumnData.Float32s [| 1.f; 2.f |]
        ColumnData.Float32s [||]
        ColumnData.Float32s [| 3.f |]
    |]
    Table.create "FLAGS" (TForm.Fixed(2, ColumnType.Int32)) (ColumnData.Int32s [| 1; 2; 3; 4; 5; 6 |])
    Table.ofLogicals "OK" [| Some true; None; Some false |]
    Table.ofVariable "NOTE" ColumnType.Char [|
        ColumnData.Strings [| "x" |]
        ColumnData.Strings [| "" |]
        ColumnData.Strings [| "yz" |]
    |]
    Table.create
        "BITS"
        (TForm.Fixed(3, ColumnType.Bit))
        (ColumnData.Bits [| true; false; true; false; false; false; true; true; true |])
]

let table () =
    let file =
        Fits.openBytes (Fits.toBytes [ Fits.emptyPrimary Header.empty; Fits.binaryTable Header.empty columns ])
    file, Fits.readTable file 1

type Star = {
    Id: int
    Name: string
    Flux: float32
    Mag: float option
    Spec: float32[]
    Flags: int[]
    Ok: bool option
    Note: string
    Bits: bool[]
    [<Column "FLUX">]
    FluxAsDouble: float
    [<Column "ID">]
    IdAsInt64: int64
    [<Column "FLUX">]
    FluxOrNone: float32 option
    [<Column "MAG">]
    RawMag: int16
}

[<Fact>]
let ``fills one record per row`` () =
    let file, table = table ()
    use file = file
    let stars = Record.ofTable<Star> table
    Assert.Equal(3, stars.Length)
    Assert.Equal(
        {
            Id = 1
            Name = "alpha"
            Flux = 1.5f
            Mag = Some 10.0
            Spec = [| 1.f; 2.f |]
            Flags = [| 1; 2 |]
            Ok = Some true
            Note = "x"
            Bits = [| true; false; true |]
            FluxAsDouble = 1.5
            IdAsInt64 = 1L
            FluxOrNone = Some 1.5f
            RawMag = 100s
        },
        stars[0]
    )
    Assert.Equal(None, stars[1].Mag)
    Assert.Equal(None, stars[1].Ok)
    Assert.Equal<float32[]>([||], stars[1].Spec)
    Assert.Equal(None, stars[2].FluxOrNone)
    Assert.Equal(Some 30.0, stars[2].Mag)
    Assert.Equal("yz", stars[2].Note)
    Assert.Equal(Some false, stars[2].Ok)
    Assert.Equal<Star[]>(stars, Fits.readRecords<Star> file 1)

type Wide = { Id: float; Mag: float32; Flags: int64[] }

[<Fact>]
let ``widens integer columns into floats and rejects the rest`` () =
    let _, table = table ()
    let issues =
        Record.tryOfTable<Wide> table |> expectError |> List.map (fun i -> i.Violation)
    Assert.Equal<Violation list>([ Violation.TypeMismatch("FLAGS", "Int64[]", "2J") ], issues)

type Partial = { Id: float; Mag: float32 }

[<Fact>]
let ``integer columns become scaled floats`` () =
    let _, table = table ()
    let rows = Record.ofTable<Partial> table
    Assert.Equal({ Id = 1.0; Mag = 10.0f }, rows[0])
    Assert.True(Single.IsNaN rows[1].Mag)

type Broken = { Nope: int; Name: int; Ok: bool }

[<Fact>]
let ``reports missing columns and mismatches together`` () =
    let _, table = table ()
    let issues =
        Record.tryOfTable<Broken> table
        |> expectError
        |> List.map (fun i -> i.Violation)
    Assert.Equal<Violation list>(
        [
            Violation.MissingColumn "Nope"
            Violation.TypeMismatch("NAME", "Int32", "8A")
        ],
        issues
    )

type Strict = { Ok: bool }

[<Fact>]
let ``an undefined logical cannot fill a plain bool`` () =
    let _, table = table ()
    Assert.Throws<FitsException>(fun () -> Record.ofTable<Strict> table |> ignore)
    |> ignore

type Counts = {
    Id: int
    Level: sbyte
    Pixels: uint16
    Total: uint32
    Ticks: uint64
    Window: uint16[]
}

[<Fact>]
let ``a record field may be unsigned when the column uses the TZERO convention`` () =
    let bytes =
        Fits.toBytes [
            Fits.emptyPrimary Header.empty
            Fits.binaryTable Header.empty [
                Table.ofInt32s "ID" [| 1; 2 |]
                Table.ofInt8s "LEVEL" [| -128y; 127y |]
                Table.ofUInt16s "PIXELS" [| 0us; 65535us |]
                Table.ofUInt32s "TOTAL" [| 4294967295u; 7u |]
                Table.ofUInt64s "TICKS" [| 18446744073709551615UL; 9UL |]
                Table.create
                    "WINDOW"
                    (TForm.Fixed(2, ColumnType.Int16))
                    (ColumnData.Int16s([| 0us; 65535us; 1us; 2us |] |> Array.map (fun v -> int16 (int v - 32768))))
                |> Table.withScaling 32768.0 1.0
            ]
        ]

    use file = Fits.openBytes bytes
    let rows = Fits.readRecords<Counts> file 1

    Assert.Equal(
        {
            Id = 1
            Level = -128y
            Pixels = 0us
            Total = 4294967295u
            Ticks = 18446744073709551615UL
            Window = [| 0us; 65535us |]
        },
        rows[0]
    )

    Assert.Equal(127y, rows[1].Level)
    Assert.Equal(65535us, rows[1].Pixels)
    Assert.Equal<uint16[]>([| 1us; 2us |], rows[1].Window)

type Plain = { Pixels: uint16 }

[<Fact>]
let ``an unsigned field over a plain column is still a mismatch`` () =
    let bytes =
        Fits.toBytes [
            Fits.emptyPrimary Header.empty
            Fits.binaryTable Header.empty [ Table.ofInt16s "PIXELS" [| 1s |] ]
        ]

    use file = Fits.openBytes bytes
    let issues = Record.tryOfTable<Plain>(Fits.readTable file 1) |> expectError
    Assert.Contains(issues, fun i -> (Issue.describe i).Contains "PIXELS")
