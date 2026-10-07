module HduTests

open Xunit
open FsCheck
open FsCheck.FSharp
open FSharp.Astro.Fits
open Generators

let header (cards: (string * CardValue) list) =
    Header.ofCards (cards |> List.map (fun (k, v) -> Card.Value(k, v, None)))

let primary2d =
    header [
        "SIMPLE", CardValue.Logical true
        "BITPIX", CardValue.Integer -32L
        "NAXIS", CardValue.Integer 2L
        "NAXIS1", CardValue.Integer 10L
        "NAXIS2", CardValue.Integer 4L
    ]

[<Fact>]
let ``computes the layout of a primary image`` () =
    let layout, warnings = HduLayout.tryOfHeader Strict 0 primary2d |> expectOk
    Assert.Equal(HduKind.Primary, layout.Kind)
    Assert.Equal(BitPix.Float32, layout.BitPix)
    Assert.Equal<int64[]>([| 10L; 4L |], layout.Axes)
    Assert.Equal(160L, HduLayout.dataLength layout)
    Assert.Empty warnings

[<Fact>]
let ``an empty primary has no data`` () =
    let layout, _ =
        HduLayout.tryOfHeader
            Strict
            0
            (header [
                "SIMPLE", CardValue.Logical true
                "BITPIX", CardValue.Integer 8L
                "NAXIS", CardValue.Integer 0L
            ])
        |> expectOk
    Assert.Equal(0L, HduLayout.dataLength layout)

[<Fact>]
let ``classifies extensions and counts the heap`` () =
    let table =
        header [
            "XTENSION", CardValue.String "BINTABLE"
            "BITPIX", CardValue.Integer 8L
            "NAXIS", CardValue.Integer 2L
            "NAXIS1", CardValue.Integer 12L
            "NAXIS2", CardValue.Integer 3L
            "PCOUNT", CardValue.Integer 100L
            "GCOUNT", CardValue.Integer 1L
            "TFIELDS", CardValue.Integer 2L
        ]

    let layout, warnings = HduLayout.tryOfHeader Strict 1 table |> expectOk
    Assert.Equal(HduKind.BinaryTable, layout.Kind)
    Assert.Equal(136L, HduLayout.dataLength layout)
    Assert.Empty warnings

[<Fact>]
let ``random groups exclude NAXIS1 from the size`` () =
    let groups =
        header [
            "SIMPLE", CardValue.Logical true
            "BITPIX", CardValue.Integer 16L
            "NAXIS", CardValue.Integer 3L
            "NAXIS1", CardValue.Integer 0L
            "NAXIS2", CardValue.Integer 4L
            "NAXIS3", CardValue.Integer 5L
            "GROUPS", CardValue.Logical true
            "PCOUNT", CardValue.Integer 2L
            "GCOUNT", CardValue.Integer 3L
        ]

    let layout, _ = HduLayout.tryOfHeader Strict 0 groups |> expectOk
    Assert.Equal(HduKind.RandomGroups, layout.Kind)
    Assert.Equal(2L * 3L * (2L + 20L), HduLayout.dataLength layout)

[<Fact>]
let ``strict rejects a mandatory keyword out of order`` () =
    let shuffled =
        header [
            "SIMPLE", CardValue.Logical true
            "NAXIS", CardValue.Integer 0L
            "BITPIX", CardValue.Integer 8L
        ]
    let issues = HduLayout.tryOfHeader Strict 0 shuffled |> expectError
    Assert.Contains(Violation.KeywordOutOfOrder("BITPIX", 1), issues |> List.map (fun i -> i.Violation))

[<Fact>]
let ``lenient accepts a mandatory keyword out of order with a warning`` () =
    let shuffled =
        header [
            "SIMPLE", CardValue.Logical true
            "NAXIS", CardValue.Integer 0L
            "BITPIX", CardValue.Integer 8L
        ]
    let layout, warnings = HduLayout.tryOfHeader Lenient 0 shuffled |> expectOk
    Assert.Equal(BitPix.UInt8, layout.BitPix)
    Assert.Equal(2, warnings.Length)

[<Fact>]
let ``a missing BITPIX is fatal even when lenient`` () =
    let issues =
        HduLayout.tryOfHeader Lenient 0 (header [ "SIMPLE", CardValue.Logical true; "NAXIS", CardValue.Integer 0L ])
        |> expectError
    Assert.Contains(Violation.MissingKeyword "BITPIX", issues |> List.map (fun i -> i.Violation))

[<Fact>]
let ``an invalid BITPIX is fatal`` () =
    let issues =
        HduLayout.tryOfHeader
            Lenient
            0
            (header [
                "SIMPLE", CardValue.Logical true
                "BITPIX", CardValue.Integer 12L
                "NAXIS", CardValue.Integer 0L
            ])
        |> expectError
    Assert.Single issues |> ignore

[<Fact>]
let ``mandatory cards reproduce the layout`` () =
    check (
        forAll
            (Gen.map2 (fun s b -> s, b) shape bitPix)
            (fun (shape, bitPix) ->
                let layout = {
                    Kind = HduKind.Image
                    BitPix = bitPix
                    Axes = Image.axesOfShape shape
                    PCount = 0L
                    GCount = 1L
                }
                let header = Header.ofCards (HduLayout.mandatoryCards layout)

                match HduLayout.tryOfHeader Strict 1 header with
                | Ok(parsed, []) -> parsed = layout
                | _ -> false
            )
    )
