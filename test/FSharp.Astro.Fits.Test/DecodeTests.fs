module DecodeTests

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
        Card.Value("CPLX", CardValue.ComplexInt(1L, 2L), None)
        Card.Commentary("COMMENT", "first")
        Card.Commentary("HISTORY", "step")
    ]

let violations (result: Result<'T, Issue list>) =
    expectError result |> List.map (fun i -> i.Violation)

type Observation = { Target: string; ExpTime: float; Filter: string option }

[<Fact>]
let ``primitive decoders read typed values`` () =
    Assert.Equal(Ok "M31", Decode.string "OBJECT" header)
    Assert.Equal(Ok 30.0, Decode.float "EXPTIME" header)
    Assert.Equal(Ok 2.0, Decode.float "NAXIS" header)
    Assert.Equal(Ok 2, Decode.int "NAXIS" header)
    Assert.Equal(Ok 5000000000L, Decode.int64 "BIG" header)
    Assert.Equal(Ok true, Decode.bool "SIMPLE" header)
    Assert.Equal(Ok(Complex(1.0, 2.0)), Decode.complex "CPLX" header)
    Assert.Equal(Ok(Some "target"), Decode.comment "OBJECT" header)
    Assert.Equal(Ok [ "first" ], Decode.comments header)
    Assert.Equal(Ok [ "step" ], Decode.history header)
    Assert.Equal(Ok(CardValue.Integer 2L), Decode.value "NAXIS" header)

[<Fact>]
let ``type and range mismatches are reported`` () =
    Assert.Equal<Violation list>(
        [ Violation.TypeMismatch("OBJECT", "a real", "a string") ],
        violations (Decode.float "OBJECT" header)
    )
    Assert.Equal<Violation list>(
        [ Violation.TypeMismatch("BIG", "a 32-bit integer", "an integer") ],
        violations (Decode.int "BIG" header)
    )
    Assert.Equal<Violation list>([ Violation.MissingKeyword "NOPE" ], violations (Decode.string "NOPE" header))

[<Fact>]
let ``and! reports every issue`` () =
    let decoder = decode {
        let! a = Decode.string "NOPE"
        and! b = Decode.float "OBJECT"
        and! c = Decode.bool "SIMPLE"
        return a, b, c
    }

    Assert.Equal<Violation list>(
        [
            Violation.MissingKeyword "NOPE"
            Violation.TypeMismatch("OBJECT", "a real", "a string")
        ],
        violations (Decode.run decoder header)
    )

[<Fact>]
let ``let! stops at the first failure`` () =
    let decoder = decode {
        let! a = Decode.string "NOPE"
        let! b = Decode.float "OBJECT"
        return a, b
    }

    Assert.Equal<Violation list>([ Violation.MissingKeyword "NOPE" ], violations (Decode.run decoder header))

[<Fact>]
let ``builds a record`` () =
    let decoder = decode {
        let! target = Decode.string "OBJECT"
        and! expTime = Decode.float "EXPTIME"
        and! filter = Decode.optional (Decode.string "FILTER")
        return { Target = target; ExpTime = expTime; Filter = filter }
    }

    Assert.Equal(Ok { Target = "M31"; ExpTime = 30.0; Filter = None }, Decode.run decoder header)

[<Fact>]
let ``optional accepts a missing keyword but not a wrong type`` () =
    Assert.Equal(Ok None, Decode.optional (Decode.string "FILTER") header)
    Assert.Equal(Ok(Some "M31"), Decode.optional (Decode.string "OBJECT") header)
    Assert.Equal<Violation list>(
        [ Violation.TypeMismatch("EXPTIME", "a string", "a real") ],
        violations (Decode.optional (Decode.string "EXPTIME") header)
    )
    Assert.Equal(
        Ok "V",
        Decode.optional (Decode.string "FILTER")
        |> Decode.withDefault "V"
        |> fun d -> d header
    )

[<Fact>]
let ``check validates decoded values`` () =
    let positive =
        Decode.float "EXPTIME"
        |> Decode.check "EXPTIME" "must be positive" (fun x -> x > 0.0)
    Assert.Equal(Ok 30.0, positive header)
    let negative =
        Decode.float "EXPTIME"
        |> Decode.check "EXPTIME" "must be negative" (fun x -> x < 0.0)
    Assert.Equal<Violation list>(
        [ Violation.InvalidKeywordValue("EXPTIME", "must be negative") ],
        violations (negative header)
    )

[<Fact>]
let ``all collects every issue`` () =
    let result =
        Decode.all [ Decode.string "OBJECT"; Decode.string "NOPE"; Decode.string "NOPE2" ] header
    Assert.Equal<Violation list>(
        [ Violation.MissingKeyword "NOPE"; Violation.MissingKeyword "NOPE2" ],
        violations result
    )
    Assert.Equal(Ok [ "M31" ], Decode.all [ Decode.string "OBJECT" ] header)

[<Fact>]
let ``map and andThen compose`` () =
    let halved = Decode.float "EXPTIME" |> Decode.map (fun x -> x / 2.0)
    Assert.Equal(Ok 15.0, halved header)

    let dependent =
        Decode.int "NAXIS"
        |> Decode.andThen (fun n -> Decode.all [ for i in 1..n -> Decode.int64 $"NAXIS{i}" ])

    Assert.Equal<Violation list>(
        [ Violation.MissingKeyword "NAXIS1"; Violation.MissingKeyword "NAXIS2" ],
        violations (dependent header)
    )

type Exposure = {
    Target: string
    Seconds: float
} with

    interface IFitsRecord<Exposure> with
        static member Decoder = decode {
            let! target = Decode.string "OBJECT"
            and! seconds = Decode.float "EXPTIME"
            return { Target = target; Seconds = seconds }
        }

[<Fact>]
let ``a record can declare its own decoder`` () =
    Assert.Equal(Ok { Target = "M31"; Seconds = 30.0 }, Decode.run (Decode.record<Exposure>()) header)

[<Fact>]
let ``Fits.readAs uses the declared decoder and locates issues`` () =
    let image = Image.create [| 1 |] (ImageData.UInt8 [| 1uy |])
    use file =
        Fits.openBytes (Fits.toBytes [ Fits.primary header image; Fits.imageExtension Header.empty image ])
    Assert.Equal({ Target = "M31"; Seconds = 30.0 }, Fits.readAs<Exposure> file 0)

    match Fits.tryReadAs<Exposure> file 1 with
    | Error(Violations issues) ->
        Assert.Equal(2, issues.Length)
        Assert.All(issues, fun i -> Assert.Equal(Some 1, i.Hdu))
    | other -> failwith $"unexpected %A{other}"
