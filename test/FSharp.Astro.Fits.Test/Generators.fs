/// FsCheck generators for FITS values, and helpers shared by the test files.
module Generators

open System
open FsCheck
open FsCheck.FSharp
open FSharp.Astro.Fits

let check (property: Property) =
    Check.One(Config.QuickThrowOnFailure.WithMaxTest 300, property)

let forAll (gen: Gen<'T>) (test: 'T -> bool) : Property = Prop.forAll (Arb.fromGen gen) test

let expectOk (result: Result<'T, 'E>) : 'T =
    match result with
    | Ok v -> v
    | Error e -> failwith $"expected Ok but got Error %A{e}"

let expectError (result: Result<'T, 'E>) : 'E =
    match result with
    | Ok v -> failwith $"expected Error but got Ok %A{v}"
    | Error e -> e

/// A stream that refuses to seek, to exercise the buffering path of the stream loaders.
type NonSeekableStream(bytes: byte[]) =
    inherit System.IO.MemoryStream(bytes)
    override _.CanSeek = false

/// Violations that a generated card may legitimately trigger because it is too long for a card.
let unrepresentable (violation: Violation) : bool =
    match violation with
    | Violation.CommentTooLong _
    | Violation.ValueTooLong _
    | Violation.AmbiguousCommentary _ -> true
    | _ -> false

/// Describes the first difference between two card lists, for property failure messages.
let firstMismatch (expected: Card list) (actual: Card list) : string =
    let rec go i (e: Card list) (a: Card list) =
        match e, a with
        | [], [] -> "lists are equal"
        | e :: es, a :: ``as`` when e = a -> go (i + 1) es ``as``
        | e :: _, a :: _ -> $"card {i}: expected %A{e} but got %A{a}"
        | e :: _, [] -> $"card {i}: expected %A{e} but the list ended"
        | [], a :: _ -> $"card {i}: unexpected extra %A{a}"

    go 0 expected actual

/// Pads a card image to 80 characters.
let line (text: string) : string = text.PadRight 80

let asciiChar: Gen<char> = Gen.choose (0x20, 0x7E) |> Gen.map char

let asciiString (maxLength: int) : Gen<string> = gen {
    let! n = Gen.choose (0, maxLength)
    let! chars = Gen.arrayOfLength n asciiChar
    return String chars
}

let private reserved = set [ "END"; "COMMENT"; "HISTORY"; "CONTINUE"; "HIERARCH" ]

let keyword: Gen<string> =
    gen {
        let! n = Gen.choose (1, 8)
        let! chars =
            Gen.arrayOfLength n (Gen.elements ([ 'A' .. 'Z' ] @ [ '0' .. '9' ] @ [ '-'; '_' ]))
        return String chars
    }
    |> Gen.filter (fun k -> not (reserved.Contains k))

let hierarchKeyword: Gen<string> = gen {
    let! n = Gen.choose (2, 4)

    let! words =
        Gen.listOfLength
            n
            (gen {
                let! m = Gen.choose (1, 6)
                let! chars =
                    Gen.arrayOfLength m (Gen.elements ([ 'A' .. 'Z' ] @ [ 'a' .. 'z' ] @ [ '0' .. '9' ]))
                return String chars
            })

    return String.Join(" ", words)
}

let int64Value: Gen<int64> = gen {
    let! hi = Gen.choose (Int32.MinValue, Int32.MaxValue)
    let! lo = Gen.choose (Int32.MinValue, Int32.MaxValue)
    return (int64 hi <<< 32) ||| int64 (uint32 lo)
}

let finiteFloat: Gen<float> =
    Gen.frequency [
        3,
        int64Value
        |> Gen.map BitConverter.Int64BitsToDouble
        |> Gen.filter Double.IsFinite
        2, Gen.choose (-100000, 100000) |> Gen.map (fun i -> float i / 8.0)
        1, Gen.elements [ 0.0; 1.0; -1.0; 1e-300; 1e300; 0.1; 32768.0 ]
    ]

let comment: Gen<string option> =
    Gen.frequency [
        1, Gen.constant None
        2,
        asciiString 40
        |> Gen.map (fun s -> s.Trim ' ')
        |> Gen.filter (fun s -> s <> "")
        |> Gen.map Some
    ]

let cardValue: Gen<CardValue> =
    Gen.frequency [
        2, Gen.elements [ true; false ] |> Gen.map CardValue.Logical
        3, int64Value |> Gen.map CardValue.Integer
        3, finiteFloat |> Gen.map CardValue.Real
        1, Gen.map2 (fun a b -> CardValue.ComplexInt(a, b)) int64Value int64Value
        1, Gen.map2 (fun a b -> CardValue.ComplexReal(a, b)) finiteFloat finiteFloat
        4, asciiString 200 |> Gen.map (fun s -> CardValue.String(s.TrimEnd ' '))
        1, Gen.constant CardValue.Undefined
    ]

/// Value cards that can be formatted and read back unchanged.
let valueCard: Gen<Card> = gen {
    let! keyword = Gen.frequency [ 5, keyword; 1, hierarchKeyword ]
    let! value = cardValue
    let! comment = comment
    return Card.Value(keyword, value, comment)
}

/// Commentary cards that can be formatted and read back unchanged.
let commentaryCard: Gen<Card> =
    gen {
        let! keyword =
            Gen.frequency [ 3, Gen.elements [ ""; "COMMENT"; "HISTORY" ]; 1, keyword ]
        let! text = asciiString 72 |> Gen.map (fun s -> s.TrimEnd ' ')
        return Card.Commentary(keyword, text)
    }
    |> Gen.filter (fun c ->
        match c with
        | Card.Commentary(k, text) -> k = "" || k = "COMMENT" || k = "HISTORY" || not (text.StartsWith "= ")
        | _ -> true
    )

let card: Gen<Card> = Gen.frequency [ 4, valueCard; 1, commentaryCard ]

let cards: Gen<Card list> = gen {
    let! n = Gen.choose (0, 30)
    return! Gen.listOfLength n card
}

let bitPix: Gen<BitPix> =
    Gen.elements [
        BitPix.UInt8
        BitPix.Int16
        BitPix.Int32
        BitPix.Int64
        BitPix.Float32
        BitPix.Float64
    ]

let shape: Gen<int[]> = gen {
    let! rank = Gen.choose (0, 3)
    let! dims = Gen.arrayOfLength rank (Gen.choose (1, 8))
    return dims
}

let imageData (bitPix: BitPix) (count: int) : Gen<ImageData> =
    match bitPix with
    | BitPix.UInt8 ->
        Gen.arrayOfLength count (Gen.choose (0, 255) |> Gen.map byte)
        |> Gen.map ImageData.UInt8
    | BitPix.Int16 ->
        Gen.arrayOfLength count (Gen.choose (int Int16.MinValue, int Int16.MaxValue) |> Gen.map int16)
        |> Gen.map ImageData.Int16
    | BitPix.Int32 ->
        Gen.arrayOfLength count (Gen.choose (Int32.MinValue, Int32.MaxValue))
        |> Gen.map ImageData.Int32
    | BitPix.Int64 -> Gen.arrayOfLength count int64Value |> Gen.map ImageData.Int64
    | BitPix.Float32 ->
        Gen.arrayOfLength
            count
            (Gen.choose (Int32.MinValue, Int32.MaxValue)
             |> Gen.map BitConverter.Int32BitsToSingle
             |> Gen.filter (fun f -> not (Single.IsNaN f)))
        |> Gen.map ImageData.Float32
    | BitPix.Float64 ->
        Gen.arrayOfLength
            count
            (int64Value
             |> Gen.map BitConverter.Int64BitsToDouble
             |> Gen.filter (fun f -> not (Double.IsNaN f)))
        |> Gen.map ImageData.Float64

let image: Gen<Image> = gen {
    let! shape = shape
    let! bitPix = bitPix
    let count = if shape.Length = 0 then 0 else shape |> Array.fold (*) 1
    let! data = imageData bitPix count
    return Image.create shape data
}
