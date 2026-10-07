module ImageTests

open System
open Xunit
open FSharp.Astro.Fits
open Generators

[<Fact>]
let ``decodes big-endian bytes`` () =
    Assert.Equal(ImageData.Int16 [| 1s; -2s |], Image.decode BitPix.Int16 (ReadOnlySpan [| 0uy; 1uy; 0xFFuy; 0xFEuy |]))
    Assert.Equal(ImageData.Int32 [| 0x01020304 |], Image.decode BitPix.Int32 (ReadOnlySpan [| 1uy; 2uy; 3uy; 4uy |]))
    Assert.Equal(
        ImageData.Float32 [| 1.0f |],
        Image.decode BitPix.Float32 (ReadOnlySpan [| 0x3Fuy; 0x80uy; 0uy; 0uy |])
    )
    Assert.Equal(
        ImageData.Float64 [| -2.0 |],
        Image.decode BitPix.Float64 (ReadOnlySpan [| 0xC0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy |])
    )

[<Fact>]
let ``encodes big-endian bytes`` () =
    Assert.Equal<byte[]>([| 0uy; 1uy; 0xFFuy; 0xFEuy |], Image.encode (ImageData.Int16 [| 1s; -2s |]))
    Assert.Equal<byte[]>([| 0x3Fuy; 0x80uy; 0uy; 0uy |], Image.encode (ImageData.Float32 [| 1.0f |]))
    Assert.Equal<byte[]>([| 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 1uy |], Image.encode (ImageData.Int64 [| 1L |]))

[<Fact>]
let ``decoding encoded data gives it back`` () =
    check (
        forAll
            image
            (fun image ->
                let bytes = Image.encode image.Data
                bytes.Length = image.Length * image.Data.BitPix.Size
                && Image.decode image.Data.BitPix (ReadOnlySpan bytes) = image.Data
            )
    )

[<Fact>]
let ``shape is reported in C order`` () =
    Assert.Equal<int[]>([| 4; 10 |], Image.shapeOfAxes [| 10L; 4L |])
    Assert.Equal<int64[]>([| 10L; 4L |], Image.axesOfShape [| 4; 10 |])

[<Fact>]
let ``applies BZERO and BSCALE and maps BLANK to NaN`` () =
    let image = {
        Image.create [| 3 |] (ImageData.Int16 [| 0s; 10s; -1s |]) with
            BZero = 100.0
            BScale = 0.5
            Blank = Some -1L
    }
    let values = Image.toFloat64 image
    Assert.Equal(100.0, values[0])
    Assert.Equal(105.0, values[1])
    Assert.True(Double.IsNaN values[2])

[<Fact>]
let ``unsigned 16-bit images round trip through signed storage`` () =
    let values = [| 0us; 1us; 32767us; 32768us; 65535us |]
    let image = Image.ofUInt16 [| 5 |] values
    Assert.True(Image.isUInt16 image)
    Assert.Equal<uint16[]>(values, Image.toUInt16 image)
    Assert.Equal<float[]>([| 0.0; 1.0; 32767.0; 32768.0; 65535.0 |], Image.toFloat64 image)

[<Fact>]
let ``unsigned 32-bit images round trip through signed storage`` () =
    let values = [| 0u; 2147483647u; 2147483648u; 4294967295u |]
    let image = Image.ofUInt32 [| 4 |] values
    Assert.True(Image.isUInt32 image)
    Assert.Equal<uint32[]>(values, Image.toUInt32 image)

[<Fact>]
let ``create rejects a shape that does not match the data`` () =
    Assert.Throws<ArgumentException>(fun () -> Image.create [| 2; 2 |] (ImageData.UInt8 [| 1uy |]) |> ignore)
    |> ignore

// Cutouts: the arithmetic that says which parts of a data unit a sub-region needs, and the reads
// that follow from it.

[<Fact>]
let ``a region is one contiguous run per line along the fastest axis`` () =
    // A 4 x 5 image, rows of five. The region is rows 1..2, columns 2..4.
    let runs = Image.regionRuns [| 4; 5 |] [| 1; 2 |] [| 2; 3 |] |> List.ofSeq
    Assert.Equal<(int64 * int) list>([ 7L, 3; 12L, 3 ], runs)

    // The whole image is one run per row.
    Assert.Equal<(int64 * int) list>(
        [ 0L, 5; 5L, 5; 10L, 5; 15L, 5 ],
        Image.regionRuns [| 4; 5 |] [| 0; 0 |] [| 4; 5 |] |> List.ofSeq
    )

    // A 1-D image is a single run.
    Assert.Equal<(int64 * int) list>([ 2L, 3 ], Image.regionRuns [| 8 |] [| 2 |] [| 3 |] |> List.ofSeq)

    // Three dimensions count like an odometer over the outer axes.
    Assert.Equal<(int64 * int) list>(
        [ 5L, 2; 9L, 2; 17L, 2; 21L, 2 ],
        Image.regionRuns [| 2; 3; 4 |] [| 0; 1; 1 |] [| 2; 2; 2 |] |> List.ofSeq
    )

    // An empty region reads nothing.
    Assert.Empty(Image.regionRuns [| 4; 5 |] [| 1; 1 |] [| 0; 3 |])

[<Fact>]
let ``a region outside the image or of the wrong rank is refused`` () =
    Assert.Throws<ArgumentException>(fun () ->
        Image.regionRuns [| 4; 5 |] [| 3; 0 |] [| 2; 5 |] |> List.ofSeq |> ignore
    )
    |> ignore

    Assert.Throws<ArgumentException>(fun () ->
        Image.regionRuns [| 4; 5 |] [| -1; 0 |] [| 1; 1 |] |> List.ofSeq |> ignore
    )
    |> ignore

    Assert.Throws<ArgumentException>(fun () -> Image.regionRuns [| 4; 5 |] [| 0 |] [| 1 |] |> List.ofSeq |> ignore)
    |> ignore

[<Fact>]
let ``a cutout reads the same values the whole image would give`` () =
    // 4 rows of 5, values 0..19, with scaling so the physical values differ from the stored ones.
    let image = {
        Image.create [| 4; 5 |] (ImageData.Int16(Array.init 20 int16)) with
            BZero = 100.0
            BScale = 2.0
    }

    let bytes = Fits.toBytes [ Fits.primary Header.empty image ]
    use file = Fits.openBytes bytes

    let cutout = Fits.readCutout file 0 [| 1; 2 |] [| 2; 3 |]
    Assert.Equal<int[]>([| 2; 3 |], cutout.Shape)

    match cutout.Data with
    | ImageData.Int16 values -> Assert.Equal<int16[]>([| 7s; 8s; 9s; 12s; 13s; 14s |], values)
    | other -> failwith $"expected int16 but got %A{other}"

    // The scaling keywords come along, so physical values match the full read.
    let whole = Image.toFloat64 (Fits.readImage file 0)
    let expected = [|
        for r in 1..2 do
            for c in 2..4 -> whole[r * 5 + c]
    |]
    Assert.Equal<float[]>(expected, Image.toFloat64 cutout)
    Assert.Equal(100.0, cutout.BZero)
    Assert.Equal(2.0, cutout.BScale)

[<Fact>]
let ``a cutout of the whole image is the whole image`` () =
    let image = Image.create [| 3; 2 |] (ImageData.Float32(Array.init 6 float32))
    let bytes = Fits.toBytes [ Fits.primary Header.empty image ]
    use file = Fits.openBytes bytes
    Assert.Equal(Fits.readImage file 0, Fits.readCutout file 0 [| 0; 0 |] [| 3; 2 |])

[<Fact>]
let ``cutouts work in three dimensions and on extensions`` () =
    let cube = Image.create [| 2; 3; 4 |] (ImageData.Int32(Array.init 24 id))

    let bytes =
        Fits.toBytes [ Fits.emptyPrimary Header.empty; Fits.imageExtension Header.empty cube ]

    use file = Fits.openBytes bytes
    let cutout = Fits.readCutout file 1 [| 0; 1; 1 |] [| 2; 2; 2 |]
    Assert.Equal<int[]>([| 2; 2; 2 |], cutout.Shape)

    match cutout.Data with
    | ImageData.Int32 values -> Assert.Equal<int[]>([| 5; 6; 9; 10; 17; 18; 21; 22 |], values)
    | other -> failwith $"expected int32 but got %A{other}"

[<Fact>]
let ``a cutout of a table HDU fails`` () =
    let bytes =
        Fits.toBytes [
            Fits.emptyPrimary Header.empty
            Fits.binaryTable Header.empty [ Table.ofInt32s "ID" [| 1 |] ]
        ]

    use file = Fits.openBytes bytes
    Assert.True((Fits.tryReadCutout file 1 [| 0 |] [| 1 |]).IsError)
    Assert.Throws<InvalidOperationException>(fun () -> Fits.readCutout file 1 [| 0 |] [| 1 |] |> ignore)
    |> ignore
