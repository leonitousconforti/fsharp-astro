module CompressTests

open System
open System.IO
open System.IO.Compression
open Xunit
open FSharp.Astro.Fits
open Generators

let private gzip (bytes: byte[]) : byte[] =
    use target = new MemoryStream()

    (use stream = new GZipStream(target, CompressionLevel.SmallestSize)
     stream.Write(bytes, 0, bytes.Length))

    target.ToArray()

/// The bytes of int16 values in FITS order.
let private bigEndian (values: int16[]) : byte[] =
    values
    |> Array.collect (fun v -> [| byte (int v >>> 8 &&& 0xFF); byte (int v &&& 0xFF) |])

/// Shuffles bytes into planes, which is what GZIP_2 does before deflating.
let private shuffle (size: int) (bytes: byte[]) : byte[] =
    let count = bytes.Length / size
    let out = Array.zeroCreate<byte> bytes.Length

    for plane in 0 .. size - 1 do
        for i in 0 .. count - 1 do
            out[plane * count + i] <- bytes[i * size + plane]

    out

/// A 4 x 6 image of int16, in tiles of one row, compressed the way the argument says.
let private build (cmpType: string) (tile: int16[] -> byte[]) =
    let rows = [| for r in 0..3 -> Array.init 6 (fun c -> int16 (r * 6 + c)) |]

    let header =
        Header.empty
        |> Header.set "ZIMAGE" (CardValue.Logical true)
        |> Header.set "ZBITPIX" (CardValue.Integer 16L)
        |> Header.set "ZNAXIS" (CardValue.Integer 2L)
        |> Header.set "ZNAXIS1" (CardValue.Integer 6L)
        |> Header.set "ZNAXIS2" (CardValue.Integer 4L)
        |> Header.set "ZTILE1" (CardValue.Integer 6L)
        |> Header.set "ZTILE2" (CardValue.Integer 1L)
        |> Header.set "ZCMPTYPE" (CardValue.String cmpType)

    let column =
        Table.ofVariable "COMPRESSED_DATA" ColumnType.UInt8 (rows |> Array.map (fun r -> ColumnData.UInt8s(tile r)))

    Fits.toBytes [ Fits.emptyPrimary Header.empty; Fits.binaryTable header [ column ] ]

let private expected = Array.init 24 float

[<Fact>]
let ``GZIP_1 tiles read back`` () =
    let bytes = build "GZIP_1" (bigEndian >> gzip)
    use file = Fits.openBytes bytes
    let image = Fits.readImage file 1
    Assert.Equal<int[]>([| 4; 6 |], image.Shape)
    Assert.Equal(BitPix.Int16, image.Data.BitPix)
    Assert.Equal<float[]>(expected, Image.toFloat64 image)

[<Fact>]
let ``GZIP_2 tiles are unshuffled before they are read`` () =
    let bytes = build "GZIP_2" (bigEndian >> shuffle 2 >> gzip)
    use file = Fits.openBytes bytes
    Assert.Equal<float[]>(expected, Image.toFloat64 (Fits.readImage file 1))

    // Reading a GZIP_2 tile as GZIP_1 would give the shuffled bytes back as values, so the two
    // really are different and the unshuffle is doing something.
    let wrong = build "GZIP_1" (bigEndian >> shuffle 2 >> gzip)
    use file = Fits.openBytes wrong
    Assert.NotEqual<float[]>(expected, Image.toFloat64 (Fits.readImage file 1))

[<Fact>]
let ``NOCOMPRESS tiles are stored as they are`` () =
    let bytes = build "NOCOMPRESS" bigEndian
    use file = Fits.openBytes bytes
    Assert.Equal<float[]>(expected, Image.toFloat64 (Fits.readImage file 1))

[<Fact>]
let ``RICE_1 tiles read back`` () =
    let riceTile (values: int16[]) =
        Rice.encode 2 Rice.DefaultBlockSize (values |> Array.map int)

    let bytes = build "RICE_1" riceTile
    use file = Fits.openBytes bytes
    let compressed = Fits.readCompressedImage file 1
    // BYTEPIX defaults to the width of the stored type when the header does not say.
    Assert.Equal(TileCompression.Rice(Rice.DefaultBlockSize, 2), compressed.Compression)
    Assert.Equal<float[]>(expected, Image.toFloat64 (Fits.readImage file 1))

[<Fact>]
let ``an algorithm this library does not read is refused, not guessed at`` () =
    let bytes = build "HCOMPRESS_1" bigEndian
    // Strict refuses the HDU outright.
    use strict = Fits.openBytes bytes

    match Fits.tryReadCompressedImage strict 1 with
    | Error(Violations issues) -> Assert.Contains(issues, fun i -> (Issue.describe i).Contains "HCOMPRESS_1")
    | other -> failwith $"expected a violation but got %A{other}"

    // Lenient gets as far as the tiles and then says what it cannot do, rather than inventing data.
    use lenient = Fits.openBytesWith FitsOptions.Lenient bytes
    let image = Fits.readCompressedImage lenient 1
    Assert.Equal(TileCompression.Unsupported "HCOMPRESS_1", image.Compression)

    Assert.Throws<InvalidOperationException>(fun () -> Compress.tile 0 image |> ignore)
    |> ignore

[<Fact>]
let ``a tile grid that does not divide the image is clipped at the edges`` () =
    // 5 columns in tiles of 2 gives three tiles across, the last one a single column wide.
    let header =
        Header.empty
        |> Header.set "ZIMAGE" (CardValue.Logical true)
        |> Header.set "ZBITPIX" (CardValue.Integer 16L)
        |> Header.set "ZNAXIS" (CardValue.Integer 2L)
        |> Header.set "ZNAXIS1" (CardValue.Integer 5L)
        |> Header.set "ZNAXIS2" (CardValue.Integer 3L)
        |> Header.set "ZTILE1" (CardValue.Integer 2L)
        |> Header.set "ZTILE2" (CardValue.Integer 2L)
        |> Header.set "ZCMPTYPE" (CardValue.String "NOCOMPRESS")

    // Six tiles: three across by two down, walking the first axis fastest.
    let image = Array.init 15 int16

    let tileValues (originRow: int) (originCol: int) (rows: int) (cols: int) = [|
        for r in originRow .. originRow + rows - 1 do
            for c in originCol .. originCol + cols - 1 -> image[r * 5 + c]
    |]

    let tiles = [|
        tileValues 0 0 2 2
        tileValues 0 2 2 2
        tileValues 0 4 2 1
        tileValues 2 0 1 2
        tileValues 2 2 1 2
        tileValues 2 4 1 1
    |]

    let column =
        Table.ofVariable
            "COMPRESSED_DATA"
            ColumnType.UInt8
            (tiles |> Array.map (fun t -> ColumnData.UInt8s(bigEndian t)))

    let bytes =
        Fits.toBytes [ Fits.emptyPrimary Header.empty; Fits.binaryTable header [ column ] ]

    use file = Fits.openBytes bytes
    let compressed = Fits.readCompressedImage file 1
    Assert.Equal(6, compressed.TileCount)
    Assert.Equal<int[]>([| 2; 3 |], compressed.TileCounts)
    Assert.Equal<int[]>([| 0; 4 |], Compress.tileOrigin 2 compressed)
    Assert.Equal<int[]>([| 2; 1 |], Compress.tileShape 2 compressed)
    Assert.Equal<int[]>([| 2; 0 |], Compress.tileOrigin 3 compressed)
    Assert.Equal<int[]>([| 1; 2 |], Compress.tileShape 3 compressed)
    Assert.Equal<float[]>(Array.init 15 float, Image.toFloat64 (Fits.readImage file 1))
