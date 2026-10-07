module ArchiveTests

open System
open System.IO
open Xunit
open FSharp.Astro.Fits
open Generators

/// Files this library did not write. See fixtures/archive/README.md for where they came from.
let private archive (name: string) =
    Path.Combine(__SOURCE_DIRECTORY__, "fixtures", "archive", name)

let private openArchive (name: string) = Fits.openFile (archive name)

[<Fact>]
let ``every archive file parses under strict handling`` () =
    for name in
        [
            "compressed_image.fits"
            "comp.fits"
            "compressed_float_bzero.fits"
            "random_groups.fits"
            "ascii.fits"
            "tdim.fits"
        ] do
        match Fits.tryOpenFile (archive name) with
        | Ok file ->
            use file = file
            Assert.NotEmpty file.Hdus
            Assert.Empty file.Warnings
        | Error e -> failwith $"{name} did not parse: %A{e}"

/// The tiles of a compressed image, which are the rows of a variable-length byte column.
let private tiles (file: FitsFile) =
    match (Table.tryColumn "COMPRESSED_DATA" (Fits.readTable file 1)).Value.Data with
    | ColumnData.Variable entries ->
        entries
        |> Array.map (fun e ->
            match e with
            | ColumnData.UInt8s bytes -> bytes
            | other -> failwith $"expected bytes but got %A{other}"
        )
    | other -> failwith $"expected a variable-length column but got %A{other}"

[<Fact>]
let ``the Rice decoder agrees with fpack on a image it can be checked by eye`` () =
    // A 10 x 10 image of 0 to 99, one tile per row, RICE_1 with BYTEPIX 2.
    use file = openArchive "compressed_image.fits"
    let tiles = tiles file
    Assert.Equal(10, tiles.Length)

    for row in 0..9 do
        let expected = Array.init 10 (fun i -> row * 10 + i)
        Assert.Equal<int[]>(expected, Rice.decode 2 32 10 tiles[row])

[<Fact>]
let ``the Rice decoder agrees with fpack on a real image`` () =
    // 440 x 300, one tile per row. Nothing says what the pixels should be, but a wrong decoder
    // gives noise, and noise does not stay inside a narrow range with no jumps between neighbours.
    use file = openArchive "comp.fits"
    let tiles = tiles file
    Assert.Equal(300, tiles.Length)
    let pixels = tiles |> Array.collect (Rice.decode 2 32 440)
    Assert.Equal(132000, pixels.Length)
    Assert.InRange(Array.min pixels, 0, 100)
    Assert.InRange(Array.max pixels, 500, 5000)

    let jumps =
        pixels |> Array.pairwise |> Array.filter (fun (a, b) -> abs (a - b) > 1000)

    Assert.Empty jumps

[<Fact>]
let ``the Rice encoder writes the bytes fpack wrote`` () =
    // Agreeing on the values is interoperability one way. Producing the same stream from them is
    // the other, and it says the block splitting matches cfitsio's and not merely the decoder's.
    for name, count in [ "compressed_image.fits", 10; "comp.fits", 440 ] do
        use file = openArchive name

        for tile in tiles file do
            let values = Rice.decode 2 32 count tile
            Assert.Equal<byte[]>(tile, Rice.encode 2 32 values)

[<Fact>]
let ``a real random groups file reads`` () =
    // An AIPS UV data set: five parameters over a 3 x 1 x 128 x 1 x 1 visibility array.
    use file = openArchive "random_groups.fits"
    Assert.Equal(HduKind.RandomGroups, file[0].Kind)
    let groups = Fits.readGroups file 0
    Assert.Equal(3, groups.Count)

    Assert.Equal<string[]>([| "UU"; "VV"; "WW"; "BASELINE"; "DATE" |], groups.Parameters |> Array.map (fun p -> p.Name))

    Assert.Equal<int[]>([| 1; 1; 128; 1; 3 |], groups.Shape)

    let parameters = Groups.parameters 0 groups
    Assert.Equal(5, parameters.Length)
    // A baseline number and a Julian date, which is what those two parameters hold.
    Assert.Equal(258.0, parameters[3])
    Assert.InRange(parameters[4], 2400000.0, 2500000.0)
    Assert.Equal<int[]>([| 1; 1; 128; 1; 3 |], (Groups.image 0 groups).Shape)

[<Fact>]
let ``a real ASCII table reads, undefined fields and all`` () =
    use file = openArchive "ascii.fits"
    Assert.Equal(HduKind.AsciiTable, file[1].Kind)
    let table = Fits.readAsciiTable file 1
    Assert.Equal(5, table.Rows)
    Assert.Equal(16, table.RowLength)

    Assert.Equal<(string * int * string) list>(
        [ "a", 1, "E10.4"; "b", 12, "I5" ],
        [ for c in table.Columns -> c.Name, c.Start, AsciiForm.format c.Form ]
    )

    let a = Table.toFloat64 (Ascii.column 0 table)
    Assert.Equal<float[]>([| 10.123; 5.2; 15.61; 345.0 |], [| a[0]; a[1]; a[2]; a[4] |])
    // The fourth row is blank in both fields, which is what undefined looks like in an ASCII table.
    Assert.True(Double.IsNaN a[3])
    let b = Table.toFloat64 (Ascii.column 1 table)
    Assert.Equal<float[]>([| 37.0; 23.0; 17.0; 345.0 |], [| b[0]; b[1]; b[2]; b[4] |])
    Assert.True(Double.IsNaN b[3])

[<Fact>]
let ``a real TDIM reads`` () =
    use file = openArchive "tdim.fits"
    let table = Fits.readTable file 1
    Assert.Equal<string[]>([| "target"; "V_mag" |], table.Columns |> Array.map (fun c -> c.Name))
    Assert.Equal<int64[] option>(Some [| 1L; 1L |], table.Columns[1].Dim)
    Assert.Equal<int[]>([| 1; 1 |], Table.cellShape table.Columns[1])

[<Fact>]
let ``a compressed image reads as the image it was`` () =
    use file = openArchive "compressed_image.fits"
    let info = file[1]
    Assert.Equal(HduKind.BinaryTable, info.Kind)
    Assert.True(Compress.isCompressedImage info)

    let compressed = Fits.readCompressedImage file 1
    Assert.Equal(BitPix.Int16, compressed.BitPix)
    Assert.Equal<int[]>([| 10; 10 |], compressed.Shape)
    Assert.Equal<int[]>([| 1; 10 |], compressed.TileShape)
    Assert.Equal(TileCompression.Rice(32, 2), compressed.Compression)
    Assert.Equal(10, compressed.TileCount)
    Assert.False compressed.Quantized

    // Fits.readImage sees through the table to the image, so nothing above it has to know.
    let image = Fits.readImage file 1
    Assert.Equal<int[]>([| 10; 10 |], image.Shape)
    Assert.Equal(BitPix.Int16, image.Data.BitPix)
    Assert.Equal<float[]>(Array.init 100 float, Image.toFloat64 image)

[<Fact>]
let ``a real compressed image puts its tiles back in the right places`` () =
    use file = openArchive "comp.fits"
    let compressed = Fits.readCompressedImage file 1
    Assert.Equal<int[]>([| 300; 440 |], compressed.Shape)
    Assert.Equal<int[]>([| 1; 440 |], compressed.TileShape)
    Assert.Equal(300, compressed.TileCount)

    let image = Fits.readImage file 1
    Assert.Equal(132000, image.Length)

    // Tile n is row n, so the first row of the image has to be the first tile decoded on its own.
    let firstTile = Compress.tile 0 compressed

    match firstTile, image.Data with
    | ImageData.Int16 tile, ImageData.Int16 all ->
        Assert.Equal<int16[]>(tile, Array.sub all 0 440)
        // And the last tile has to be the last row, which is what says the order is right.
        let lastTile =
            match Compress.tile 299 compressed with
            | ImageData.Int16 v -> v
            | other -> failwith $"%A{other}"

        Assert.Equal<int16[]>(lastTile, Array.sub all (299 * 440) 440)
    | _ -> failwith "expected int16 data"

[<Fact>]
let ``the tile grid is walked with the first axis fastest`` () =
    use file = openArchive "comp.fits"
    let compressed = Fits.readCompressedImage file 1
    Assert.Equal<int[]>([| 300; 1 |], compressed.TileCounts)
    Assert.Equal<int[]>([| 0; 0 |], Compress.tileOrigin 0 compressed)
    Assert.Equal<int[]>([| 7; 0 |], Compress.tileOrigin 7 compressed)
    Assert.Equal<int[]>([| 1; 440 |], Compress.tileShape 7 compressed)

[<Fact>]
let ``the image header is the compression machinery taken off`` () =
    use file = openArchive "compressed_image.fits"
    let header = Compress.imageHeader (Fits.readCompressedImage file 1)

    for keyword in
        [
            "ZIMAGE"
            "ZBITPIX"
            "ZNAXIS1"
            "ZTILE1"
            "ZCMPTYPE"
            "ZNAME1"
            "ZVAL1"
            "TFORM1"
            "TTYPE1"
            "XTENSION"
        ] do
        Assert.False(Header.contains keyword header, $"{keyword} should be gone")

[<Fact>]
let ``the unsigned convention on a compressed image still applies`` () =
    // Three pixels stored as signed shorts with BZERO 32768, which is how an unsigned image goes.
    use file = openArchive "compressed_float_bzero.fits"
    let image = Fits.readImage file 1
    Assert.Equal<int[]>([| 3 |], image.Shape)
    Assert.Equal(32768.0, image.BZero)
    Assert.Equal<float[]>([| 1.0; 2.0; 3.0 |], Image.toFloat64 image)
