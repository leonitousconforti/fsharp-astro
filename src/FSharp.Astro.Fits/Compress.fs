namespace FSharp.Astro.Fits

open System
open System.IO
open System.IO.Compression

// The tiled image compression convention of section 10 of the standard, which is what fpack
// writes. A compressed image is a binary table: the image is cut into tiles, each tile is
// compressed into one row of a variable-length byte column, and the keywords describing the image
// it used to be are carried alongside with a Z in front of their names.
//
// The reader puts the image back together. Nothing here writes one.

/// How the tiles of a compressed image are stored, from ZCMPTYPE and the ZNAMEn parameters.
[<RequireQualifiedAccess>]
type TileCompression =
    /// RICE_1, the one fpack chooses unless told otherwise.
    | Rice of blockSize: int * bytePix: int
    /// GZIP_1, deflate over the tile's values in the order they are stored.
    | Gzip1
    /// GZIP_2, deflate over the same values with their bytes shuffled into planes first, which
    /// puts the high bytes of every value together and so compresses smooth data better.
    | Gzip2
    /// NOCOMPRESS, the tile stored as it is.
    | Uncompressed
    /// An algorithm this library does not implement, by its ZCMPTYPE name.
    | Unsupported of string

    /// The ZCMPTYPE value.
    member this.Name: string =
        match this with
        | Rice _ -> "RICE_1"
        | Gzip1 -> "GZIP_1"
        | Gzip2 -> "GZIP_2"
        | Uncompressed -> "NOCOMPRESS"
        | Unsupported name -> name

/// An image stored as the rows of a binary table, with the data unit loaded. Tiles are
/// decompressed on request.
type CompressedImage = {
    Header: Header
    /// ZBITPIX, the type of the image before it was compressed.
    BitPix: BitPix
    /// ZNAXISn in C order, the shape of the whole image.
    Shape: int[]
    /// ZTILEn in C order. A tile at an edge is clipped to what is left of the image.
    TileShape: int[]
    Compression: TileCompression
    /// BZERO of the image, which the convention keeps under its own name on the table.
    BZero: float
    /// BSCALE of the image.
    BScale: float
    /// ZBLANK, the stored value that means undefined.
    Blank: int64 option
    /// True when the tiles hold integers that a scale and a zero per tile turn back into floats.
    /// That is how fpack stores a floating point image, and this library does not read it yet.
    Quantized: bool
    /// The table the tiles are rows of.
    Table: BinaryTable
} with

    /// Number of tiles along each axis, in C order.
    member this.TileCounts: int[] =
        Array.map2 (fun size tile -> if tile <= 0 then 1 else (size + tile - 1) / tile) this.Shape this.TileShape

    /// Number of tiles in the image.
    member this.TileCount: int =
        if this.Shape.Length = 0 then
            0
        else
            this.TileCounts |> Array.fold (*) 1

/// Reading images stored under the tiled compression convention.
[<RequireQualifiedAccess>]
module Compress =

    /// True when an HDU is a binary table carrying a compressed image rather than a table.
    let isCompressedImage (info: HduInfo) : bool =
        info.Kind = HduKind.BinaryTable
        && (
            match Header.tryBool "ZIMAGE" info.Header with
            | Some true -> true
            | _ -> false
        )

    /// The value of a named ZNAMEn parameter, which is how the algorithms take their settings.
    let private parameter (name: string) (header: Header) : float option =
        let rec find n =
            if n > 999 then
                None
            else
                match Header.tryString $"ZNAME{n}" header with
                | Some found when String.Equals(found.Trim(), name, StringComparison.OrdinalIgnoreCase) ->
                    Header.tryFloat $"ZVAL{n}" header
                | Some _ -> find (n + 1)
                | None -> None

        find 1

    let private compressionOf (header: Header) (bitPix: BitPix) : TileCompression =
        match Header.tryString "ZCMPTYPE" header |> Option.map (fun s -> s.Trim()) with
        | Some "RICE_1"
        | Some "RICE_ONE" ->
            let blockSize =
                parameter "BLOCKSIZE" header
                |> Option.map int
                |> Option.defaultValue Rice.DefaultBlockSize

            // BYTEPIX defaults to the width of the stored type, which is what fpack assumes.
            let bytePix =
                parameter "BYTEPIX" header
                |> Option.map int
                |> Option.defaultValue (max 1 bitPix.Size)

            TileCompression.Rice(blockSize, bytePix)
        | Some "GZIP_1" -> TileCompression.Gzip1
        | Some "GZIP_2" -> TileCompression.Gzip2
        | Some "NOCOMPRESS" -> TileCompression.Uncompressed
        | Some other -> TileCompression.Unsupported other
        | None -> TileCompression.Unsupported ""

    /// Interprets a binary table HDU that carries a compressed image.
    let tryOfHdu
        (strictness: Strictness)
        (info: HduInfo)
        (bytes: byte[])
        : Result<CompressedImage * Issue list, Issue list> =
        if not (isCompressedImage info) then
            invalidOp $"HDU {info.Index} does not carry a compressed image"

        let locate (violation: Violation) = { Issue.create violation with Hdu = Some info.Index }

        match Table.tryOfHdu strictness info bytes with
        | Error issues -> Error issues
        | Ok(table, warnings) ->
            let header = info.Header

            let bitPix, bitPixIssues =
                match Header.tryInt "ZBITPIX" header |> Option.bind BitPix.TryOfCode with
                | Some b -> b, []
                | None -> BitPix.Int32, [ locate (Violation.MissingKeyword "ZBITPIX") ]

            let rank = Header.tryInt "ZNAXIS" header |> Option.defaultValue 0L |> int

            let axes = [|
                for n in 1..rank -> Header.tryInt $"ZNAXIS{n}" header |> Option.defaultValue 0L
            |]

            // ZTILEn defaults to a tile one row wide, which is what the standard says and what
            // every file in the wild does.
            let tiles = [|
                for n in 1..rank ->
                    match Header.tryInt $"ZTILE{n}" header with
                    | Some v -> v
                    | None -> if n = 1 then (if rank > 0 then axes[0] else 1L) else 1L
            |]

            let shapeIssues = [
                if rank = 0 then
                    locate (Violation.MissingKeyword "ZNAXIS")

                for n in 1..rank do
                    if Header.tryInt $"ZNAXIS{n}" header |> Option.isNone then
                        locate (Violation.MissingKeyword $"ZNAXIS{n}")
            ]

            let compression = compressionOf header bitPix

            let unsupported = [
                match compression with
                | TileCompression.Unsupported name ->
                    locate (
                        Violation.InvalidKeywordValue(
                            "ZCMPTYPE",
                            if name = "" then
                                "missing"
                            else
                                $"'{name}' is not an algorithm this library reads"
                        )
                    )
                | _ -> ()
            ]

            let quantized =
                table.Columns
                |> Array.exists (fun c -> String.Equals(c.Name, "ZSCALE", StringComparison.OrdinalIgnoreCase))

            let issues = warnings @ bitPixIssues @ shapeIssues @ unsupported

            let image = {
                Header = header
                BitPix = bitPix
                Shape = Image.shapeOfAxes axes
                TileShape = Image.shapeOfAxes tiles
                Compression = compression
                BZero = defaultArg (Header.tryFloat "BZERO" header) 0.0
                BScale = defaultArg (Header.tryFloat "BSCALE" header) 1.0
                Blank = Header.tryInt "ZBLANK" header
                Quantized = quantized
                Table = table
            }

            if issues |> List.exists Issue.isFatal then
                Error issues
            elif strictness = Strict && not issues.IsEmpty then
                Error issues
            else
                Ok(image, issues)

    /// The header the image would have had, with the compression machinery taken off and the Z
    /// keywords put back under the names they stand for.
    let imageHeader (image: CompressedImage) : Header =
        let dropped =
            set [
                "ZIMAGE"
                "ZTENSION"
                "ZBITPIX"
                "ZNAXIS"
                "ZPCOUNT"
                "ZGCOUNT"
                "ZCMPTYPE"
                "ZBLANK"
                "ZQUANTIZ"
                "ZDITHER0"
                "ZMASKCMP"
                "ZSIMPLE"
                "ZEXTEND"
                "XTENSION"
                "BITPIX"
                "NAXIS"
                "NAXIS1"
                "NAXIS2"
                "PCOUNT"
                "GCOUNT"
                "TFIELDS"
                "THEAP"
                "EXTNAME"
            ]

        let pattern =
            Text.RegularExpressions.Regex(
                @"^(T(FORM|TYPE|UNIT|NULL|SCAL|ZERO|DIM|DISP|BCOL)[0-9]+|Z(NAXIS|TILE|NAME|VAL)[0-9]+)$",
                Text.RegularExpressions.RegexOptions.CultureInvariant
            )

        let kept =
            image.Header
            |> Header.filter (fun card ->
                match card with
                | Card.Value(keyword, _, _) -> not (dropped.Contains keyword) && not (pattern.IsMatch keyword)
                | _ -> true
            )

        // ZHECKSUM and ZDATASUM are the checksums of the image, so they go back to their own names.
        let renamed =
            [ "ZHECKSUM", "CHECKSUM"; "ZDATASUM", "DATASUM" ]
            |> List.fold
                (fun (header: Header) (from, into) ->
                    match Header.tryValue from header with
                    | Some value -> header |> Header.remove from |> Header.set into value
                    | None -> header
                )
                kept

        renamed

    /// The bytes of one tile as they are stored, and which column they came from.
    let private storedTile (index: int) (image: CompressedImage) : byte[] * string =
        let cell (name: string) =
            match Table.tryColumn name image.Table with
            | Some column ->
                match column.Data with
                | ColumnData.Variable entries when index < entries.Length ->
                    match entries[index] with
                    | ColumnData.UInt8s bytes when bytes.Length > 0 -> Some bytes
                    | _ -> None
                | _ -> None
            | None -> None

        // A tile the algorithm could not shrink is stored under one of the other two columns.
        match cell "COMPRESSED_DATA" with
        | Some bytes -> bytes, "COMPRESSED_DATA"
        | None ->
            match cell "GZIP_COMPRESSED_DATA" with
            | Some bytes -> bytes, "GZIP_COMPRESSED_DATA"
            | None ->
                match cell "UNCOMPRESSED_DATA" with
                | Some bytes -> bytes, "UNCOMPRESSED_DATA"
                | None -> [||], ""

    let private gunzip (bytes: byte[]) : byte[] =
        use source = new MemoryStream(bytes)
        use gzip = new GZipStream(source, CompressionMode.Decompress)
        use target = new MemoryStream()
        gzip.CopyTo target
        target.ToArray()

    /// Undoes the byte shuffle of GZIP_2, which stores all the first bytes of the values, then all
    /// the second bytes, and so on.
    let private unshuffle (size: int) (bytes: byte[]) : byte[] =
        if size <= 1 then
            bytes
        else
            let count = bytes.Length / size
            let out = Array.zeroCreate<byte> bytes.Length

            for plane in 0 .. size - 1 do
                for i in 0 .. count - 1 do
                    out[i * size + plane] <- bytes[plane * count + i]

            out

    /// The shape of the tile at a position in the grid, clipped to what is left of the image.
    let tileShape (index: int) (image: CompressedImage) : int[] =
        let counts = image.TileCounts
        let rank = image.Shape.Length
        let origin = Array.zeroCreate<int> rank
        let mutable remaining = index

        // The first axis varies fastest in FITS order, which is the last one in C order.
        for axis in rank - 1 .. -1 .. 0 do
            origin[axis] <- (remaining % counts[axis]) * image.TileShape[axis]
            remaining <- remaining / counts[axis]

        Array.init rank (fun axis -> min image.TileShape[axis] (image.Shape[axis] - origin[axis]))

    /// The offset of the tile at an index, in C order.
    let tileOrigin (index: int) (image: CompressedImage) : int[] =
        let counts = image.TileCounts
        let rank = image.Shape.Length
        let origin = Array.zeroCreate<int> rank
        let mutable remaining = index

        for axis in rank - 1 .. -1 .. 0 do
            origin[axis] <- (remaining % counts[axis]) * image.TileShape[axis]
            remaining <- remaining / counts[axis]

        origin

    /// The stored values of one tile, decompressed.
    let tile (index: int) (image: CompressedImage) : ImageData =
        if index < 0 || index >= image.TileCount then
            invalidArg (nameof index) $"tile {index} is outside the {image.TileCount} tiles"

        if image.Quantized then
            invalidOp
                "the tiles hold quantized integers that ZSCALE and ZZERO turn back into floats, which this library does not read yet"

        let count = tileShape index image |> Array.fold (*) 1
        let bytes, source = storedTile index image

        if bytes.Length = 0 then
            invalidOp $"tile {index} holds no data in any of the tile columns"

        let values (raw: byte[]) =
            Image.decode image.BitPix (ReadOnlySpan raw)

        match source with
        | "UNCOMPRESSED_DATA" -> values bytes
        | "GZIP_COMPRESSED_DATA" -> values (gunzip bytes)
        | _ ->
            match image.Compression with
            | TileCompression.Uncompressed -> values bytes
            | TileCompression.Gzip1 -> values (gunzip bytes)
            | TileCompression.Gzip2 -> values (unshuffle image.BitPix.Size (gunzip bytes))
            | TileCompression.Rice(blockSize, bytePix) ->
                let decoded = Rice.decode bytePix blockSize count bytes

                match image.BitPix with
                | BitPix.UInt8 -> ImageData.UInt8(decoded |> Array.map byte)
                | BitPix.Int16 -> ImageData.Int16(decoded |> Array.map int16)
                | BitPix.Int32 -> ImageData.Int32 decoded
                | BitPix.Int64 -> ImageData.Int64(decoded |> Array.map int64)
                | other -> invalidOp $"Rice coded tiles hold whole numbers, not {other}"
            | TileCompression.Unsupported name ->
                invalidOp $"'{name}' is not a compression algorithm this library reads"

    /// Copies elements between two arrays of the same stored type.
    let private blit (source: ImageData) (from: int) (target: ImageData) (into: int) (count: int) =
        match source, target with
        | ImageData.UInt8 a, ImageData.UInt8 b -> Array.blit a from b into count
        | ImageData.Int16 a, ImageData.Int16 b -> Array.blit a from b into count
        | ImageData.Int32 a, ImageData.Int32 b -> Array.blit a from b into count
        | ImageData.Int64 a, ImageData.Int64 b -> Array.blit a from b into count
        | ImageData.Float32 a, ImageData.Float32 b -> Array.blit a from b into count
        | ImageData.Float64 a, ImageData.Float64 b -> Array.blit a from b into count
        | _ -> invalidOp $"a tile holds {source.BitPix} but the image is {target.BitPix}"

    /// Puts the tiles back together into the image they were cut from. The result holds the stored
    /// values and the scaling keywords, exactly as reading an uncompressed image does.
    let toImage (image: CompressedImage) : Image =
        let total =
            if image.Shape.Length = 0 then
                0
            else
                image.Shape |> Array.fold (*) 1

        // A zeroed array of the stored type, which is what decoding zero bytes gives.
        let out =
            Image.decode image.BitPix (ReadOnlySpan(Array.zeroCreate<byte>(total * image.BitPix.Size)))

        for index in 0 .. image.TileCount - 1 do
            let origin = tileOrigin index image
            let shape = tileShape index image
            let data = tile index image

            if data.Length <> (shape |> Array.fold (*) 1) then
                invalidOp
                    $"tile {index} decompressed to {data.Length} values, not the {shape |> Array.fold (*) 1} its place needs"

            let mutable at = 0

            for offset, count in Image.regionRuns image.Shape origin shape do
                blit data at out (int offset) count
                at <- at + count

        {
            Shape = image.Shape
            Data = out
            BZero = image.BZero
            BScale = image.BScale
            Blank = image.Blank
        }
