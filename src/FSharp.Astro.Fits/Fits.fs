namespace FSharp.Astro.Fits

open System
open System.IO

/// Options for reading.
type FitsOptions = {
    /// How violations of the standard are handled.
    Strictness: Strictness
    /// Upper bound on header size, as a guard against runaway reads of corrupt input.
    MaxHeaderBlocks: int
} with

    /// Strict handling and a 100000-block (288 MB) header limit.
    static member Default: FitsOptions = { Strictness = Strict; MaxHeaderBlocks = 100000 }

    /// Lenient handling with the default header limit.
    static member Lenient: FitsOptions = { FitsOptions.Default with Strictness = Lenient }

/// An open FITS file: every header parsed, no data read. Dispose it to release the source.
[<Sealed>]
type FitsFile internal (source: IFitsSource, hdus: HduInfo[], warnings: Issue list, options: FitsOptions) =
    let mutable disposed = false

    member internal _.Source: IFitsSource =
        if disposed then
            raise (ObjectDisposedException(nameof FitsFile))

        source

    /// The HDUs in order.
    member _.Hdus: HduInfo list = List.ofArray hdus

    /// Number of HDUs.
    member _.Count: int = hdus.Length

    /// The HDU at an index.
    member _.Item
        with get (index: int): HduInfo = hdus[index]

    /// The first HDU.
    member _.Primary: HduInfo = hdus[0]

    /// Recoverable violations found while reading. Always empty under strict handling.
    member _.Warnings: Issue list = warnings

    /// The options the file was opened with.
    member _.Options: FitsOptions = options

    interface IDisposable with
        member _.Dispose() =
            if not disposed then
                disposed <- true
                source.Dispose()

/// Data to write with a header.
[<RequireQualifiedAccess>]
type HduData =
    /// No data unit.
    | Empty
    /// An image, encoded to big-endian on write.
    | Image of ImageData
    /// Bytes already in on-disk form, such as a table or a data unit copied from another file.
    | Bytes of byte[]

/// An HDU to write.
type Hdu = { Header: Header; Data: HduData }

/// Reading and writing whole files.
[<RequireQualifiedAccess>]
module Fits =
    let private issueAt (hdu: int) (offset: int64) (violation: Violation) : Issue = {
        Issue.create violation with
            Hdu = Some hdu
            Offset = Some offset
    }

    /// Reads header blocks from `position` until the END card appears, accumulating the blocks read
    /// so far in reverse. Ok None means the source ended cleanly before any block was read.
    let rec private readHeaderBlocks
        (options: FitsOptions)
        (source: IFitsSource)
        (hduIndex: int)
        (offset: int64)
        (blocks: byte[] list)
        (count: int)
        (position: int64)
        : Result<byte[] option, Issue> =
        let block = Array.zeroCreate<byte> Block.Size
        let n = Source.readExact source position (Span block)
        let first = count = 0

        if n = 0 && first then
            Ok None
        elif n < Block.Size then
            if first && hduIndex > 0 then
                Error(issueAt hduIndex position (Violation.TrailingBytes(int64 n)))
            elif first then
                Error(issueAt 0 position (Violation.NotAFitsFile $"only {n} bytes, less than one block"))
            else
                Error(issueAt hduIndex offset Violation.TruncatedHeader)
        elif
            hduIndex = 0
            && first
            && not (block.AsSpan(0, 8).SequenceEqual(ReadOnlySpan "SIMPLE  "B))
        then
            Error(issueAt 0 0L (Violation.NotAFitsFile "first keyword is not SIMPLE"))
        elif (Header.findEnd (ReadOnlySpan block)).IsSome then
            Ok(Some(block :: blocks |> List.rev |> Array.concat))
        elif count + 1 >= options.MaxHeaderBlocks then
            Error(issueAt hduIndex offset Violation.MissingEnd)
        else
            readHeaderBlocks options source hduIndex offset (block :: blocks) (count + 1) (position + int64 Block.Size)

    /// Walks every HDU of a source, parsing headers and recording where data lives.
    let private enumerate (options: FitsOptions) (source: IFitsSource) : Result<HduInfo[] * Issue list, FitsError> =
        let length = source.Length

        let finish (hdus: HduInfo list) (warnings: Issue seq) =
            Ok(hdus |> List.rev |> Array.ofList, List.ofSeq warnings)

        let rec walk (offset: int64) (index: int) (hdus: HduInfo list) (warnings: Issue seq) =
            if offset >= length then
                finish hdus warnings
            else
                match readHeaderBlocks options source index offset [] 0 offset with
                | Error issue when options.Strictness = Strict || Issue.isFatal issue -> Error(Violations [ issue ])
                | Error issue -> finish hdus (Seq.append warnings (Seq.singleton issue))
                | Ok None -> finish hdus warnings
                | Ok(Some headerBytes) ->
                    match Header.parse options.Strictness index offset (ReadOnlySpan headerBytes) with
                    | Error issues -> Error(Violations issues)
                    | Ok parsed ->
                        match HduLayout.tryOfHeader options.Strictness index parsed.Header with
                        | Error issues -> Error(Violations issues)
                        | Ok(layout, layoutIssues) ->
                            let dataOffset = offset + int64 parsed.Length
                            let dataLength = HduLayout.dataLength layout

                            let info = {
                                Index = index
                                Kind = layout.Kind
                                Header = parsed.Header
                                BitPix = layout.BitPix
                                Axes = layout.Axes
                                PCount = layout.PCount
                                GCount = layout.GCount
                                HeaderOffset = offset
                                HeaderLength = int64 parsed.Length
                                DataOffset = dataOffset
                                DataLength = dataLength
                            }

                            let warnings = Seq.concat [ warnings; parsed.Warnings; layoutIssues ]

                            if dataOffset + dataLength > length then
                                Error(
                                    Violations [
                                        issueAt
                                            index
                                            dataOffset
                                            (Violation.TruncatedData(dataLength, max 0L (length - dataOffset)))
                                    ]
                                )
                            elif info.DataEnd > length then
                                let issue = issueAt index dataOffset (Violation.UnpaddedData(info.DataEnd - length))

                                if options.Strictness = Strict then
                                    Error(Violations [ issue ])
                                else
                                    finish (info :: hdus) (Seq.append warnings (Seq.singleton issue))
                            else
                                walk info.DataEnd (index + 1) (info :: hdus) warnings

        if length < int64 Block.Size then
            Error(
                Violations [
                    issueAt 0 0L (Violation.NotAFitsFile $"only {length} bytes, less than one block")
                ]
            )
        else
            walk 0L 0 [] Seq.empty

    /// Opens a source, taking ownership of it. On failure the source is disposed.
    let tryOpenSource (options: FitsOptions) (source: IFitsSource) : Result<FitsFile, FitsError> =
        try
            match enumerate options source with
            | Ok(hdus, warnings) -> Ok(new FitsFile(source, hdus, warnings, options))
            | Error e ->
                source.Dispose()
                Error e
        with
        | :? FitsException as e ->
            source.Dispose()
            Error e.Error
        | e ->
            source.Dispose()
            Error(IoError e)

    /// Opens a file with the given options.
    let tryOpenFileWith (options: FitsOptions) (path: string) : Result<FitsFile, FitsError> =
        try
            tryOpenSource options (Source.ofFile path)
        with e ->
            Error(IoError e)

    /// Opens a file with strict handling.
    let tryOpenFile (path: string) : Result<FitsFile, FitsError> =
        tryOpenFileWith FitsOptions.Default path

    /// Opens bytes in memory with the given options.
    let tryOpenBytesWith (options: FitsOptions) (bytes: byte[]) : Result<FitsFile, FitsError> =
        tryOpenSource options (Source.ofBytes bytes)

    /// Opens bytes in memory with strict handling.
    let tryOpenBytes (bytes: byte[]) : Result<FitsFile, FitsError> =
        tryOpenBytesWith FitsOptions.Default bytes

    let private orRaise (result: Result<'T, FitsError>) : 'T =
        match result with
        | Ok v -> v
        | Error e -> raise (FitsException e)

    /// Opens a file with the given options, throwing FitsException on failure.
    let openFileWith (options: FitsOptions) (path: string) : FitsFile = tryOpenFileWith options path |> orRaise

    /// Opens a file with strict handling, throwing FitsException on failure.
    let openFile (path: string) : FitsFile = tryOpenFile path |> orRaise

    /// Opens bytes with the given options, throwing FitsException on failure.
    let openBytesWith (options: FitsOptions) (bytes: byte[]) : FitsFile =
        tryOpenBytesWith options bytes |> orRaise

    /// Opens bytes with strict handling, throwing FitsException on failure.
    let openBytes (bytes: byte[]) : FitsFile = tryOpenBytes bytes |> orRaise

    /// Opens a stream with the given options. A seekable stream is read in place and is disposed
    /// with the file unless leaveOpen is set. A stream that cannot seek is copied into memory and
    /// disposed right away unless leaveOpen is set.
    let tryOpenStreamWith (options: FitsOptions) (leaveOpen: bool) (stream: Stream) : Result<FitsFile, FitsError> =
        try
            if stream.CanSeek then
                tryOpenSource options (Source.ofStream stream leaveOpen)
            else
                use buffer = new MemoryStream()
                stream.CopyTo buffer

                if not leaveOpen then
                    stream.Dispose()

                tryOpenSource options (Source.ofBytes (buffer.ToArray()))
        with e ->
            Error(IoError e)

    /// Opens a stream with strict handling, taking ownership of it.
    let tryOpenStream (stream: Stream) : Result<FitsFile, FitsError> =
        tryOpenStreamWith FitsOptions.Default false stream

    /// Opens a stream with the given options, throwing FitsException on failure.
    let openStreamWith (options: FitsOptions) (leaveOpen: bool) (stream: Stream) : FitsFile =
        tryOpenStreamWith options leaveOpen stream |> orRaise

    /// Opens a stream with strict handling, taking ownership of it, throwing FitsException on failure.
    let openStream (stream: Stream) : FitsFile = tryOpenStream stream |> orRaise

    /// The HDUs of a file.
    let hdus (file: FitsFile) : HduInfo list = file.Hdus

    /// The first HDU whose EXTNAME matches, ignoring case.
    let tryFindByName (name: string) (file: FitsFile) : HduInfo option =
        file.Hdus
        |> List.tryFind (fun h ->
            match h.Name with
            | Some n -> String.Equals(n, name, StringComparison.OrdinalIgnoreCase)
            | None -> false
        )

    /// The data unit of an HDU, without padding.
    let readData (file: FitsFile) (index: int) : byte[] =
        let info = file[index]

        if info.DataLength > int64 Int32.MaxValue then
            invalidOp $"data unit of HDU {index} is {info.DataLength} bytes, larger than a single array"

        Source.readBytes file.Source info.DataOffset (int info.DataLength)

    /// The data unit of an HDU including its padding, as it sits on disk.
    let readPaddedData (file: FitsFile) (index: int) : byte[] =
        let info = file[index]
        let padded = Block.padded info.DataLength

        if padded > int64 Int32.MaxValue then
            invalidOp $"data unit of HDU {index} is {padded} bytes, larger than a single array"

        let buffer = Array.zeroCreate<byte>(int padded)
        Source.readExact file.Source info.DataOffset (Span buffer) |> ignore
        buffer

    /// The raw header bytes of an HDU, as they sit on disk.
    let readHeaderBytes (file: FitsFile) (index: int) : byte[] =
        let info = file[index]
        Source.readBytes file.Source info.HeaderOffset (int info.HeaderLength)

    /// The compressed image in a binary table HDU written under the tiled compression convention.
    let tryReadCompressedImage (file: FitsFile) (index: int) : Result<CompressedImage, FitsError> =
        let info = file[index]

        if Compress.isCompressedImage info then
            match Compress.tryOfHdu file.Options.Strictness info (readData file index) with
            | Ok(image, _) -> Ok image
            | Error issues -> Error(Violations issues)
        else
            Error(
                Violations [
                    {
                        Issue.create (
                            Violation.InvalidKeywordValue("ZIMAGE", $"HDU {index} does not carry a compressed image")
                        ) with
                            Hdu = Some index
                    }
                ]
            )

    /// The compressed image in a binary table HDU, throwing FitsException on failure.
    let readCompressedImage (file: FitsFile) (index: int) : CompressedImage =
        match tryReadCompressedImage file index with
        | Ok image -> image
        | Error e -> raise (FitsException e)

    /// The image in a primary or IMAGE HDU, or in a binary table HDU that holds one under the
    /// tiled compression convention. A compressed image comes back put together, with the shape
    /// and the stored type it had before it was compressed.
    let readImage (file: FitsFile) (index: int) : Image =
        let info = file[index]

        if Compress.isCompressedImage info then
            Compress.toImage (readCompressedImage file index)
        else
            Image.ofHdu info (ReadOnlySpan(readData file index))

    /// A rectangular sub-region of an image, read without touching the rest of the data unit.
    /// `start` and `shape` are in C order, like Image.Shape, so the last entry of each is along
    /// NAXIS1. The result is an image of `shape` carrying the scaling keywords of the HDU.
    ///
    /// Only the bytes of the region are read: one run per line along the fastest axis. A cutout of
    /// a few hundred pixels from a large image costs a few hundred pixels of reading.
    let readCutout (file: FitsFile) (index: int) (start: int[]) (shape: int[]) : Image =
        let info = file[index]

        match info.Kind with
        | HduKind.Primary
        | HduKind.Image -> ()
        | kind -> invalidOp $"HDU {index} is a {kind}, not an image"

        let full = Image.shapeOfAxes info.Axes
        let size = info.BitPix.Size

        let elements = if shape.Length = 0 then 0 else shape |> Array.fold (*) 1

        let bytes = Array.zeroCreate<byte>(elements * size)
        let mutable at = 0

        for offset, count in Image.regionRuns full start shape do
            let run =
                Source.readBytes file.Source (info.DataOffset + offset * int64 size) (count * size)
            Array.blit run 0 bytes at run.Length
            at <- at + run.Length

        {
            Shape = shape
            Data = Image.decode info.BitPix (ReadOnlySpan bytes)
            BZero = defaultArg (Header.tryFloat "BZERO" info.Header) 0.0
            BScale = defaultArg (Header.tryFloat "BSCALE" info.Header) 1.0
            Blank = Header.tryInt "BLANK" info.Header
        }

    /// A rectangular sub-region of an image, or an error when the HDU is not an image.
    let tryReadCutout (file: FitsFile) (index: int) (start: int[]) (shape: int[]) : Result<Image, string> =
        let info = file[index]

        match info.Kind with
        | HduKind.Primary
        | HduKind.Image -> Ok(readCutout file index start shape)
        | kind -> Error $"HDU {index} is a {kind}, not an image"

    /// The image in a primary or IMAGE HDU, or an error when the HDU is something else.
    let tryReadImage (file: FitsFile) (index: int) : Result<Image, string> =
        let info = file[index]

        match info.Kind with
        | HduKind.Primary
        | HduKind.Image -> Ok(readImage file index)
        | _ when Compress.isCompressedImage info -> Ok(readImage file index)
        | kind -> Error $"HDU {index} is a {kind}, not an image"

    /// The random groups of a primary HDU that declares them, under the file's strictness.
    let tryReadGroups (file: FitsFile) (index: int) : Result<RandomGroups, FitsError> =
        let info = file[index]

        match info.Kind with
        | HduKind.RandomGroups ->
            match Groups.tryOfHdu file.Options.Strictness info (readData file index) with
            | Ok(groups, _) -> Ok groups
            | Error issues -> Error(Violations issues)
        | kind ->
            Error(
                Violations [
                    {
                        Issue.create (
                            Violation.InvalidKeywordValue("GROUPS", $"HDU {index} is a {kind}, not random groups")
                        ) with
                            Hdu = Some index
                    }
                ]
            )

    /// The random groups of a primary HDU, throwing FitsException on failure.
    let readGroups (file: FitsFile) (index: int) : RandomGroups =
        match tryReadGroups file index with
        | Ok groups -> groups
        | Error e -> raise (FitsException e)

    /// The binary table in a BINTABLE HDU, under the file's strictness.
    let tryReadTable (file: FitsFile) (index: int) : Result<BinaryTable, FitsError> =
        let info = file[index]

        match info.Kind with
        | HduKind.BinaryTable ->
            match Table.tryOfHdu file.Options.Strictness info (readData file index) with
            | Ok(table, _) -> Ok table
            | Error issues -> Error(Violations issues)
        | kind ->
            Error(
                Violations [
                    {
                        Issue.create (
                            Violation.InvalidKeywordValue("XTENSION", $"HDU {index} is a {kind}, not a binary table")
                        ) with
                            Hdu = Some index
                    }
                ]
            )

    /// The binary table in a BINTABLE HDU, throwing FitsException on failure.
    let readTable (file: FitsFile) (index: int) : BinaryTable =
        match tryReadTable file index with
        | Ok table -> table
        | Error e -> raise (FitsException e)

    /// The ASCII table in a TABLE HDU, under the file's strictness.
    let tryReadAsciiTable (file: FitsFile) (index: int) : Result<AsciiTable, FitsError> =
        let info = file[index]

        match info.Kind with
        | HduKind.AsciiTable ->
            match Ascii.tryOfHdu file.Options.Strictness info (readData file index) with
            | Ok(table, _) -> Ok table
            | Error issues -> Error(Violations issues)
        | kind ->
            Error(
                Violations [
                    {
                        Issue.create (
                            Violation.InvalidKeywordValue("XTENSION", $"HDU {index} is a {kind}, not an ASCII table")
                        ) with
                            Hdu = Some index
                    }
                ]
            )

    /// The ASCII table in a TABLE HDU, throwing FitsException on failure.
    let readAsciiTable (file: FitsFile) (index: int) : AsciiTable =
        match tryReadAsciiTable file index with
        | Ok table -> table
        | Error e -> raise (FitsException e)

    /// Decodes the header of an HDU into a record type that declares its own decoder.
    let tryReadAs<'T when 'T :> IFitsRecord<'T>> (file: FitsFile) (index: int) : Result<'T, FitsError> =
        Decode.run 'T.Decoder file[index].Header
        |> Result.mapError (fun issues -> Violations(issues |> List.map (fun i -> { i with Hdu = Some index })))

    /// Decodes the header of an HDU into a record type that declares its own decoder, throwing
    /// FitsException on failure.
    let readAs<'T when 'T :> IFitsRecord<'T>> (file: FitsFile) (index: int) : 'T =
        match tryReadAs<'T> file index with
        | Ok v -> v
        | Error e -> raise (FitsException e)

    /// Reads the rows of a table HDU as records, mapping columns to fields by name. Binary and
    /// ASCII tables both work; the HDU says which it is.
    let tryReadRecords<'T> (file: FitsFile) (index: int) : Result<'T[], FitsError> =
        let locate issues =
            Violations(issues |> List.map (fun i -> { i with Hdu = Some index }))

        match file[index].Kind with
        | HduKind.AsciiTable ->
            tryReadAsciiTable file index
            |> Result.bind (fun table -> Record.tryOfAsciiTable<'T> table |> Result.mapError locate)
        | _ ->
            tryReadTable file index
            |> Result.bind (fun table -> Record.tryOfTable<'T> table |> Result.mapError locate)

    /// Reads the rows of a table HDU as records, throwing FitsException on failure.
    let readRecords<'T> (file: FitsFile) (index: int) : 'T[] =
        match tryReadRecords<'T> file index with
        | Ok rows -> rows
        | Error e -> raise (FitsException e)

    // Writing

    /// An HDU whose header already carries valid mandatory keywords and whose data is in disk form.
    let private raw (header: Header) (data: HduData) : Hdu = { Header = header; Data = data }

    /// Builds the header for a layout: mandatory cards first, then the user's cards with any
    /// mandatory keywords removed.
    let private build (layout: HduLayout) (extra: Card list) (header: Header) : Header =
        let keep =
            Header.removeAll (HduLayout.mandatoryKeywords 999 @ [ "BZERO"; "BSCALE"; "BLANK" ]) header
        keep |> Header.prepend (HduLayout.mandatoryCards layout @ extra)

    /// A primary HDU holding an image. User cards follow the mandatory ones.
    let primary (header: Header) (image: Image) : Hdu =
        let layout = {
            Kind = HduKind.Primary
            BitPix = image.Data.BitPix
            Axes = Image.axesOfShape image.Shape
            PCount = 0L
            GCount = 1L
        }

        raw (build layout (Image.scalingCards image) header) (HduData.Image image.Data)

    /// A primary HDU with no data, for files whose content is in extensions.
    let emptyPrimary (header: Header) : Hdu =
        let layout = {
            Kind = HduKind.Primary
            BitPix = BitPix.UInt8
            Axes = [||]
            PCount = 0L
            GCount = 1L
        }

        raw (build layout [] header) HduData.Empty

    let private groupPattern =
        Text.RegularExpressions.Regex(
            @"^P(TYPE|SCAL|ZERO)[0-9]+$",
            Text.RegularExpressions.RegexOptions.CultureInvariant
        )

    /// A primary HDU in the random groups layout. The shape is one group's array in C order,
    /// without the NAXIS1 of zero that marks the layout. Groups carry stored values, so BZERO and
    /// BSCALE for the arrays and PZEROn and PSCALn for the parameters only describe them.
    let randomGroups (header: Header) (shape: int[]) (parameters: ParameterInfo list) (groups: Group list) : Hdu =
        let bitPix, bytes = Groups.encode shape parameters.Length groups

        let layout = {
            Kind = HduKind.RandomGroups
            BitPix = bitPix
            // NAXIS1 is zero and the array axes follow it, in FITS order.
            Axes = Array.append [| 0L |] (Image.axesOfShape shape)
            PCount = int64 parameters.Length
            GCount = int64 groups.Length
        }

        let numbered = parameters |> List.mapi (fun i p -> { p with Number = i + 1 })

        // build strips the array scaling keywords so that the image writer can regenerate them
        // from the image. There is no Image here to regenerate them from, so carry them over.
        let scaling = [
            for keyword in [ "BSCALE"; "BZERO"; "BLANK" ] do
                match Header.tryValue keyword header with
                | Some value -> Card.Value(keyword, value, Some "physical = BZERO + BSCALE * stored")
                | None -> ()
        ]

        let keep =
            header
            |> Header.filter (fun card ->
                match card with
                | Card.Value(k, _, _) -> not (groupPattern.IsMatch k)
                | _ -> true
            )

        raw (build layout (scaling @ Groups.parameterCards numbered) keep) (HduData.Bytes bytes)

    /// An IMAGE extension. User cards, including EXTNAME, follow the mandatory ones.
    let imageExtension (header: Header) (image: Image) : Hdu =
        let layout = {
            Kind = HduKind.Image
            BitPix = image.Data.BitPix
            Axes = Image.axesOfShape image.Shape
            PCount = 0L
            GCount = 1L
        }

        raw (build layout (Image.scalingCards image) header) (HduData.Image image.Data)

    let private tablePattern =
        Text.RegularExpressions.Regex(
            @"^T(FORM|TYPE|UNIT|NULL|SCAL|ZERO|DIM|DISP|BCOL)[0-9]+$",
            Text.RegularExpressions.RegexOptions.CultureInvariant
        )

    /// A BINTABLE extension. User cards, including EXTNAME, follow the mandatory ones and the
    /// column descriptors.
    let binaryTable (header: Header) (columns: Column list) : Hdu =
        let infos, rows, rowLength, rowBytes, heap = Table.encode columns

        let layout = {
            Kind = HduKind.BinaryTable
            BitPix = BitPix.UInt8
            Axes = [| int64 rowLength; int64 rows |]
            PCount = int64 heap.Length
            GCount = 1L
        }

        let descriptors =
            Card.Value("TFIELDS", CardValue.Integer(int64 infos.Length), Some "number of fields in each row")
            :: Table.descriptorCards infos

        let keep =
            header
            |> Header.filter (fun card ->
                match card with
                | Card.Value(k, _, _) -> not (tablePattern.IsMatch k) && k <> "TFIELDS" && k <> "THEAP"
                | _ -> true
            )

        raw (build layout descriptors keep) (HduData.Bytes(Array.append rowBytes heap))

    /// A TABLE extension, the ASCII kind. Fields are laid out left to right with one blank
    /// between, and TBCOLn is written from that layout. User cards, including EXTNAME, follow the
    /// mandatory ones and the field descriptors.
    let asciiTable (header: Header) (columns: AsciiColumn list) : Hdu =
        let infos, rows, rowLength, rowBytes = Ascii.encode columns

        let layout = {
            Kind = HduKind.AsciiTable
            BitPix = BitPix.UInt8
            Axes = [| int64 rowLength; int64 rows |]
            PCount = 0L
            GCount = 1L
        }

        let descriptors =
            Card.Value("TFIELDS", CardValue.Integer(int64 infos.Length), Some "number of fields in each row")
            :: Ascii.descriptorCards infos

        let keep =
            header
            |> Header.filter (fun card ->
                match card with
                | Card.Value(k, _, _) -> not (tablePattern.IsMatch k) && k <> "TFIELDS" && k <> "THEAP"
                | _ -> true
            )

        raw (build layout descriptors keep) (HduData.Bytes rowBytes)

    /// An HDU copied from an open file: its header as read and its data unit as stored.
    let copy (file: FitsFile) (index: int) : Hdu =
        raw file[index].Header (HduData.Bytes(readData file index))

    let private dataBytes (hdu: Hdu) : byte[] =
        match hdu.Data with
        | HduData.Empty -> [||]
        | HduData.Image data -> Image.encode data
        | HduData.Bytes bytes -> bytes

    /// Serializes one HDU to its header bytes and data bytes, validating both against the standard.
    let private serialize (index: int) (hdu: Hdu) : Result<byte[] * byte[] * HduLayout, FitsError> =
        match Header.tryToBytes hdu.Header with
        | Error issues -> Error(Violations(issues |> List.map (fun i -> { i with Hdu = Some index })))
        | Ok headerBytes ->
            match HduLayout.tryOfHeader Strict index hdu.Header with
            | Error issues -> Error(Violations issues)
            | Ok(layout, _) ->
                let data = dataBytes hdu
                let expected = HduLayout.dataLength layout

                if expected <> int64 data.Length then
                    Error(
                        Violations [
                            {
                                Issue.create (Violation.DataLengthMismatch(expected, int64 data.Length)) with
                                    Hdu = Some index
                            }
                        ]
                    )
                else
                    Ok(headerBytes, data, layout)

    let private writePadding (stream: Stream) (layout: HduLayout) (dataLength: int64) =
        let padding = int (Block.padding dataLength)

        if padding > 0 then
            let fill =
                match layout.Kind with
                | HduKind.AsciiTable -> 0x20uy
                | _ -> 0uy

            stream.Write(Array.create padding fill, 0, padding)

    /// Writes HDUs to a stream. The first must be a primary HDU.
    let tryWriteTo (stream: Stream) (hdus: Hdu list) : Result<unit, FitsError> =
        if List.isEmpty hdus then
            Error(Violations [ Issue.create (Violation.MissingKeyword "SIMPLE") ])
        else
            try
                hdus
                |> List.mapi (fun i hdu -> i, hdu)
                |> List.fold
                    (fun state (i, hdu) ->
                        match state with
                        | Error e -> Error e
                        | Ok() ->
                            match serialize i hdu with
                            | Error e -> Error e
                            | Ok(headerBytes, data, layout) ->
                                stream.Write(headerBytes, 0, headerBytes.Length)
                                stream.Write(data, 0, data.Length)
                                writePadding stream layout (int64 data.Length)
                                Ok()
                    )
                    (Ok())
            with e ->
                Error(IoError e)

    /// Serializes HDUs to a byte array.
    let tryToBytes (hdus: Hdu list) : Result<byte[], FitsError> =
        use stream = new MemoryStream()
        tryWriteTo stream hdus |> Result.map (fun () -> stream.ToArray())

    /// Writes HDUs to a file, replacing any existing file.
    let tryWriteFile (path: string) (hdus: Hdu list) : Result<unit, FitsError> =
        try
            use stream = new FileStream(path, FileMode.Create, FileAccess.Write, FileShare.None)
            tryWriteTo stream hdus
        with e ->
            Error(IoError e)

    /// Writes HDUs to a stream, throwing FitsException on failure.
    let writeTo (stream: Stream) (hdus: Hdu list) : unit = tryWriteTo stream hdus |> orRaise

    /// Serializes HDUs to a byte array, throwing FitsException on failure.
    let toBytes (hdus: Hdu list) : byte[] = tryToBytes hdus |> orRaise

    /// Writes HDUs to a file, throwing FitsException on failure.
    let writeFile (path: string) (hdus: Hdu list) : unit = tryWriteFile path hdus |> orRaise

    // Checksums

    /// Sets DATASUM and CHECKSUM on an HDU so that a reader can verify it.
    let tryWithChecksum (hdu: Hdu) : Result<Hdu, FitsError> =
        let data = dataBytes hdu
        let padded = Array.zeroCreate<byte>(int (Block.padded (int64 data.Length)))
        data.CopyTo(padded, 0)
        let dataSum = Checksum.sum (ReadOnlySpan padded)

        let header =
            hdu.Header
            |> Header.setWithComment "DATASUM" (CardValue.String(dataSum.ToString())) "data unit checksum"
            |> Header.setWithComment "CHECKSUM" (CardValue.String Checksum.Placeholder) "HDU checksum"

        match Header.tryToBytes header with
        | Error issues -> Error(Violations issues)
        | Ok headerBytes ->
            let checksum = Checksum.checksum (ReadOnlySpan headerBytes) dataSum

            Ok {
                hdu with
                    Header =
                        header
                        |> Header.setWithComment "CHECKSUM" (CardValue.String checksum) "HDU checksum"
            }

    /// Sets DATASUM and CHECKSUM on an HDU, throwing FitsException on failure.
    let withChecksum (hdu: Hdu) : Hdu = tryWithChecksum hdu |> orRaise

    /// The outcome of checking an HDU's checksum keywords.
    type ChecksumStatus =
        /// The HDU has no CHECKSUM keyword.
        | NoChecksum
        /// Both keywords agree with the bytes on disk.
        | Valid
        /// The data unit does not match DATASUM.
        | DataMismatch
        /// The HDU does not match CHECKSUM.
        | HduMismatch

    /// Checks the CHECKSUM and DATASUM keywords of an HDU against its bytes.
    let verifyChecksum (file: FitsFile) (index: int) : ChecksumStatus =
        let info = file[index]

        match Header.tryString "CHECKSUM" info.Header with
        | None -> NoChecksum
        | Some _ ->
            let headerBytes = readHeaderBytes file index
            let data = readPaddedData file index
            let dataSum = Checksum.sum (ReadOnlySpan data)

            let dataOk =
                match Header.tryString "DATASUM" info.Header with
                | Some declared -> declared.Trim() = dataSum.ToString()
                | None -> true

            if not dataOk then
                DataMismatch
            elif Checksum.verify (Checksum.add (Checksum.sum (ReadOnlySpan headerBytes)) dataSum) then
                Valid
            else
                HduMismatch
