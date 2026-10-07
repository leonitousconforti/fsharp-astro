namespace FSharp.Astro.Fits

open System
open System.Buffers.Binary
open System.Runtime.InteropServices

/// Array elements exactly as stored on disk, after conversion to host byte order.
[<RequireQualifiedAccess>]
type ImageData =
    | UInt8 of byte[]
    | Int16 of int16[]
    | Int32 of int32[]
    | Int64 of int64[]
    | Float32 of float32[]
    | Float64 of float[]

    /// Number of elements.
    member this.Length: int =
        match this with
        | UInt8 a -> a.Length
        | Int16 a -> a.Length
        | Int32 a -> a.Length
        | Int64 a -> a.Length
        | Float32 a -> a.Length
        | Float64 a -> a.Length

    /// The BITPIX this data is stored as.
    member this.BitPix: BitPix =
        match this with
        | UInt8 _ -> BitPix.UInt8
        | Int16 _ -> BitPix.Int16
        | Int32 _ -> BitPix.Int32
        | Int64 _ -> BitPix.Int64
        | Float32 _ -> BitPix.Float32
        | Float64 _ -> BitPix.Float64

/// An n-dimensional array with its scaling keywords.
type Image = {
    /// Axis lengths in C order: the last entry is NAXIS1 and varies fastest. A flat index into
    /// Data is row-major over this shape.
    Shape: int[]
    /// The stored values.
    Data: ImageData
    /// BZERO, the offset applied to stored values to get physical values.
    BZero: float
    /// BSCALE, the factor applied to stored values to get physical values.
    BScale: float
    /// BLANK, the stored integer value that means undefined.
    Blank: int64 option
} with

    /// Number of axes.
    member this.Rank: int = this.Shape.Length

    /// Number of elements.
    member this.Length: int = this.Data.Length

/// Decoding, encoding and scaling of image data.
[<RequireQualifiedAccess>]
module Image =
    /// Shape in C order for axes given in FITS order.
    let shapeOfAxes (axes: int64[]) : int[] = axes |> Array.rev |> Array.map int

    /// Axes in FITS order for a shape given in C order.
    let axesOfShape (shape: int[]) : int64[] = shape |> Array.rev |> Array.map int64

    /// An image with no scaling. A shape with no axes holds no data, as NAXIS = 0 does.
    let create (shape: int[]) (data: ImageData) : Image =
        let expected =
            if shape.Length = 0 then
                0L
            else
                shape |> Array.fold (fun acc n -> acc * int64 n) 1L

        if expected <> int64 data.Length then
            invalidArg (nameof data) $"shape implies {expected} elements but data has {data.Length}"

        {
            Shape = shape
            Data = data
            BZero = 0.0
            BScale = 1.0
            Blank = None
        }

    let private littleEndian = BitConverter.IsLittleEndian

    /// Element strides of a shape in C order: how far apart two elements are along each axis.
    /// The last axis has a stride of one, which is what makes a row of it contiguous on disk.
    let strides (shape: int[]) : int64[] =
        let result = Array.zeroCreate<int64> shape.Length
        let mutable stride = 1L

        for i in shape.Length - 1 .. -1 .. 0 do
            result[i] <- stride
            stride <- stride * int64 shape[i]

        result

    /// Checks that a region lies inside a shape and has the same rank.
    let private checkRegion (shape: int[]) (start: int[]) (size: int[]) =
        if start.Length <> shape.Length || size.Length <> shape.Length then
            invalidArg
                (nameof start)
                $"a region of a rank {shape.Length} image needs {shape.Length} offsets and lengths"

        for i in 0 .. shape.Length - 1 do
            if start[i] < 0 || size[i] < 0 || int64 start[i] + int64 size[i] > int64 shape[i] then
                invalidArg
                    (nameof start)
                    $"axis {i} of the region runs from {start[i]} for {size[i]}, outside the {shape[i]} available"

    /// The contiguous runs a sub-region of an image is made of, as element offsets into the whole
    /// image paired with element counts. A run is one line along the fastest axis, so a region of
    /// a 2-D image is one run per row. Nothing is read here; this is the arithmetic a cutout needs
    /// to know which parts of the data unit to ask for.
    let regionRuns (shape: int[]) (start: int[]) (size: int[]) : (int64 * int) seq =
        checkRegion shape start size

        if shape.Length = 0 || size |> Array.exists (fun n -> n = 0) then
            Seq.empty
        else
            let strides = strides shape
            let last = shape.Length - 1
            let outer = size[.. last - 1]

            let count = if outer.Length = 0 then 1 else outer |> Array.fold (*) 1

            seq {
                // Walk the outer axes in row-major order, counting like an odometer.
                let index = Array.zeroCreate<int>(max 0 last)

                for _ in 1..count do
                    let mutable offset = int64 start[last] * strides[last]

                    for i in 0 .. last - 1 do
                        offset <- offset + int64 (start[i] + index[i]) * strides[i]

                    yield offset, size[last]

                    let mutable axis = last - 1
                    let mutable carry = true

                    while carry && axis >= 0 do
                        index[axis] <- index[axis] + 1

                        if index[axis] >= size[axis] then
                            index[axis] <- 0
                            axis <- axis - 1
                        else
                            carry <- false
            }

    /// Decodes big-endian bytes into host-order elements.
    let decode (bitPix: BitPix) (bytes: ReadOnlySpan<byte>) : ImageData =
        let size = bitPix.Size

        if bytes.Length % size <> 0 then
            invalidArg (nameof bytes) $"byte count {bytes.Length} is not a multiple of the element size {size}"

        let count = bytes.Length / size

        match bitPix with
        | BitPix.UInt8 -> ImageData.UInt8(bytes.ToArray())
        | BitPix.Int16 ->
            let arr = Array.zeroCreate<int16> count
            let source = MemoryMarshal.Cast<byte, int16> bytes
            let target = Span<int16> arr

            if littleEndian then
                BinaryPrimitives.ReverseEndianness(source, target)
            else
                source.CopyTo target

            ImageData.Int16 arr
        | BitPix.Int32 ->
            let arr = Array.zeroCreate<int32> count
            let source = MemoryMarshal.Cast<byte, int32> bytes
            let target = Span<int32> arr

            if littleEndian then
                BinaryPrimitives.ReverseEndianness(source, target)
            else
                source.CopyTo target

            ImageData.Int32 arr
        | BitPix.Int64 ->
            let arr = Array.zeroCreate<int64> count
            let source = MemoryMarshal.Cast<byte, int64> bytes
            let target = Span<int64> arr

            if littleEndian then
                BinaryPrimitives.ReverseEndianness(source, target)
            else
                source.CopyTo target

            ImageData.Int64 arr
        | BitPix.Float32 ->
            let arr = Array.zeroCreate<float32> count
            let source = MemoryMarshal.Cast<byte, int32> bytes
            let target = MemoryMarshal.Cast<float32, int32>(Span<float32> arr)

            if littleEndian then
                BinaryPrimitives.ReverseEndianness(source, target)
            else
                source.CopyTo target

            ImageData.Float32 arr
        | BitPix.Float64 ->
            let arr = Array.zeroCreate<float> count
            let source = MemoryMarshal.Cast<byte, int64> bytes
            let target = MemoryMarshal.Cast<float, int64>(Span<float> arr)

            if littleEndian then
                BinaryPrimitives.ReverseEndianness(source, target)
            else
                source.CopyTo target

            ImageData.Float64 arr

    /// Encodes host-order elements as big-endian bytes.
    let encode (data: ImageData) : byte[] =
        match data with
        | ImageData.UInt8 a -> Array.copy a
        | ImageData.Int16 a ->
            let out = Array.zeroCreate<byte>(a.Length * 2)
            let target = MemoryMarshal.Cast<byte, int16>(Span<byte> out)
            let source = ReadOnlySpan<int16> a

            if littleEndian then
                BinaryPrimitives.ReverseEndianness(source, target)
            else
                source.CopyTo target

            out
        | ImageData.Int32 a ->
            let out = Array.zeroCreate<byte>(a.Length * 4)
            let target = MemoryMarshal.Cast<byte, int32>(Span<byte> out)
            let source = ReadOnlySpan<int32> a

            if littleEndian then
                BinaryPrimitives.ReverseEndianness(source, target)
            else
                source.CopyTo target

            out
        | ImageData.Int64 a ->
            let out = Array.zeroCreate<byte>(a.Length * 8)
            let target = MemoryMarshal.Cast<byte, int64>(Span<byte> out)
            let source = ReadOnlySpan<int64> a

            if littleEndian then
                BinaryPrimitives.ReverseEndianness(source, target)
            else
                source.CopyTo target

            out
        | ImageData.Float32 a ->
            let out = Array.zeroCreate<byte>(a.Length * 4)
            let target = MemoryMarshal.Cast<byte, int32>(Span<byte> out)
            let source = MemoryMarshal.Cast<float32, int32>(ReadOnlySpan<float32> a)

            if littleEndian then
                BinaryPrimitives.ReverseEndianness(source, target)
            else
                source.CopyTo target

            out
        | ImageData.Float64 a ->
            let out = Array.zeroCreate<byte>(a.Length * 8)
            let target = MemoryMarshal.Cast<byte, int64>(Span<byte> out)
            let source = MemoryMarshal.Cast<float, int64>(ReadOnlySpan<float> a)

            if littleEndian then
                BinaryPrimitives.ReverseEndianness(source, target)
            else
                source.CopyTo target

            out

    /// Builds an image from an HDU's layout and header, decoding its data bytes.
    let ofHdu (info: HduInfo) (bytes: ReadOnlySpan<byte>) : Image =
        match info.Kind with
        | HduKind.Primary
        | HduKind.Image -> ()
        | kind -> invalidOp $"HDU {info.Index} is a {kind}, not an image"

        let shape = shapeOfAxes info.Axes
        let data = decode info.BitPix bytes

        {
            Shape = shape
            Data = data
            BZero = defaultArg (Header.tryFloat "BZERO" info.Header) 0.0
            BScale = defaultArg (Header.tryFloat "BSCALE" info.Header) 1.0
            Blank = Header.tryInt "BLANK" info.Header
        }

    /// Physical values as doubles: BZERO + BSCALE * stored, with BLANK mapped to NaN.
    let toFloat64 (image: Image) : float[] =
        let n = image.Length
        let out = Array.zeroCreate<float> n
        let zero = image.BZero
        let scale = image.BScale
        let identity = zero = 0.0 && scale = 1.0

        let inline scaled (x: float) =
            if identity then x else zero + scale * x

        match image.Data, image.Blank with
        | ImageData.UInt8 a, Some blank ->
            for i in 0 .. n - 1 do
                out[i] <- if int64 a[i] = blank then nan else scaled (float a[i])
        | ImageData.UInt8 a, None ->
            for i in 0 .. n - 1 do
                out[i] <- scaled (float a[i])
        | ImageData.Int16 a, Some blank ->
            for i in 0 .. n - 1 do
                out[i] <- if int64 a[i] = blank then nan else scaled (float a[i])
        | ImageData.Int16 a, None ->
            for i in 0 .. n - 1 do
                out[i] <- scaled (float a[i])
        | ImageData.Int32 a, Some blank ->
            for i in 0 .. n - 1 do
                out[i] <- if int64 a[i] = blank then nan else scaled (float a[i])
        | ImageData.Int32 a, None ->
            for i in 0 .. n - 1 do
                out[i] <- scaled (float a[i])
        | ImageData.Int64 a, Some blank ->
            for i in 0 .. n - 1 do
                out[i] <- if a[i] = blank then nan else scaled (float a[i])
        | ImageData.Int64 a, None ->
            for i in 0 .. n - 1 do
                out[i] <- scaled (float a[i])
        | ImageData.Float32 a, _ ->
            for i in 0 .. n - 1 do
                out[i] <- scaled (float a[i])
        | ImageData.Float64 a, _ ->
            for i in 0 .. n - 1 do
                out[i] <- scaled a[i]

        out

    /// Physical values as singles. See toFloat64.
    let toFloat32 (image: Image) : float32[] =
        match image.Data, image.Blank with
        | ImageData.Float32 a, _ when image.BZero = 0.0 && image.BScale = 1.0 -> Array.copy a
        | _ -> toFloat64 image |> Array.map float32

    /// True when the scaling keywords describe unsigned 16-bit integers stored as signed.
    let isUInt16 (image: Image) : bool =
        image.Data.BitPix = BitPix.Int16 && image.BZero = 32768.0 && image.BScale = 1.0

    /// True when the scaling keywords describe unsigned 32-bit integers stored as signed.
    let isUInt32 (image: Image) : bool =
        image.Data.BitPix = BitPix.Int32
        && image.BZero = 2147483648.0
        && image.BScale = 1.0

    /// The unsigned 16-bit values of an image stored with BZERO = 32768.
    let toUInt16 (image: Image) : uint16[] =
        match image.Data with
        | ImageData.Int16 a when isUInt16 image -> a |> Array.map (fun v -> uint16 (int v + 32768))
        | _ -> invalidOp "image is not stored as unsigned 16-bit integers"

    /// The unsigned 32-bit values of an image stored with BZERO = 2147483648.
    let toUInt32 (image: Image) : uint32[] =
        match image.Data with
        | ImageData.Int32 a when isUInt32 image -> a |> Array.map (fun v -> uint32 (int64 v + 2147483648L))
        | _ -> invalidOp "image is not stored as unsigned 32-bit integers"

    /// An image of unsigned 16-bit values, stored as signed with BZERO = 32768.
    let ofUInt16 (shape: int[]) (values: uint16[]) : Image =
        let stored = values |> Array.map (fun v -> int16 (int v - 32768))

        {
            create shape (ImageData.Int16 stored) with
                BZero = 32768.0
        }

    /// An image of unsigned 32-bit values, stored as signed with BZERO = 2147483648.
    let ofUInt32 (shape: int[]) (values: uint32[]) : Image =
        let stored = values |> Array.map (fun v -> int32 (int64 v - 2147483648L))

        {
            create shape (ImageData.Int32 stored) with
                BZero = 2147483648.0
        }

    /// The scaling cards an image needs in its header, omitting defaults.
    let scalingCards (image: Image) : Card list = [
        if image.BScale <> 1.0 then
            Card.Value("BSCALE", CardValue.Real image.BScale, Some "physical = BZERO + BSCALE * stored")
        if image.BZero <> 0.0 then
            Card.Value("BZERO", CardValue.Real image.BZero, Some "physical = BZERO + BSCALE * stored")
        match image.Blank with
        | Some blank -> Card.Value("BLANK", CardValue.Integer blank, Some "stored value meaning undefined")
        | None -> ()
    ]
