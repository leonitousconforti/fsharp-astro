namespace FSharp.Astro.Fits

open System

/// The FITS checksum convention: a 32-bit ones' complement sum encoded as 16 ASCII characters.
[<RequireQualifiedAccess>]
module Checksum =
    /// The CHECKSUM value that stands in while the real one is computed.
    [<Literal>]
    let Placeholder = "0000000000000000"

    let private fold (sum: uint64) : uint64 =
        let mutable s = sum

        while s > 0xFFFFFFFFUL do
            s <- (s &&& 0xFFFFFFFFUL) + (s >>> 32)

        s

    /// Adds bytes to a running ones' complement sum. The byte count must be a multiple of 4, which
    /// every padded header and data unit satisfies.
    let accumulate (sum: uint32) (bytes: ReadOnlySpan<byte>) : uint32 =
        if bytes.Length % 4 <> 0 then
            invalidArg (nameof bytes) "byte count must be a multiple of 4"

        let mutable total = uint64 sum
        let mutable i = 0

        while i < bytes.Length do
            let word =
                (uint64 bytes[i] <<< 24)
                ||| (uint64 bytes[i + 1] <<< 16)
                ||| (uint64 bytes[i + 2] <<< 8)
                ||| uint64 bytes[i + 3]

            total <- total + word

            if i % 65536 = 65532 then
                total <- fold total

            i <- i + 4

        uint32 (fold total)

    /// The ones' complement sum of a byte sequence.
    let sum (bytes: ReadOnlySpan<byte>) : uint32 = accumulate 0u bytes

    /// Adds two ones' complement sums.
    let add (a: uint32) (b: uint32) : uint32 = uint32 (fold (uint64 a + uint64 b))

    let private excluded = [|
        0x3Auy
        0x3Buy
        0x3Cuy
        0x3Duy
        0x3Euy
        0x3Fuy
        0x40uy
        0x5Buy
        0x5Cuy
        0x5Duy
        0x5Euy
        0x5Fuy
        0x60uy
    |]

    /// Encodes a 32-bit value as 16 ASCII characters following the checksum convention.
    let encode (value: uint32) : string =
        let bytes = [| byte (value >>> 24); byte (value >>> 16); byte (value >>> 8); byte value |]

        let ascii = Array.zeroCreate<byte> 16

        for i in 0..3 do
            let quotient = int bytes[i] / 4 + int '0'
            let remainder = int bytes[i] % 4
            let ch = [| quotient + remainder; quotient; quotient; quotient |]
            let mutable check = true

            while check do
                check <- false

                for k in 0 .. excluded.Length - 1 do
                    for j in 0..2..2 do
                        if ch[j] = int excluded[k] || ch[j + 1] = int excluded[k] then
                            ch[j] <- ch[j] + 1
                            ch[j + 1] <- ch[j + 1] - 1
                            check <- true

            for j in 0..3 do
                ascii[4 * j + i] <- byte ch[j]

        let rotated = Array.init 16 (fun i -> char ascii[(i + 15) % 16])
        String rotated

    /// Decodes 16 checksum characters back to the 32-bit value.
    let decode (ascii: string) : uint32 =
        if ascii.Length <> 16 then
            invalidArg (nameof ascii) "a checksum is 16 characters"

        let unrotated = Array.init 16 (fun i -> int ascii[(i + 1) % 16] - int '0')
        let mutable value = 0u

        for i in 0..3 do
            let b = unrotated[i] + unrotated[4 + i] + unrotated[8 + i] + unrotated[12 + i]
            value <- (value <<< 8) ||| uint32 (b &&& 0xFF)

        value

    /// The DATASUM keyword value for a data unit: its sum in decimal.
    let dataSum (data: ReadOnlySpan<byte>) : string = (sum data).ToString()

    /// The CHECKSUM keyword value for an HDU whose header already contains CHECKSUM set to the
    /// placeholder, given the sum of its data unit.
    let checksum (headerBytes: ReadOnlySpan<byte>) (dataSum: uint32) : string =
        encode (~~~(add (sum headerBytes) dataSum))

    /// True when the ones' complement sum of a complete HDU is negative zero, as it is when the
    /// CHECKSUM keyword is correct.
    let verify (hduSum: uint32) : bool = hduSum = 0xFFFFFFFFu
