namespace FSharp.Astro.Fits

open System

// The Rice coder of section 10.4 of the standard, as fpack writes it and cfitsio reads it.
//
// A tile is coded as blocks of BLOCKSIZE values. Each value is the difference from the one before,
// folded so that small negative differences stay small, and then split: the low FS bits go out as
// they are and the high bits as that many zeros followed by a one. FS is chosen per block from the
// average size of the differences in it and written at the head of the block, which is what adapts
// the code to the data.
//
// Two values of FS mean something else: one below the range says every difference in the block is
// zero, and one at the top says the block defeated the coder and its values follow uncoded.

/// Reading a big-endian bit stream, most significant bit first.
type internal BitReader(bytes: byte[], start: int, length: int) =
    let mutable at = start
    let last = start + length
    let mutable bit = 0

    /// True when every bit has been read.
    member _.AtEnd: bool = at >= last

    /// The next bit.
    member this.ReadBit() : int =
        if at >= last then
            0
        else
            let value = (int bytes[at] >>> (7 - bit)) &&& 1

            if bit = 7 then
                bit <- 0
                at <- at + 1
            else
                bit <- bit + 1

            value

    /// The next n bits as a number, most significant first. n is at most 32.
    member this.ReadBits(n: int) : uint32 =
        let mutable value = 0u

        for _ in 1..n do
            value <- (value <<< 1) ||| uint32 (this.ReadBit())

        value

    /// The number of zero bits before the next one bit, which is how a high part is coded.
    member this.ReadZeros() : int =
        let mutable zeros = 0

        while at < last && this.ReadBit() = 0 do
            zeros <- zeros + 1

        zeros

/// Writing a big-endian bit stream, most significant bit first.
type internal BitWriter() =
    let bytes = ResizeArray<byte>()
    let mutable current = 0
    let mutable bit = 0

    /// Writes one bit.
    member _.WriteBit(value: int) =
        current <- current ||| ((value &&& 1) <<< (7 - bit))

        if bit = 7 then
            bytes.Add(byte current)
            current <- 0
            bit <- 0
        else
            bit <- bit + 1

    /// Writes the low n bits of a value, most significant first.
    member this.WriteBits(value: uint32, n: int) =
        for i in n - 1 .. -1 .. 0 do
            this.WriteBit(int ((value >>> i) &&& 1u))

    /// Writes n zero bits followed by a one, which is how a high part is coded.
    member this.WriteZeros(n: int) =
        for _ in 1..n do
            this.WriteBit 0

        this.WriteBit 1

    /// The bytes written, with the last one padded with zeros.
    member _.ToArray() : byte[] =
        if bit > 0 then
            let padded = ResizeArray bytes
            padded.Add(byte current)
            padded.ToArray()
        else
            bytes.ToArray()

/// The Rice coder, RICE_1 in ZCMPTYPE.
[<RequireQualifiedAccess>]
module Rice =
    /// The default BLOCKSIZE, which is what fpack writes unless told otherwise.
    [<Literal>]
    let DefaultBlockSize = 32

    /// Bits the FS of a block is written in, and the value of FS that means the block is uncoded.
    /// Both follow from how wide a value is.
    let private splitBits (bytePix: int) =
        match bytePix with
        | 1 -> 3, 6
        | 2 -> 4, 14
        | 4 -> 5, 25
        | _ -> invalidArg (nameof bytePix) $"{bytePix} is not 1, 2 or 4 bytes per value"

    let inline private mask (bits: int) : uint32 =
        if bits >= 32 then 0xFFFFFFFFu else (1u <<< bits) - 1u

    /// Reads a difference held in `bits` bits as the signed number it stands for. Differences are
    /// only ever taken modulo the width, which is what keeps the fold below a bijection on it.
    let inline private asSigned (bits: int) (value: int) : int =
        match bits with
        | 8 -> int (sbyte value)
        | 16 -> int (int16 value)
        | _ -> value

    /// Folds a signed difference onto the non-negative numbers of the same width, so that -1 is 1
    /// and 1 is 2 and the coder only ever sees small numbers for small differences.
    let inline private fold (bits: int) (difference: int) : uint32 =
        ((uint32 difference <<< 1) ^^^ uint32 (difference >>> 31)) &&& mask bits

    let inline private unfold (value: uint32) : int =
        int (value >>> 1) ^^^ -(int (value &&& 1u))

    /// Narrows a value to the width being coded. One byte is unsigned, as BITPIX 8 is; two and
    /// four bytes are signed.
    let inline private narrow (bytePix: int) (value: int) : int =
        match bytePix with
        | 1 -> int (byte value)
        | 2 -> int (int16 value)
        | _ -> value

    /// Decodes `count` values of `bytePix` bytes each from a Rice stream.
    let decode (bytePix: int) (blockSize: int) (count: int) (bytes: byte[]) : int[] =
        let fsBits, fsMax = splitBits bytePix
        let valueBits = 8 * bytePix
        let values = Array.zeroCreate<int> count

        if count = 0 then
            values
        else
            let reader = BitReader(bytes, 0, bytes.Length)
            // The first value is written as it is, with no difference to take it from.
            let mutable last = narrow bytePix (int (reader.ReadBits valueBits))
            let mutable i = 0

            while i < count do
                let fs = int (reader.ReadBits fsBits) - 1
                let upto = min count (i + blockSize)

                if fs < 0 then
                    // Every difference in the block is zero.
                    while i < upto do
                        values[i] <- last
                        i <- i + 1
                elif fs = fsMax then
                    // The block defeated the coder, so its differences follow uncoded.
                    while i < upto do
                        let difference = unfold (reader.ReadBits valueBits)
                        last <- narrow bytePix (last + difference)
                        values[i] <- last
                        i <- i + 1
                else
                    while i < upto do
                        let high = reader.ReadZeros()
                        let low = reader.ReadBits fs
                        let difference = unfold ((uint32 high <<< fs) ||| low)
                        last <- narrow bytePix (last + difference)
                        values[i] <- last
                        i <- i + 1

            values

    /// The split that codes a block of folded differences in the fewest bits: the position of the
    /// top bit of their mean, which is how many low bits are worth sending as they are.
    let private splitFor (folded: uint32[]) (from: int) (upto: int) : int =
        let count = upto - from
        let mutable total = 0UL

        for i in from .. upto - 1 do
            total <- total + uint64 folded[i]

        // Half the block is subtracted so that the mean rounds the way cfitsio's does.
        let mean =
            if total < uint64 (count / 2 + 1) then
                0UL
            else
                (total - uint64 (count / 2) - 1UL) / uint64 count

        let mutable remaining = mean >>> 1
        let mutable fs = 0

        while remaining > 0UL do
            remaining <- remaining >>> 1
            fs <- fs + 1

        fs

    /// Encodes values of `bytePix` bytes each. The values must already fit that width.
    let encode (bytePix: int) (blockSize: int) (values: int[]) : byte[] =
        let fsBits, fsMax = splitBits bytePix
        let valueBits = 8 * bytePix

        if values.Length = 0 then
            [||]
        else
            let writer = BitWriter()
            writer.WriteBits(uint32 values[0] &&& mask valueBits, valueBits)

            let mutable last = narrow bytePix values[0]
            let mutable i = 0

            while i < values.Length do
                let upto = min values.Length (i + blockSize)

                let folded = [|
                    let mutable previous = last

                    for j in i .. upto - 1 do
                        let value = narrow bytePix values[j]
                        yield fold valueBits (asSigned valueBits (value - previous))
                        previous <- value
                |]

                let fs = splitFor folded 0 folded.Length

                if folded |> Array.forall (fun f -> f = 0u) then
                    writer.WriteBits(0u, fsBits)
                elif fs >= fsMax then
                    writer.WriteBits(uint32 (fsMax + 1), fsBits)

                    for f in folded do
                        writer.WriteBits(f, valueBits)
                else
                    writer.WriteBits(uint32 (fs + 1), fsBits)

                    for f in folded do
                        writer.WriteZeros(int (f >>> fs))
                        writer.WriteBits(f, fs)

                last <- narrow bytePix values[upto - 1]
                i <- upto

            writer.ToArray()
