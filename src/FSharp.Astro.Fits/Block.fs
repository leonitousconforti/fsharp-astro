namespace FSharp.Astro.Fits

/// Geometry of FITS blocks and header records.
[<RequireQualifiedAccess>]
module Block =
    /// Size in bytes of a FITS block. Every header and data unit is padded to a
    /// multiple of this.
    [<Literal>]
    let Size = 2880

    /// Size in bytes of one header record (a card image).
    [<Literal>]
    let CardSize = 80

    /// Number of card images in one block.
    [<Literal>]
    let CardsPerBlock = 36

    /// Rounds a byte count up to the next multiple of the block size.
    let padded (length: int64) : int64 =
        if length <= 0L then
            0L
        else
            (length + int64 Size - 1L) / int64 Size * int64 Size

    /// Number of padding bytes needed to reach the next block boundary.
    let padding (length: int64) : int64 = padded length - length
