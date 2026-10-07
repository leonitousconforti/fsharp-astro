namespace FSharp.Astro.Fits

open System
open System.IO
open Microsoft.Win32.SafeHandles

/// Random access to the bytes of a FITS file. Reads are by absolute offset so the same shape fits
/// files, memory, memory-mapped regions and remote range requests.
type IFitsSource =
    inherit IDisposable

    /// Total length in bytes.
    abstract Length: int64

    /// Reads up to buffer.Length bytes starting at offset, returning how many were read. Zero means
    /// the offset is at or past the end.
    abstract ReadAt: offset: int64 * buffer: Span<byte> -> int

/// Sources over files, memory and streams.
[<RequireQualifiedAccess>]
module Source =
    /// Fills the buffer from the given offset, stopping early only at the end of the source.
    /// Returns the number of bytes read.
    let readExact (source: IFitsSource) (offset: int64) (buffer: Span<byte>) : int =
        let mutable total = 0
        let mutable eof = false

        while not eof && total < buffer.Length do
            let n = source.ReadAt(offset + int64 total, buffer.Slice total)

            if n <= 0 then eof <- true else total <- total + n

        total

    /// Reads exactly the given number of bytes into a new array, throwing at end of source.
    let readBytes (source: IFitsSource) (offset: int64) (length: int) : byte[] =
        let buffer = Array.zeroCreate<byte> length
        let n = readExact source offset (Span buffer)

        if n < length then
            raise (EndOfStreamException $"wanted {length} bytes at offset {offset} but the source ended after {n}")

        buffer

    type private FileSource(handle: SafeFileHandle) =
        let length = RandomAccess.GetLength handle

        interface IFitsSource with
            member _.Length = length
            member _.ReadAt(offset, buffer) =
                RandomAccess.Read(handle, buffer, offset)
            member _.Dispose() = handle.Dispose()

    type private MemorySource(memory: ReadOnlyMemory<byte>) =
        interface IFitsSource with
            member _.Length = int64 memory.Length

            member _.ReadAt(offset, buffer) =
                if offset >= int64 memory.Length then
                    0
                else
                    let available = memory.Length - int offset
                    let n = min available buffer.Length
                    memory.Span.Slice(int offset, n).CopyTo buffer
                    n

            member _.Dispose() = ()

    type private StreamSource(stream: Stream, leaveOpen: bool) =
        interface IFitsSource with
            member _.Length = stream.Length

            member _.ReadAt(offset, buffer) =
                if offset >= stream.Length then
                    0
                else
                    stream.Position <- offset
                    stream.Read buffer

            member _.Dispose() =
                if not leaveOpen then
                    stream.Dispose()

    /// Opens a file for random access reads.
    let ofFile (path: string) : IFitsSource =
        let handle = File.OpenHandle(path, FileMode.Open, FileAccess.Read, FileShare.Read)
        new FileSource(handle) :> IFitsSource

    /// Wraps bytes already in memory.
    let ofMemory (memory: ReadOnlyMemory<byte>) : IFitsSource = new MemorySource(memory) :> IFitsSource

    /// Wraps a byte array.
    let ofBytes (bytes: byte[]) : IFitsSource = ofMemory (ReadOnlyMemory bytes)

    /// Wraps a seekable stream. The stream is disposed with the source unless leaveOpen is set.
    let ofStream (stream: Stream) (leaveOpen: bool) : IFitsSource =
        if not stream.CanSeek then
            invalidArg (nameof stream) "stream must be seekable"

        new StreamSource(stream, leaveOpen) :> IFitsSource
