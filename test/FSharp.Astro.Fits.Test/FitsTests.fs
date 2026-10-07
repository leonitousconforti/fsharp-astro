module FitsTests

open System
open System.IO
open System.Text
open Xunit
open FsCheck
open FsCheck.FSharp
open FSharp.Astro.Fits
open Generators

let violations (error: FitsError) : Violation list =
    match error with
    | Violations issues -> issues |> List.map (fun i -> i.Violation)
    | IoError e -> failwith $"unexpected I/O error {e}"

let sample = Image.create [| 2; 3 |] (ImageData.Int16 [| 1s; 2s; 3s; 4s; 5s; 6s |])

[<Fact>]
let ``writes a primary image that reads back`` () =
    let header =
        Header.empty
        |> Header.setWithComment "OBJECT" (CardValue.String "M31") "target"
        |> Header.addComment "made by fitsharp"
    let bytes = Fits.toBytes [ Fits.primary header sample ]
    Assert.Equal(2 * Block.Size, bytes.Length)
    use file = Fits.openBytes bytes
    Assert.Equal(1, file.Count)
    Assert.Empty file.Warnings
    let hdu = file.Primary
    Assert.Equal(HduKind.Primary, hdu.Kind)
    Assert.Equal<int64[]>([| 3L; 2L |], hdu.Axes)
    Assert.Equal(2880L, hdu.DataOffset)
    Assert.Equal(12L, hdu.DataLength)
    Assert.Equal(Some "M31", Header.tryString "OBJECT" hdu.Header)
    Assert.Equal<string list>([ "made by fitsharp" ], Header.comments hdu.Header)
    Assert.Equal<string list>(
        [ "SIMPLE"; "BITPIX"; "NAXIS"; "NAXIS1"; "NAXIS2"; "EXTEND"; "OBJECT" ],
        Header.keywords hdu.Header
    )
    Assert.Equal(sample, Fits.readImage file 0)

[<Fact>]
let ``user copies of mandatory keywords are replaced`` () =
    let header =
        Header.ofCards [
            Card.Value("NAXIS", CardValue.Integer 99L, None)
            Card.Value("BITPIX", CardValue.Integer 8L, None)
        ]
    use file = Fits.openBytes (Fits.toBytes [ Fits.primary header sample ])
    Assert.Equal<string list>(
        [ "SIMPLE"; "BITPIX"; "NAXIS"; "NAXIS1"; "NAXIS2"; "EXTEND" ],
        Header.keywords file.Primary.Header
    )

[<Fact>]
let ``writes extensions with names`` () =
    let ext = Header.empty |> Header.set "EXTNAME" (CardValue.String "SCI")
    let bytes =
        Fits.toBytes [
            Fits.emptyPrimary Header.empty
            Fits.imageExtension ext sample
            Fits.imageExtension (Header.set "EXTNAME" (CardValue.String "ERR") Header.empty) sample
        ]
    use file = Fits.openBytes bytes
    Assert.Equal(3, file.Count)
    Assert.Equal(0L, file.Primary.DataLength)
    Assert.Equal(HduKind.Image, file[1].Kind)
    Assert.Equal(Some "SCI", file[1].Name)
    Assert.Equal(Some 2, Fits.tryFindByName "err" file |> Option.map (fun h -> h.Index))
    Assert.Equal(sample, Fits.readImage file 2)
    Assert.Equal(int64 (2 * Block.Size), file[1].DataOffset)

[<Fact>]
let ``scaling keywords are written and read`` () =
    let image = Image.ofUInt16 [| 2 |] [| 0us; 65535us |]
    use file = Fits.openBytes (Fits.toBytes [ Fits.primary Header.empty image ])
    let read = Fits.readImage file 0
    Assert.True(Image.isUInt16 read)
    Assert.Equal<uint16[]>([| 0us; 65535us |], Image.toUInt16 read)

[<Fact>]
let ``copies an HDU byte for byte`` () =
    let bytes =
        Fits.toBytes [
            Fits.primary (Header.set "OBJECT" (CardValue.String "x") Header.empty) sample
        ]
    use file = Fits.openBytes bytes
    let copied = Fits.toBytes [ Fits.copy file 0 ]
    Assert.Equal<byte[]>(bytes, copied)

[<Fact>]
let ``rejects a data length that does not match the header`` () =
    let hdu = {
        Fits.primary Header.empty sample with
            Data = HduData.Bytes [| 1uy |]
    }
    let error = Fits.tryToBytes [ hdu ] |> expectError
    Assert.Equal<Violation list>([ Violation.DataLengthMismatch(12L, 1L) ], violations error)

[<Fact>]
let ``rejects a file that does not start with SIMPLE`` () =
    let error =
        Fits.tryOpenBytes (Encoding.ASCII.GetBytes(String(' ', 2880))) |> expectError
    Assert.Equal<Violation list>([ Violation.NotAFitsFile "first keyword is not SIMPLE" ], violations error)

[<Fact>]
let ``rejects an empty file`` () =
    let error = Fits.tryOpenBytes [||] |> expectError
    Assert.Equal<Violation list>([ Violation.NotAFitsFile "only 0 bytes, less than one block" ], violations error)

[<Fact>]
let ``rejects a truncated header`` () =
    let bytes = Fits.toBytes [ Fits.primary Header.empty sample ]
    let error = Fits.tryOpenBytes (Array.sub bytes 0 100) |> expectError
    Assert.Equal<Violation list>([ Violation.NotAFitsFile "only 100 bytes, less than one block" ], violations error)

[<Fact>]
let ``rejects truncated data`` () =
    let bytes = Fits.toBytes [ Fits.primary Header.empty sample ]
    let error =
        Fits.tryOpenBytesWith FitsOptions.Lenient (Array.sub bytes 0 (2880 + 6))
        |> expectError
    Assert.Equal<Violation list>([ Violation.TruncatedData(12L, 6L) ], violations error)

[<Fact>]
let ``strict rejects missing data padding but lenient warns`` () =
    let bytes = Fits.toBytes [ Fits.primary Header.empty sample ]
    let unpadded = Array.sub bytes 0 (2880 + 12)
    let error = Fits.tryOpenBytes unpadded |> expectError
    Assert.Equal<Violation list>([ Violation.UnpaddedData 2868L ], violations error)
    use file = Fits.openBytesWith FitsOptions.Lenient unpadded
    Assert.Equal<Violation list>([ Violation.UnpaddedData 2868L ], file.Warnings |> List.map (fun w -> w.Violation))
    Assert.Equal(sample, Fits.readImage file 0)

[<Fact>]
let ``strict rejects trailing bytes but lenient warns`` () =
    let bytes =
        Array.append (Fits.toBytes [ Fits.primary Header.empty sample ]) [| 0uy; 0uy |]
    let error = Fits.tryOpenBytes bytes |> expectError
    Assert.Equal<Violation list>([ Violation.TrailingBytes 2L ], violations error)
    use file = Fits.openBytesWith FitsOptions.Lenient bytes
    Assert.Equal(1, file.Count)

[<Fact>]
let ``strict rejects a lowercase keyword and says where`` () =
    let bytes = Fits.toBytes [ Fits.primary Header.empty sample ]
    let patched = Array.copy bytes
    Encoding.ASCII.GetBytes("extra   = 1".PadRight 80).CopyTo(patched, 5 * 80)
    let error = Fits.tryOpenBytes patched |> expectError

    match error with
    | Violations [ issue ] ->
        Assert.Equal(Violation.InvalidKeyword "extra", issue.Violation)
        Assert.Equal(Some 0, issue.Hdu)
        Assert.Equal(Some 5, issue.Card)
        Assert.Equal(Some 400L, issue.Offset)
        Assert.Contains("HDU 0, card 5, offset 400: invalid keyword 'extra'", FitsError.describe error)
    | other -> failwith $"unexpected %A{other}"

    use file = Fits.openBytesWith FitsOptions.Lenient patched
    Assert.Equal(Some 1L, Header.tryInt "extra" file.Primary.Header)

[<Fact>]
let ``the throwing API wraps the error`` () =
    let e = Assert.Throws<FitsException>(fun () -> Fits.openBytes [||] |> ignore)
    Assert.Equal<Violation list>([ Violation.NotAFitsFile "only 0 bytes, less than one block" ], violations e.Error)

[<Fact>]
let ``reads and writes through the file system`` () =
    let path = Path.Combine(Path.GetTempPath(), $"fitsharp-{Guid.NewGuid()}.fits")

    try
        Fits.writeFile path [ Fits.primary Header.empty sample ]
        use file = Fits.openFile path
        Assert.Equal(sample, Fits.readImage file 0)
    finally
        File.Delete path

[<Fact>]
let ``opens seekable and non-seekable streams`` () =
    let bytes = Fits.toBytes [ Fits.primary Header.empty sample ]
    use seekable = new MemoryStream(bytes)
    use file = Fits.openStream seekable
    Assert.Equal(sample, Fits.readImage file 0)
    use nonSeekable = new NonSeekableStream(bytes)
    use buffered = Fits.openStream nonSeekable
    Assert.Equal(sample, Fits.readImage buffered 0)

[<Fact>]
let ``a stream is disposed with the file unless left open`` () =
    let bytes = Fits.toBytes [ Fits.primary Header.empty sample ]
    let owned = new MemoryStream(bytes)
    (Fits.openStream owned :> IDisposable).Dispose()
    Assert.False owned.CanRead
    let kept = new MemoryStream(bytes)
    (Fits.openStreamWith FitsOptions.Default true kept :> IDisposable).Dispose()
    Assert.True kept.CanRead
    let unseekable = new NonSeekableStream(bytes)
    (Fits.openStream unseekable :> IDisposable).Dispose()
    Assert.False unseekable.CanRead

[<Fact>]
let ``a missing file is an I/O error`` () =
    match Fits.tryOpenFile "/nonexistent/file.fits" with
    | Error(IoError(:? FileNotFoundException)) -> ()
    | Error(IoError(:? DirectoryNotFoundException)) -> ()
    | other -> failwith $"unexpected %A{other}"

[<Fact>]
let ``checksums verify and detect corruption`` () =
    let hdu =
        Fits.withChecksum (Fits.primary (Header.set "OBJECT" (CardValue.String "x") Header.empty) sample)
    Assert.Equal(Some "DATASUM", Header.keywords hdu.Header |> List.tryFind (fun k -> k = "DATASUM"))
    let bytes = Fits.toBytes [ hdu ]
    use file = Fits.openBytes bytes
    Assert.Equal(Fits.Valid, Fits.verifyChecksum file 0)

    let corruptData = Array.copy bytes
    corruptData[2880] <- corruptData[2880] ^^^ 1uy
    use corrupted = Fits.openBytes corruptData
    Assert.Equal(Fits.DataMismatch, Fits.verifyChecksum corrupted 0)

    let corruptHeader = Array.copy bytes
    corruptHeader[6 * 80 + 11] <- byte 'y'
    use corrupted = Fits.openBytes corruptHeader
    Assert.Equal(Fits.HduMismatch, Fits.verifyChecksum corrupted 0)

    use plain = Fits.openBytes (Fits.toBytes [ Fits.primary Header.empty sample ])
    Assert.Equal(Fits.NoChecksum, Fits.verifyChecksum plain 0)

[<Fact>]
let ``empty data units have DATASUM zero`` () =
    let hdu = Fits.withChecksum (Fits.emptyPrimary Header.empty)
    Assert.Equal(Some "0", Header.tryString "DATASUM" hdu.Header)
    use file = Fits.openBytes (Fits.toBytes [ hdu ])
    Assert.Equal(Fits.Valid, Fits.verifyChecksum file 0)

[<Fact>]
let ``images of every type and shape survive a file round trip`` () =
    let gen = gen {
        let! primary = image
        let! extension = image
        let! cards = cards
        return primary, extension, cards
    }

    check (
        forAll
            gen
            (fun (primary, extension, cards) ->
                let header = Header.ofCards cards

                let generated = HduLayout.mandatoryKeywords 999 @ [ "BZERO"; "BSCALE"; "BLANK" ]

                match Fits.tryToBytes [ Fits.primary header primary; Fits.imageExtension header extension ] with
                | Error(Violations issues) when issues |> List.forall (fun i -> unrepresentable i.Violation) -> true
                | Error e -> failwith $"write failed: %A{e}"
                | Ok bytes ->
                    match Fits.tryOpenBytes bytes with
                    | Error e -> failwith $"open failed: %A{e}"
                    | Ok file ->
                        use file = file
                        let expectedUser = Header.removeAll generated header

                        let userCards (index: int) =
                            Header.removeAll generated file[index].Header

                        if Fits.readImage file 0 <> primary then
                            failwith $"primary image changed: %A{Fits.readImage file 0}"

                        if Fits.readImage file 1 <> extension then
                            failwith $"extension image changed: %A{Fits.readImage file 1}"

                        if userCards 0 <> expectedUser then
                            failwith (firstMismatch expectedUser.Cards (userCards 0).Cards)

                        if userCards 1 <> expectedUser then
                            failwith (firstMismatch expectedUser.Cards (userCards 1).Cards)

                        bytes.Length % Block.Size = 0 && file.Count = 2
            )
    )
