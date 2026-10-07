module ChecksumTests

open System
open Xunit
open FsCheck
open FsCheck.FSharp
open FSharp.Astro.Fits
open Generators

[<Fact>]
let ``encodes zero as sixteen zeros`` () =
    Assert.Equal(Checksum.Placeholder, Checksum.encode 0u)

[<Fact>]
let ``encoding avoids punctuation and decodes back`` () =
    check (
        forAll
            (Gen.map2
                (fun hi lo -> (uint32 hi <<< 16) ||| uint32 (uint16 lo))
                (Gen.choose (0, 0xFFFF))
                (Gen.choose (0, 0xFFFF)))
            (fun value ->
                let ascii = Checksum.encode value
                ascii.Length = 16
                && ascii
                   |> String.forall (fun c ->
                       c >= '0' && c <= 'z' && not (c >= ':' && c <= '@') && not (c >= '[' && c <= '`')
                   )
                && Checksum.decode ascii = value
            )
    )

[<Fact>]
let ``sum carries end around`` () =
    Assert.Equal(0xFFFFFFFFu, Checksum.sum (ReadOnlySpan [| 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]))
    Assert.Equal(1u, Checksum.sum (ReadOnlySpan [| 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0uy; 0uy; 0uy; 1uy |]))
    Assert.Equal(0x01020304u, Checksum.sum (ReadOnlySpan [| 1uy; 2uy; 3uy; 4uy |]))

[<Fact>]
let ``complement makes the total negative zero`` () =
    check (
        forAll
            (Gen.arrayOfLength 2880 (Gen.choose (0, 255) |> Gen.map byte))
            (fun data ->
                let partial = Checksum.sum (ReadOnlySpan data)
                let complement = ~~~partial
                Checksum.verify (Checksum.add partial complement)
            )
    )
