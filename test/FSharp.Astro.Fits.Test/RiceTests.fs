module RiceTests

open System
open Xunit
open FsCheck
open FsCheck.FSharp
open FSharp.Astro.Fits
open Generators

let private roundTrips (bytePix: int) (blockSize: int) (values: int[]) =
    let encoded = Rice.encode bytePix blockSize values
    let decoded = Rice.decode bytePix blockSize values.Length encoded
    decoded = values

[<Fact>]
let ``a run of equal values costs almost nothing`` () =
    let values = Array.create 1024 7
    let encoded = Rice.encode 4 Rice.DefaultBlockSize values
    Assert.Equal<int[]>(values, Rice.decode 4 Rice.DefaultBlockSize 1024 encoded)
    // Four bytes for the first value and five bits for each of the 32 blocks.
    Assert.True(encoded.Length < 32, $"a flat tile took {encoded.Length} bytes")

[<Fact>]
let ``a smooth ramp codes in far less than the raw size`` () =
    let values = Array.init 4096 id
    let encoded = Rice.encode 4 Rice.DefaultBlockSize values
    Assert.Equal<int[]>(values, Rice.decode 4 Rice.DefaultBlockSize 4096 encoded)
    Assert.True(encoded.Length < 4096, $"a ramp took {encoded.Length} bytes against 16384 raw")

[<Fact>]
let ``noise that defeats the coder still comes back`` () =
    // Alternating extremes make every difference as large as the width allows, which is the case
    // that falls through to the uncoded block.
    let values =
        Array.init 300 (fun i -> if i % 2 = 0 then Int32.MinValue else Int32.MaxValue)
    Assert.True(roundTrips 4 Rice.DefaultBlockSize values)

[<Fact>]
let ``the edges of each width survive`` () =
    Assert.True(roundTrips 1 Rice.DefaultBlockSize [| 0; 255; 0; 128; 127; 1 |])
    Assert.True(roundTrips 2 Rice.DefaultBlockSize [| 0; 32767; -32768; -1; 1 |])
    Assert.True(roundTrips 4 Rice.DefaultBlockSize [| 0; Int32.MaxValue; Int32.MinValue; -1; 1 |])

[<Fact>]
let ``an empty tile encodes to nothing`` () =
    Assert.Equal<byte[]>([||], Rice.encode 4 Rice.DefaultBlockSize [||])
    Assert.Equal<int[]>([||], Rice.decode 4 Rice.DefaultBlockSize 0 [||])

[<Fact>]
let ``a block size the values do not divide by still works`` () =
    for blockSize in [ 1; 2; 7; 16; 32; 64 ] do
        let values = Array.init 100 (fun i -> i * i % 1000)
        Assert.True(roundTrips 4 blockSize values, $"block size {blockSize}")

[<Fact>]
let ``only one, two and four byte values are coded`` () =
    Assert.Throws<ArgumentException>(fun () -> Rice.encode 3 32 [| 1 |] |> ignore)
    |> ignore
    Assert.Throws<ArgumentException>(fun () -> Rice.decode 8 32 1 [| 1uy |] |> ignore)
    |> ignore

[<Fact>]
let ``any four byte tile comes back as it went in`` () =
    let tile = gen {
        let! n = Gen.choose (1, 400)
        // A mixture of smooth and jagged, which is what a real tile is.
        let! values =
            Gen.arrayOfLength
                n
                (Gen.frequency [ 4, Gen.choose (-50, 50); 1, Gen.choose (Int32.MinValue, Int32.MaxValue) ])

        return values
    }

    check (forAll tile (roundTrips 4 Rice.DefaultBlockSize))

[<Fact>]
let ``any two byte tile comes back as it went in`` () =
    let tile = gen {
        let! n = Gen.choose (1, 400)
        let! values =
            Gen.arrayOfLength n (Gen.choose (int Int16.MinValue, int Int16.MaxValue))
        return values
    }

    check (forAll tile (roundTrips 2 Rice.DefaultBlockSize))

[<Fact>]
let ``any one byte tile comes back as it went in`` () =
    let tile = gen {
        let! n = Gen.choose (1, 400)
        let! values = Gen.arrayOfLength n (Gen.choose (0, 255))
        return values
    }

    check (forAll tile (roundTrips 1 Rice.DefaultBlockSize))
