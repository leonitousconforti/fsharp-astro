module GroupTests

open System
open Xunit
open FSharp.Astro.Fits
open Generators

/// Three groups of two parameters each over a 2 x 3 array, the shape a UV data set has in
/// miniature: the parameters carry the baseline, the array carries the visibility.
let private visibilities = [
    for g in 0..2 ->
        {
            Parameters = ImageData.Float32 [| float32 g; float32 (10 * g) |]
            Data = ImageData.Float32(Array.init 6 (fun i -> float32 (100 * g + i)))
        }
]

let private parameters = [
    Groups.parameterInfo "UU"
    Groups.parameterInfo "VV" |> Groups.withScaling 5.0 2.0
]

let private file () =
    let bytes =
        Fits.toBytes [ Fits.randomGroups Header.empty [| 2; 3 |] parameters visibilities ]

    Fits.openBytes bytes

[<Fact>]
let ``a random groups HDU is written with the layout the standard asks for`` () =
    use file = file ()
    let info = file[0]
    Assert.Equal(HduKind.RandomGroups, info.Kind)
    // NAXIS1 is the zero that marks the layout; the array axes follow it in FITS order.
    Assert.Equal<int64[]>([| 0L; 3L; 2L |], info.Axes)
    Assert.Equal(2L, info.PCount)
    Assert.Equal(3L, info.GCount)
    Assert.Equal(BitPix.Float32, info.BitPix)
    // Three groups of two parameters and six elements, four bytes each.
    Assert.Equal(96L, info.DataLength)
    Assert.Equal(Some true, Header.tryBool "GROUPS" info.Header)
    Assert.Equal(Some "UU", Header.tryString "PTYPE1" info.Header)
    Assert.Equal(Some 2.0, Header.tryFloat "PSCAL2" info.Header)
    Assert.Equal(Some 5.0, Header.tryFloat "PZERO2" info.Header)

[<Fact>]
let ``groups read back with their parameters scaled`` () =
    use file = file ()
    let groups = Fits.readGroups file 0
    Assert.Equal(3, groups.Count)
    Assert.Equal(2, groups.Parameters.Length)
    Assert.Equal<int[]>([| 2; 3 |], groups.Shape)
    Assert.Equal(6, groups.GroupLength)
    Assert.Equal(32, groups.GroupSize)

    // PZERO + PSCAL * stored, so UU is untouched and VV is 5 + 2 * stored.
    Assert.Equal<float[]>([| 0.0; 5.0 |], Groups.parameters 0 groups)
    Assert.Equal<float[]>([| 1.0; 25.0 |], Groups.parameters 1 groups)
    Assert.Equal<float[]>([| 2.0; 45.0 |], Groups.parameters 2 groups)
    Assert.Equal<float[]>([| 0.0; 1.0; 2.0 |], Groups.parameterValues 1 groups)
    Assert.Equal<float[]>([| 5.0; 25.0; 45.0 |], Groups.parameterValues 2 groups)

[<Fact>]
let ``each group's array comes back as an image`` () =
    use file = file ()
    let groups = Fits.readGroups file 0

    for g in 0..2 do
        let image = Groups.image g groups
        Assert.Equal<int[]>([| 2; 3 |], image.Shape)
        Assert.Equal<float[]>(Array.init 6 (fun i -> float (100 * g + i)), Image.toFloat64 image)

    match (Groups.image 1 groups).Data with
    | ImageData.Float32 values -> Assert.Equal<float32[]>(Array.init 6 (fun i -> float32 (100 + i)), values)
    | other -> failwith $"expected singles but got %A{other}"

[<Fact>]
let ``array scaling applies to every group`` () =
    let header =
        Header.empty
        |> Header.set "BZERO" (CardValue.Real 1000.0)
        |> Header.set "BSCALE" (CardValue.Real 0.5)

    let groups = [
        {
            Parameters = ImageData.Int16 [| 7s |]
            Data = ImageData.Int16 [| 2s; 4s |]
        }
    ]

    let bytes =
        Fits.toBytes [ Fits.randomGroups header [| 2 |] [ Groups.parameterInfo "P" ] groups ]

    use file = Fits.openBytes bytes
    let read = Fits.readGroups file 0
    Assert.Equal(1000.0, read.BZero)
    Assert.Equal(0.5, read.BScale)
    Assert.Equal<float[]>([| 1001.0; 1002.0 |], Image.toFloat64 (Groups.image 0 read))
    // The parameters have their own scaling, so BZERO does not touch them.
    Assert.Equal<float[]>([| 7.0 |], Groups.parameters 0 read)

[<Fact>]
let ``parameters that share a name are summed`` () =
    // Splitting one value across two parameters is how the layout carries more precision than the
    // stored type has.
    let groups = [
        {
            Parameters = ImageData.Float32 [| 2400000.5f; 0.25f |]
            Data = ImageData.Float32 [| 1.0f |]
        }
    ]

    let names = [ Groups.parameterInfo "DATE"; Groups.parameterInfo "DATE" ]

    let bytes = Fits.toBytes [ Fits.randomGroups Header.empty [| 1 |] names groups ]
    use file = Fits.openBytes bytes
    let read = Fits.readGroups file 0
    Assert.Equal<int[]>([| 1; 2 |], Groups.numbersOf "DATE" read)
    Assert.Equal<float[]>([| 2400000.75 |], (Groups.tryParameterValues "date" read).Value)
    Assert.Equal(None, Groups.tryParameterValues "MISSING" read)

[<Fact>]
let ``groups with no parameters are still groups`` () =
    let groups = [
        {
            Parameters = ImageData.Float64 [||]
            Data = ImageData.Float64 [| 1.0; 2.0 |]
        }
    ]

    let bytes = Fits.toBytes [ Fits.randomGroups Header.empty [| 2 |] [] groups ]
    use file = Fits.openBytes bytes
    let read = Fits.readGroups file 0
    Assert.Equal(0L, file[0].PCount)
    Assert.Empty read.Parameters
    Assert.Equal<float[]>([||], Groups.parameters 0 read)
    Assert.Equal<float[]>([| 1.0; 2.0 |], Image.toFloat64 (Groups.image 0 read))

[<Fact>]
let ``a group of the wrong shape or type is refused`` () =
    let good = {
        Parameters = ImageData.Float32 [| 1.0f |]
        Data = ImageData.Float32 [| 1.0f; 2.0f |]
    }

    // An array that does not match the shape.
    Assert.Throws<ArgumentException>(fun () ->
        Fits.randomGroups Header.empty [| 3 |] [ Groups.parameterInfo "P" ] [ good ]
        |> ignore
    )
    |> ignore

    // A parameter count that does not match the declared parameters.
    Assert.Throws<ArgumentException>(fun () -> Fits.randomGroups Header.empty [| 2 |] [] [ good ] |> ignore)
    |> ignore

    // Groups stored as more than one type.
    let other = {
        Parameters = ImageData.Int16 [| 1s |]
        Data = ImageData.Int16 [| 1s; 2s |]
    }

    Assert.Throws<ArgumentException>(fun () ->
        Fits.randomGroups Header.empty [| 2 |] [ Groups.parameterInfo "P" ] [ good; other ]
        |> ignore
    )
    |> ignore

[<Fact>]
let ``reading the wrong kind of HDU as groups fails`` () =
    let bytes =
        Fits.toBytes [
            Fits.primary Header.empty (Image.create [| 2 |] (ImageData.Float32 [| 1.0f; 2.0f |]))
        ]

    use file = Fits.openBytes bytes
    Assert.True((Fits.tryReadGroups file 0).IsError)
    Assert.Throws<FitsException>(fun () -> Fits.readGroups file 0 |> ignore)
    |> ignore

[<Fact>]
let ``asking for a group outside the file is refused`` () =
    use file = file ()
    let groups = Fits.readGroups file 0
    Assert.Throws<ArgumentException>(fun () -> Groups.image 3 groups |> ignore)
    |> ignore
    Assert.Throws<ArgumentException>(fun () -> Groups.parameters -1 groups |> ignore)
    |> ignore
    Assert.Throws<ArgumentException>(fun () -> Groups.parameterValues 3 groups |> ignore)
    |> ignore
