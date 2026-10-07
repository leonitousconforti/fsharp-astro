module ExtinctionTests

open Xunit
open FSharp.Astro.Units
open Helpers

let private micrometres (x: float) : float<m> =
    x * 1.0<um> * Length.metersPerMicrometer

[<Fact>]
let ``a magnitude of extinction removes three fifths of the flux`` () =
    close 1e-12 0.3981071705534972 (Extinction.transmission 1.0<mag>)
    close 1e-15 1.0 (Extinction.transmission 0.0<mag>)

[<Fact>]
let ``an optical depth of one is 1.086 magnitudes`` () =
    close 1e-3 1.0857<mag> (Extinction.ofOpticalDepth 1.0)
    close 1e-12 1.0 (Extinction.opticalDepth (Extinction.ofOpticalDepth 1.0))
    close 1e-12 (exp -1.0) (Extinction.transmission (Extinction.ofOpticalDepth 1.0))

[<Fact>]
let ``the curve is normalised to one in V`` () =
    // Cardelli normalises at 1/lambda of exactly 1.82 per micrometre, where a and b are 1 and 0,
    // so the result is one for every R_V.
    for rv in [ 2.0; 3.1; 5.5 ] do
        close 1e-15 1.0 (Extinction.relativeToV Cardelli1989 rv (micrometres (1.0 / 1.82)))

[<Fact>]
let ``the curve rises to the blue and falls to the red`` () =
    let at x =
        Extinction.relativeToV Cardelli1989 Extinction.rvDiffuse (micrometres x)

    close 1e-12 1.5546344478310812 (at 0.366) // U
    close 1e-12 1.331873246772909 (at 0.438) // B
    close 1e-12 1.0094589388755892 (at 0.545) // V
    close 1e-12 0.8413992716112605 (at 0.641) // R
    close 1e-12 0.600366878609617 (at 0.798) // I
    close 1e-12 0.28760574926358984 (at 1.235) // J
    close 1e-12 0.17830578090680688 (at 1.662) // H
    close 1e-12 0.11701313389302864 (at 2.159) // Ks

[<Fact>]
let ``the curve decreases monotonically from the ultraviolet to the infrared`` () =
    // Between the 2175 angstrom bump and the far ultraviolet rise the curve is not monotonic, so
    // this covers the optical and infrared, 0.35 micrometres and longer.
    let samples =
        [ 350..10..3000 ]
        |> List.map (fun nm ->
            Extinction.relativeToV Cardelli1989 Extinction.rvDiffuse (float nm * 1.0<nm> * Length.metersPerNanometer)
        )

    Assert.Equal<float list>(List.sortDescending samples, samples)

[<Fact>]
let ``the curve has the 2175 angstrom bump`` () =
    let at nm =
        Extinction.relativeToV Cardelli1989 Extinction.rvDiffuse (float nm * 1.0<nm> * Length.metersPerNanometer)

    // The bump peaks near 217.5 nm and stands above both shoulders.
    Assert.True(at 217.5 > at 195.0)
    Assert.True(at 217.5 > at 250.0)

[<Fact>]
let ``the curve recovers the ratio of total to selective extinction it was given`` () =
    // R_V = A_V / (A_B - A_V) is the definition, and the curve is a fit, so it comes back to
    // about a percent rather than exactly.
    let av = Extinction.relativeToV Cardelli1989 3.1 (micrometres 0.55)
    let ab = Extinction.relativeToV Cardelli1989 3.1 (micrometres 0.44)
    close 2e-2 3.1 (av / (ab - av))

[<Fact>]
let ``a larger ratio of total to selective extinction makes a flatter curve`` () =
    let at rv =
        Extinction.relativeToV Cardelli1989 rv (micrometres 0.36)

    // More of the extinction is grey, so the ultraviolet stands less far above V.
    Assert.True(at 5.5 < at 3.1)
    Assert.True(at 3.1 < at 2.0)

[<Fact>]
let ``every standard band falls inside the curve's range`` () =
    for band in Photometry.Bands.all @ Photometry.Bands.sdssAll do
        Assert.True(
            Option.isSome (Extinction.tryRelativeToV Cardelli1989 3.1 band.EffectiveWavelength),
            $"{band.Name} is outside the curve"
        )

[<Fact>]
let ``the curve refuses a wavelength it does not cover`` () =
    Assert.True(Option.isNone (Extinction.tryRelativeToV Cardelli1989 3.1 (micrometres 0.05)))
    Assert.True(Option.isNone (Extinction.tryRelativeToV Cardelli1989 3.1 (micrometres 5.0)))
    Assert.Throws<System.ArgumentException>(fun () ->
        Extinction.relativeToV Cardelli1989 3.1 (micrometres 5.0) |> ignore
    )
    |> ignore

[<Fact>]
let ``the curve is continuous across the joins between its pieces`` () =
    // The four pieces meet at 1.1, 3.3 and 8 per micrometre. A step there would show up as a
    // discontinuity in the dereddened magnitude of anything observed near the join.
    let at x =
        Extinction.relativeToV Cardelli1989 3.1 (micrometres (1.0 / x))

    for join in [ 1.1; 3.3; 8.0 ] do
        let below = at (join - 1e-9)
        let above = at (join + 1e-9)
        Assert.True(abs (below - above) < 1e-3, $"discontinuity of {abs (below - above)} at {join}")
