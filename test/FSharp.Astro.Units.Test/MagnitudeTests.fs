module MagnitudeTests

open Xunit
open FSharp.Astro.Units
open Helpers

[<Fact>]
let ``five magnitudes are a factor of one hundred`` () =
    close 1e-12 0.01 (Magnitude.fluxRatio 5.0<mag>)
    close 1e-12 5.0<mag> (Magnitude.ofFluxRatio 1.0<Jy> 100.0<Jy>)

[<Fact>]
let ``brighter is smaller`` () =
    Assert.True(Magnitude.ofFluxRatio 2.0<Jy> 1.0<Jy> < 0.0<mag>)

[<Fact>]
let ``the AB zero point is 3631 janskys`` () =
    Assert.Equal(0.0<mag>, Magnitude.ab 3631.0<Jy>)
    close 1e-12 7.5<mag> (Magnitude.ab 3.631<Jy>)

[<Fact>]
let ``the distance modulus is zero at ten parsecs`` () =
    Assert.Equal(0.0<mag>, Magnitude.distanceModulus 10.0<pc>)
    close 1e-12 5.0<mag> (Magnitude.distanceModulus 100.0<pc>)
    close
        1e-12
        25.0<mag>
        (Magnitude.distanceModulus (convert Length.metersPerMegaparsec Length.metersPerParsec 1.0<Mpc>))

[<Fact>]
let ``the absolute magnitude of the Sun is about 4.83`` () =
    let distance = convert Length.metersPerAu Length.metersPerParsec 1.0<au>
    let absolute = Magnitude.absolute -26.74<mag> distance
    Assert.True(abs (absolute - 4.83<mag>) < 0.01<mag>, $"got {absolute}")

[<Fact>]
let ``AB magnitude and flux density are inverses`` () =
    check (forAll positive (fun x -> within 1e-12 (x * 1.0<Jy>) (Magnitude.abFluxDensity (Magnitude.ab (x * 1.0<Jy>)))))

[<Fact>]
let ``distance modulus and distance are inverses`` () =
    check (
        forAll
            positive
            (fun x ->
                within 1e-12 (x * 1.0<pc>) (Magnitude.distanceOfModulus (Magnitude.distanceModulus (x * 1.0<pc>)))
            )
    )

[<Fact>]
let ``apparent and absolute magnitude are inverses`` () =
    check (
        forAll
            positive
            (fun x ->
                let distance = x * 1.0<pc>
                within 1e-12 3.0<mag> (Magnitude.absolute (Magnitude.apparent 3.0<mag> distance) distance)
            )
    )
