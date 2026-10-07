module DopplerTests

open Xunit
open FsCheck.FSharp
open FSharp.Astro.Units
open Helpers

let private conventions = [ Radio; Optical; Relativistic ]

[<Fact>]
let ``the radio convention is linear in frequency`` () =
    // v = c (1 - nu/nu0) by definition.
    let rest = 1420.405751768<MHz>
    let observed = 1420.0<MHz>
    let expected = Constants.c * (1.0 - observed / rest)
    close 1e-11 expected (Doppler.velocityOfFrequency Radio rest observed)

[<Fact>]
let ``the relativistic convention doubles the wavelength at three fifths of c`` () =
    // beta = 0.6 gives sqrt((1+beta)/(1-beta)) = 2, so 1 + z = 2.
    close 1e-15 1.0 (Doppler.redshiftOfBeta Relativistic 0.6)
    close 1e-15 0.6 (Doppler.betaOfRedshift Relativistic 1.0)

[<Fact>]
let ``the relativistic convention never reaches the speed of light`` () =
    check (
        forAll
            (Gen.choose (1, 100_000) |> Gen.map (fun n -> float n / 100.0))
            (fun z -> Doppler.betaOfRedshift Relativistic z < 1.0)
    )

[<Fact>]
let ``the relativistic convention saturates at c once 1 - beta falls below the float resolution`` () =
    // 1 - beta is about 2/(1+z)^2, so past z of 1e8 the nearest double to beta is 1.0. Nothing in
    // double arithmetic can do better; the velocity is simply c at that point.
    Assert.True(Doppler.betaOfRedshift Relativistic 1e7 < 1.0)
    Assert.Equal(1.0, Doppler.betaOfRedshift Relativistic 1e9)

[<Fact>]
let ``velocity and redshift round trip under every convention`` () =
    check (
        forAll
            (Gen.choose (1, 9000) |> Gen.map (fun n -> float n / 10000.0))
            (fun z ->
                conventions
                |> List.forall (fun convention ->
                    let back =
                        Doppler.redshiftOfVelocity convention (Doppler.velocityOfRedshift convention z)

                    abs (back - z) < 1e-12 * max 1.0 z
                )
            )
    )

[<Fact>]
let ``the conventions agree to first order and diverge at second order`` () =
    let rest = 1420.405751768<MHz>
    // A thousand km/s is beta = 1/300, so the three should agree to about a part in three hundred
    // and disagree by more than a part in a million.
    let velocity = 1000.0<km / s> * Length.metersPerKilometer
    let frequencies =
        conventions |> List.map (fun c -> Doppler.frequencyOfVelocity c rest velocity)
    let radio = frequencies.[0]
    let relativistic = frequencies.[2]
    let spread = abs (radio - relativistic) / rest
    Assert.InRange(spread, 1e-6, 1e-2)

[<Fact>]
let ``converting between conventions preserves the spectral shift`` () =
    let rest = 1.0<GHz>

    check (
        forAll
            (Gen.choose (-9000, 9000) |> Gen.map (fun n -> float n * 1.0<km / s>))
            (fun v ->
                let velocity = v * Length.metersPerKilometer

                conventions
                |> List.forall (fun from ->
                    conventions
                    |> List.forall (fun into ->
                        let observed = Doppler.frequencyOfVelocity from rest velocity
                        let converted = Doppler.convert from into velocity
                        let back = Doppler.frequencyOfVelocity into rest converted
                        within 1e-12 observed back
                    )
                )
            )
    )

[<Fact>]
let ``radio channel widths in velocity do not depend on where in the band they sit`` () =
    let rest = 1420.405751768<MHz>
    let width = 0.1<MHz>

    let at centre =
        Doppler.velocityWidthOfFrequencyWidth Radio rest centre width

    close 1e-9 (at 1420.0<MHz>) (at 1400.0<MHz>)

    // The optical convention does depend on it, which is the point of using radio for line work.
    let optical centre =
        Doppler.velocityWidthOfFrequencyWidth Optical rest centre width

    Assert.False(within 1e-6 (optical 1420.0<MHz>) (optical 1400.0<MHz>))
