module LengthTests

open Xunit
open FSharp.Astro.Units
open Helpers

[<Fact>]
let ``a parsec in meters matches the IAU 2015 definition`` () =
    close 1e-15 3.0856775814913673e16<m> (1.0<pc> * Length.metersPerParsec)

[<Fact>]
let ``a light year in meters is exact`` () =
    Assert.Equal(9460730472580800.0<m>, 1.0<ly> * Length.metersPerLightYear)

[<Fact>]
let ``a parsec in light years`` () =
    close 1e-14 3.2615637771674333<ly> (Length.convert Length.metersPerParsec Length.metersPerLightYear 1.0<pc>)

[<Fact>]
let ``a parsec in astronomical units`` () =
    close 1e-14 206264.80624709636<au> (Length.convert Length.metersPerParsec Length.metersPerAu 1.0<pc>)

[<Fact>]
let ``the parsec is the distance at which one au subtends one arcsecond`` () =
    let fromDefinition = float Length.metersPerAu / float Angle.radiansPerArcsecond
    close 1e-15 fromDefinition (float Length.metersPerParsec)

[<Fact>]
let ``light takes 499 seconds to cross one astronomical unit`` () =
    close 1e-14 499.00478383615643<s> (1.0<au> * Length.metersPerAu / Constants.c)

[<Fact>]
let ``metric prefixes on the parsec`` () =
    Assert.Equal(1000.0<pc>, Length.convert Length.metersPerKiloparsec Length.metersPerParsec 1.0<kpc>)
    Assert.Equal(1000.0<kpc>, Length.convert Length.metersPerMegaparsec Length.metersPerKiloparsec 1.0<Mpc>)
    Assert.Equal(1000.0<Mpc>, Length.convert Length.metersPerGigaparsec Length.metersPerMegaparsec 1.0<Gpc>)

[<Fact>]
let ``an angstrom is a tenth of a nanometer`` () =
    close 1e-15 550.0<nm> (Length.convert Length.metersPerAngstrom Length.metersPerNanometer 5500.0<AA>)

[<Fact>]
let ``the solar radius in astronomical units`` () =
    close 1e-6 4.65047e-3<au> (Length.convert Length.metersPerSolarRadius Length.metersPerAu 1.0<Rsun>)

[<Fact>]
let ``Jupiter is about eleven Earth radii across`` () =
    close 1e-4 11.209<Rearth> (Length.convert Length.metersPerJupiterRadius Length.metersPerEarthRadius 1.0<Rjup>)

[<Fact>]
let ``parallax and distance are reciprocals`` () =
    Assert.Equal(10.0<pc>, Length.ofParallax 0.1<arcsec>)
    Assert.Equal(0.01<arcsec>, Length.parallax 100.0<pc>)

[<Fact>]
let ``a milliarcsecond of parallax is a kiloparsec`` () =
    let parallax =
        Angle.convert Angle.radiansPerMilliarcsecond Angle.radiansPerArcsecond 1.0<mas>
    close 1e-12 1.0<kpc> (Length.convert Length.metersPerParsec Length.metersPerKiloparsec (Length.ofParallax parallax))

[<Fact>]
let ``the Hubble time for 70 km/s/Mpc is about fourteen gigayears`` () =
    let hubble = 70.0<km / (s Mpc)>

    let hubbleTime: float<Gyr> =
        1.0 / hubble * Length.metersPerMegaparsec
        / Length.metersPerKilometer
        / Time.secondsPerGigayear

    close 1e-4 13.968<Gyr> hubbleTime

[<Fact>]
let ``converting there and back is the identity`` () =
    check (
        forAll
            positive
            (fun x ->
                let d = x * 1.0<pc>

                Length.convert Length.metersPerParsec Length.metersPerLightYear d
                |> Length.convert Length.metersPerLightYear Length.metersPerParsec
                |> within 1e-12 d
            )
    )

[<Fact>]
let ``parallax of the distance of a parallax is the parallax`` () =
    check (
        forAll
            positive
            (fun x -> within 1e-12 (x * 1.0<arcsec>) (Length.parallax (Length.ofParallax (x * 1.0<arcsec>))))
    )
