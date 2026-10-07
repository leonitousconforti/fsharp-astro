module PhotometryTests

open Xunit
open FsCheck.FSharp
open FSharp.Astro.Units
open Helpers

[<Fact>]
let ``the AB zero point is zeroth magnitude`` () =
    close 1e-15 0.0<mag> (Photometry.ab Photometry.abZeroPoint)
    close 1e-15 Photometry.abZeroPoint (Photometry.abFluxDensity 0.0<mag>)

[<Fact>]
let ``the ST zero point is zeroth magnitude`` () =
    close 1e-15 0.0<mag> (Photometry.st Photometry.stZeroPoint)
    close 1e-15 Photometry.stZeroPoint (Photometry.stFluxDensity 0.0<mag>)

[<Fact>]
let ``the ST zero point is 3.6308e-9 in cgs`` () =
    let cgs =
        Photometry.stZeroPoint
        * Length.metersPerCentimeter
        * Length.metersPerCentimeter
        * Length.metersPerAngstrom
        / Luminosity.wattsPerErgPerSecond

    close 1e-9 3.6307805477e-9<erg / (s cm^2 AA)> cgs

[<Fact>]
let ``AB and ST agree at the pivot wavelength`` () =
    let fromAb =
        Spectral.perWavelengthOfPerFrequency
            (Photometry.abZeroPoint * FluxDensity.siPerJansky)
            Photometry.abStPivotWavelength

    close 1e-12 Photometry.stZeroPoint fromAb
    close 1e-3 (5475.3<AA> * Length.metersPerAngstrom) Photometry.abStPivotWavelength

[<Fact>]
let ``five magnitudes are a hundred in flux in every system`` () =
    close 1e-12 100.0 (Photometry.abFluxDensity 0.0<mag> / Photometry.abFluxDensity 5.0<mag>)
    close 1e-12 100.0 (Photometry.stFluxDensity 0.0<mag> / Photometry.stFluxDensity 5.0<mag>)

[<Fact>]
let ``the Vega system is near the AB system in V and far from it in the infrared`` () =
    Assert.InRange(float (Photometry.abOffset Photometry.Bands.V), -0.01, 0.01)
    close 1e-2 0.769<mag> (Photometry.abOffset Photometry.Bands.U)
    close 1e-2 -0.122<mag> (Photometry.abOffset Photometry.Bands.B)
    close 1e-2 1.840<mag> (Photometry.abOffset Photometry.Bands.Ks)

[<Fact>]
let ``a zeroth Vega magnitude is the band's zero point`` () =
    for band in Photometry.Bands.all do
        close 1e-15 0.0<mag> (Photometry.vega band band.VegaZeroPoint)
        close 1e-15 band.VegaZeroPoint (Photometry.vegaFluxDensity band 0.0<mag>)

[<Fact>]
let ``Vega magnitudes and flux densities round trip`` () =
    check (
        forAll
            (Gen.choose (-3000, 3000) |> Gen.map (fun n -> float n / 100.0 * 1.0<mag>))
            (fun magnitude ->
                Photometry.Bands.all
                |> List.forall (fun band ->
                    abs (magnitude - Photometry.vega band (Photometry.vegaFluxDensity band magnitude)) < 1e-12<mag>
                )
            )
    )

[<Fact>]
let ``the AB offset converts a magnitude between the systems`` () =
    check (
        forAll
            (Gen.choose (-3000, 3000) |> Gen.map (fun n -> float n / 100.0 * 1.0<mag>))
            (fun magnitude ->
                Photometry.Bands.all
                |> List.forall (fun band ->
                    // The same flux, read on both scales.
                    // Absolute, not relative: a magnitude passes through zero, and these two
                    // routes to it differ in the last bit rather than in proportion.
                    let flux = Photometry.vegaFluxDensity band magnitude
                    abs (Photometry.ab flux - Photometry.abOfVega band magnitude) < 1e-12<mag>
                    && abs (magnitude - Photometry.vegaOfAb band (Photometry.abOfVega band magnitude)) < 1e-12<mag>
                )
            )
    )

[<Fact>]
let ``the bands run from the ultraviolet to the infrared in order`` () =
    let wavelengths = Photometry.Bands.all |> List.map (fun b -> b.EffectiveWavelength)
    Assert.Equal<float<m> list>(List.sort wavelengths, wavelengths)

[<Fact>]
let ``a colour index is the blue magnitude minus the red one`` () =
    close 1e-15 0.65<mag> (Photometry.colourIndex 10.65<mag> 10.0<mag>)

[<Fact>]
let ``the absolute bolometric magnitude of the Sun is 4.74`` () =
    close 1e-3 4.74<mag> (Photometry.absoluteBolometric Constants.Lsun)

[<Fact>]
let ``the apparent bolometric magnitude of the Sun is -26.83`` () =
    close 1e-3 -26.832<mag> (Photometry.apparentBolometric Constants.solarIrradiance)

[<Fact>]
let ``the two bolometric zero points are the same source at ten parsecs`` () =
    let tenParsecs = 10.0<pc> * Length.metersPerParsec
    let flux = Luminosity.flux Photometry.bolometricLuminosityZeroPoint tenParsecs
    close 1e-6 Photometry.bolometricFluxZeroPoint flux

[<Fact>]
let ``apparent and absolute bolometric magnitudes differ by the distance modulus`` () =
    check (
        forAll
            positive
            (fun d ->
                let parsecs = d * 1.0<pc>
                let metres = parsecs * Length.metersPerParsec
                let luminosity = Constants.Lsun
                let apparent = Photometry.apparentBolometric (Luminosity.flux luminosity metres)
                let absolute = Photometry.absoluteBolometric luminosity
                abs (apparent - absolute - Magnitude.distanceModulus parsecs) < 1e-9<mag>
            )
    )

[<Fact>]
let ``bolometric magnitudes and luminosities round trip`` () =
    check (
        forAll
            positive
            (fun l ->
                let luminosity = l * 1.0<W>

                within
                    1e-12
                    luminosity
                    (Photometry.luminosityOfAbsoluteBolometric (Photometry.absoluteBolometric luminosity))
            )
    )

[<Fact>]
let ``absolute and apparent magnitudes round trip through a distance and an extinction`` () =
    let distance = 1500.0<pc>
    let extinction = 2.4<mag>
    let absolute = Photometry.absolute 14.2<mag> distance extinction
    close 1e-12 14.2<mag> (Photometry.apparent absolute distance extinction)
    // With no extinction this is the plain distance modulus.
    close 1e-12 (Magnitude.absolute 14.2<mag> distance) (Photometry.absolute 14.2<mag> distance 0.0<mag>)

[<Fact>]
let ``the SDSS bands reproduce Vega's synthetic AB magnitudes`` () =
    // The bands are built from these, so this checks that abOffset inverts the construction.
    let expected = [
        Photometry.Bands.u, 0.951<mag>
        Photometry.Bands.g, -0.080<mag>
        Photometry.Bands.r, 0.169<mag>
        Photometry.Bands.i, 0.389<mag>
        Photometry.Bands.z, 0.556<mag>
    ]

    for band, offset in expected do
        close 1e-12 offset (Photometry.abOffset band)

[<Fact>]
let ``the SDSS bands run from the ultraviolet to the infrared in order`` () =
    let wavelengths =
        Photometry.Bands.sdssAll |> List.map (fun b -> b.EffectiveWavelength)

    Assert.Equal<float<m> list>(List.sort wavelengths, wavelengths)

[<Fact>]
let ``an SDSS magnitude is very nearly an AB magnitude in g`` () =
    // SDSS is an AB system, so converting a g magnitude to AB barely moves it.
    Assert.InRange(float (Photometry.abOffset Photometry.Bands.g), -0.1, 0.0)

[<Fact>]
let ``every band round trips a magnitude through its flux density`` () =
    check (
        forAll
            (Gen.choose (-3000, 3000) |> Gen.map (fun n -> float n / 100.0 * 1.0<mag>))
            (fun magnitude ->
                Photometry.Bands.all @ Photometry.Bands.sdssAll
                |> List.forall (fun band ->
                    abs (magnitude - Photometry.vega band (Photometry.vegaFluxDensity band magnitude)) < 1e-12<mag>
                )
            )
    )
