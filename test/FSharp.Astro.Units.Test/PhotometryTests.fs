module PhotometryTests

open Xunit
open FSharp.Astro.Units
open Helpers

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
let ``the Vega system is near the AB system in V and far from it in the infrared`` () =
    Assert.InRange(float (Photometry.abOffset Photometry.Bands.V), -0.01, 0.01)
    close 1e-2 0.769<mag> (Photometry.abOffset Photometry.Bands.U)
    close 1e-2 -0.122<mag> (Photometry.abOffset Photometry.Bands.B)
    close 1e-2 1.840<mag> (Photometry.abOffset Photometry.Bands.Ks)

[<Fact>]
let ``the bands run from the ultraviolet to the infrared in order`` () =
    let wavelengths = Photometry.Bands.all |> List.map (fun b -> b.EffectiveWavelength)
    Assert.Equal<float<m> list>(List.sort wavelengths, wavelengths)

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
let ``the SDSS bands run from the ultraviolet to the infrared in order`` () =
    let wavelengths =
        Photometry.Bands.sdssAll |> List.map (fun b -> b.EffectiveWavelength)

    Assert.Equal<float<m> list>(List.sort wavelengths, wavelengths)
