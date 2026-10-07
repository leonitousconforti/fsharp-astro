module SpectralTests

open Xunit
open FSharp.Astro.Units
open Helpers

[<Fact>]
let ``the hydrogen line is 21 centimeters`` () =
    let frequency = 1420.405751768<MHz> * Frequency.hertzPerMegahertz
    let wavelength =
        Spectral.wavelengthOfFrequency frequency / Length.metersPerCentimeter
    close 1e-9 21.106114054<cm> wavelength

[<Fact>]
let ``a one angstrom photon carries 12.4 keV`` () =
    let energy = Spectral.energyOfWavelength (1.0<AA> * Length.metersPerAngstrom)
    close 1e-7 12.3984198<keV> (energy / Energy.joulesPerKiloelectronVolt)

[<Fact>]
let ``a jansky in cgs`` () =
    let cgs: float<erg / (s cm^2 Hz)> =
        1.0<Jy> * FluxDensity.siPerJansky / Luminosity.wattsPerErgPerSecond
        * Length.metersPerCentimeter
        * Length.metersPerCentimeter

    close 1e-15 1e-23<erg / (s cm^2 Hz)> cgs

[<Fact>]
let ``the AB zero point at 5500 angstroms in cgs per angstrom`` () =
    let wavelength = 5500.0<AA> * Length.metersPerAngstrom
    let perFrequency = Magnitude.abZeroPoint * FluxDensity.siPerJansky
    let perWavelength = Spectral.perWavelengthOfPerFrequency perFrequency wavelength

    let cgs: float<erg / (s cm^2 AA)> =
        perWavelength / Luminosity.wattsPerErgPerSecond
        * Length.metersPerCentimeter
        * Length.metersPerCentimeter
        * Length.metersPerAngstrom

    close 1e-4 3.5983e-9<erg / (s cm^2 AA)> cgs
