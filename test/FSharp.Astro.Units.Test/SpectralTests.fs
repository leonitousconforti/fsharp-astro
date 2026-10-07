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
let ``visible light at 5500 angstroms is about 545 terahertz`` () =
    let wavelength = 5500.0<AA> * Length.metersPerAngstrom
    close 1e-12 545.0771963636364<THz> (Spectral.frequencyOfWavelength wavelength / Frequency.hertzPerTerahertz)

[<Fact>]
let ``a one angstrom photon carries 12.4 keV`` () =
    let energy = Spectral.energyOfWavelength (1.0<AA> * Length.metersPerAngstrom)
    close 1e-7 12.3984198<keV> (energy / Energy.joulesPerKiloelectronVolt)

[<Fact>]
let ``frequency prefixes`` () =
    Assert.Equal(1400.0<MHz>, Frequency.convert Frequency.hertzPerGigahertz Frequency.hertzPerMegahertz 1.4<GHz>)

[<Fact>]
let ``a jansky in cgs`` () =
    let cgs: float<erg / (s cm^2 Hz)> =
        1.0<Jy> * FluxDensity.siPerJansky / Luminosity.wattsPerErgPerSecond
        * Length.metersPerCentimeter
        * Length.metersPerCentimeter

    close 1e-15 1e-23<erg / (s cm^2 Hz)> cgs

[<Fact>]
let ``jansky prefixes`` () =
    close
        1e-15
        2000.0<uJy>
        (FluxDensity.convert FluxDensity.janskysPerMillijansky FluxDensity.janskysPerMicrojansky 2.0<mJy>)

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

    close 1e-4 3.5985e-9<erg / (s cm^2 AA)> cgs

[<Fact>]
let ``wavelength and frequency are inverses`` () =
    check (
        forAll
            positive
            (fun x ->
                let wavelength = x * 1.0<m>
                within 1e-12 wavelength (Spectral.wavelengthOfFrequency (Spectral.frequencyOfWavelength wavelength))
            )
    )

[<Fact>]
let ``energy and frequency are inverses`` () =
    check (
        forAll
            positive
            (fun x ->
                let frequency = x * 1.0<Hz>
                within 1e-12 frequency (Spectral.frequencyOfEnergy (Spectral.energyOfFrequency frequency))
            )
    )

[<Fact>]
let ``energy via wavelength agrees with energy via frequency`` () =
    check (
        forAll
            positive
            (fun x ->
                let wavelength = x * 1.0<m>

                within
                    1e-12
                    (Spectral.energyOfWavelength wavelength)
                    (Spectral.energyOfFrequency (Spectral.frequencyOfWavelength wavelength))
            )
    )

[<Fact>]
let ``flux density per frequency and per wavelength are inverses`` () =
    check (
        forAll
            positive
            (fun x ->
                let perFrequency = x * 1.0<W / (m^2 Hz)>
                let wavelength = 2.2<um> * Length.metersPerMicrometer

                within
                    1e-12
                    perFrequency
                    (Spectral.perFrequencyOfPerWavelength
                        (Spectral.perWavelengthOfPerFrequency perFrequency wavelength)
                        wavelength)
            )
    )
