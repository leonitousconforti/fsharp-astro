module EnergyTests

open Xunit
open FSharp.Astro.Units
open Helpers

[<Fact>]
let ``an electron volt in joules is exact`` () =
    Assert.Equal(1.602176634e-19<J>, 1.0<eV> * Energy.joulesPerElectronVolt)

[<Fact>]
let ``a kiloelectron volt in ergs`` () =
    close 1e-15 1.602176634e-9<erg> (convert Energy.joulesPerKiloelectronVolt Energy.joulesPerErg 1.0<keV>)

[<Fact>]
let ``the electron volt prefixes`` () =
    Assert.Equal(1000.0<keV>, convert Energy.joulesPerMegaelectronVolt Energy.joulesPerKiloelectronVolt 1.0<MeV>)
    Assert.Equal(1000.0<MeV>, convert Energy.joulesPerGigaelectronVolt Energy.joulesPerMegaelectronVolt 1.0<GeV>)

[<Fact>]
let ``ten million kelvin is a bit under a kiloelectron volt`` () =
    let kT = Energy.ofTemperature 1e7<K> / Energy.joulesPerKiloelectronVolt
    close 1e-5 0.861733<keV> kT

[<Fact>]
let ``thermal energy and temperature are inverses`` () =
    check (
        forAll positive (fun x -> within 1e-12 (x * 1.0<K>) (Energy.temperature (Energy.ofTemperature (x * 1.0<K>))))
    )

[<Fact>]
let ``a solar luminosity in erg per second`` () =
    close 1e-15 3.828e33<erg / s> (convert Luminosity.wattsPerSolarLuminosity Luminosity.wattsPerErgPerSecond 1.0<Lsun>)

[<Fact>]
let ``flux and luminosity are inverses at a fixed distance`` () =
    check (
        forAll
            positive
            (fun x ->
                let luminosity = x * 1.0<W>
                let distance = 10.0<pc> * Length.metersPerParsec
                within 1e-12 luminosity (Luminosity.ofFlux (Luminosity.flux luminosity distance) distance)
            )
    )
