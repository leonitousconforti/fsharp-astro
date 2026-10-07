namespace FSharp.Astro.Units

/// Energy units and the factors that relate them to the joule.
[<RequireQualifiedAccess>]
module Energy =

    /// Joules in one erg.
    let joulesPerErg: float<J / erg> = 1e-7<J / erg>

    /// Joules in one electron volt. Exact since the 2019 SI fixed the elementary charge.
    let joulesPerElectronVolt: float<J / eV> = 1.602176634e-19<J / eV>

    /// Joules in one kiloelectron volt.
    let joulesPerKiloelectronVolt: float<J / keV> =
        joulesPerElectronVolt * 1e3<eV / keV>

    /// Joules in one megaelectron volt.
    let joulesPerMegaelectronVolt: float<J / MeV> =
        joulesPerElectronVolt * 1e6<eV / MeV>

    /// Joules in one gigaelectron volt.
    let joulesPerGigaelectronVolt: float<J / GeV> =
        joulesPerElectronVolt * 1e9<eV / GeV>

    /// Thermal energy kT of a temperature.
    let ofTemperature (temperature: float<K>) : float<J> = Constants.kB * temperature

    /// Temperature whose thermal energy kT is the given energy.
    let temperature (energy: float<J>) : float<K> = energy / Constants.kB
