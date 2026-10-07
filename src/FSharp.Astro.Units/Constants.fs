namespace FSharp.Astro.Units

open System

/// Physical and astronomical constants with their units attached.
///
/// Values are the exact defining constants of the 2019 SI, the 2018 CODATA
/// recommended values for everything measured, and the IAU 2015 Resolution B3
/// nominal conversion constants for the Sun, Earth and Jupiter. A nominal value
/// is a fixed number the IAU chose so that results stay comparable between
/// papers. It is not a measurement, so `Rsun` is exactly 6.957e8 m.
[<RequireQualifiedAccess>]
module Constants =

    /// Speed of light in vacuum. Exact.
    let c: float<m / s> = 299792458.0<m / s>

    /// Newtonian constant of gravitation. CODATA 2018.
    let G: float<m^3 / (kg s^2)> = 6.67430e-11<m^3 / (kg s^2)>

    /// Planck constant. Exact.
    let h: float<J s> = 6.62607015e-34<J s>

    /// Reduced Planck constant, h/2pi.
    let hbar: float<J s> = h / (2.0 * Math.PI)

    /// Boltzmann constant. Exact.
    let kB: float<J / K> = 1.380649e-23<J / K>

    /// Stefan-Boltzmann constant, 2 pi^5 kB^4 / (15 h^3 c^2). Exact, computed
    /// from its definition.
    let sigmaSB: float<W / (m^2 K^4)> =
        2.0 * Math.PI ** 5.0 * (kB * kB * kB * kB) / (15.0 * (h * h * h) * (c * c))

    /// Wien wavelength displacement law constant. CODATA 2018.
    let bWien: float<m K> = 2.897771955e-3<m K>

    /// Electron mass. CODATA 2018.
    let electronMass: float<kg> = 9.1093837015e-31<kg>

    /// Proton mass. CODATA 2018.
    let protonMass: float<kg> = 1.67262192369e-27<kg>

    /// Neutron mass. CODATA 2018.
    let neutronMass: float<kg> = 1.67492749804e-27<kg>

    /// Atomic mass constant, one twelfth of the mass of a carbon-12 atom.
    /// CODATA 2018.
    let atomicMassUnit: float<kg> = 1.66053906660e-27<kg>

    /// Thomson cross section. CODATA 2018.
    let sigmaT: float<m^2> = 6.6524587321e-29<m^2>

    /// Nominal solar mass parameter GM. IAU 2015 B3.
    let GMsun: float<m^3 / s^2> = 1.3271244e20<m^3 / s^2>

    /// Solar mass, GMsun / G. Inherits the relative uncertainty of G, about
    /// 2e-5.
    let Msun: float<kg> = GMsun / G

    /// Nominal solar radius. IAU 2015 B3.
    let Rsun: float<m> = 6.957e8<m>

    /// Nominal solar luminosity. IAU 2015 B3.
    let Lsun: float<W> = 3.828e26<W>

    /// Nominal solar effective temperature. IAU 2015 B3.
    let Tsun: float<K> = 5772.0<K>

    /// Nominal total solar irradiance at 1 au. IAU 2015 B3.
    let solarIrradiance: float<W / m^2> = 1361.0<W / m^2>

    /// Nominal terrestrial mass parameter GM. IAU 2015 B3.
    let GMearth: float<m^3 / s^2> = 3.986004e14<m^3 / s^2>

    /// Earth mass, GMearth / G.
    let Mearth: float<kg> = GMearth / G

    /// Nominal equatorial radius of the Earth. IAU 2015 B3.
    let Rearth: float<m> = 6.3781e6<m>

    /// Nominal polar radius of the Earth. IAU 2015 B3.
    let RearthPolar: float<m> = 6.3568e6<m>

    /// Nominal jovian mass parameter GM. IAU 2015 B3.
    let GMjup: float<m^3 / s^2> = 1.2668653e17<m^3 / s^2>

    /// Jupiter mass, GMjup / G.
    let Mjup: float<kg> = GMjup / G

    /// Nominal equatorial radius of Jupiter. IAU 2015 B3.
    let Rjup: float<m> = 7.1492e7<m>

    /// Nominal polar radius of Jupiter. IAU 2015 B3.
    let RjupPolar: float<m> = 6.6854e7<m>
