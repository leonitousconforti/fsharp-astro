namespace FSharp.Astro.Units

/// Power units and the factors that relate them to the watt.
[<RequireQualifiedAccess>]
module Luminosity =

    /// Watts in one erg per second.
    let wattsPerErgPerSecond: float<W s / erg> = 1e-7<W s / erg>

    /// Watts in one nominal solar luminosity.
    let wattsPerSolarLuminosity: float<W / Lsun> = Constants.Lsun / 1.0<Lsun>

    /// Converts between two power units given their factors to watts:
    /// `Luminosity.convert Luminosity.wattsPerSolarLuminosity Luminosity.wattsPerErgPerSecond 1.0<Lsun>`
    /// is `3.828e33<erg/s>`.
    let convert (from: float<W / 'a>) (into: float<W / 'b>) (x: float<'a>) : float<'b> = x * from / into

    /// Bolometric flux at a distance from an isotropic source, L / 4 pi d^2.
    let flux (luminosity: float<W>) (distance: float<m>) : float<W / m^2> =
        luminosity / (4.0 * System.Math.PI * distance * distance)

    /// Luminosity of an isotropic source from its bolometric flux at a distance, 4 pi d^2 F.
    let ofFlux (flux: float<W / m^2>) (distance: float<m>) : float<W> =
        4.0 * System.Math.PI * distance * distance * flux

    /// Luminosity of a spherical black body, 4 pi R^2 sigma T^4.
    let blackBody (radius: float<m>) (temperature: float<K>) : float<W> =
        let t2 = temperature * temperature
        4.0 * System.Math.PI * radius * radius * Constants.sigmaSB * t2 * t2
