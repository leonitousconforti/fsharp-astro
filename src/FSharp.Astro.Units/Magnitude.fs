namespace FSharp.Astro.Units

/// Astronomical magnitudes on the Pogson scale, where 5 magnitudes are a factor of 100 in flux.
[<RequireQualifiedAccess>]
module Magnitude =

    /// Magnitude difference corresponding to a flux ratio, -2.5 log10 (flux / reference). The two
    /// fluxes must share a unit; the result is brighter-is-smaller.
    let ofFluxRatio (flux: float<'u>) (reference: float<'u>) : float<mag> =
        -2.5 * log10 (flux / reference) * 1.0<mag>

    /// Flux ratio corresponding to a magnitude difference, 10^(-0.4 dm).
    let fluxRatio (difference: float<mag>) : float = 10.0 ** (-0.4 * float difference)

    /// Zero point of the AB system, the flux density of a zeroth magnitude source.
    let abZeroPoint: float<Jy> = 3631.0<Jy>

    /// AB magnitude of a flux density per unit frequency.
    let ab (fluxDensity: float<Jy>) : float<mag> = ofFluxRatio fluxDensity abZeroPoint

    /// Flux density per unit frequency of a source with the given AB magnitude.
    let abFluxDensity (magnitude: float<mag>) : float<Jy> = abZeroPoint * fluxRatio magnitude

    /// Distance modulus, 5 log10 (d / 10 pc).
    let distanceModulus (distance: float<pc>) : float<mag> =
        5.0 * log10 (distance / 10.0<pc>) * 1.0<mag>

    /// Distance at which a source has the given distance modulus.
    let distanceOfModulus (modulus: float<mag>) : float<pc> =
        10.0<pc> * 10.0 ** (float modulus / 5.0)

    /// Absolute magnitude of a source with the given apparent magnitude at the given distance,
    /// ignoring extinction.
    let absolute (apparent: float<mag>) (distance: float<pc>) : float<mag> = apparent - distanceModulus distance

    /// Apparent magnitude of a source with the given absolute magnitude at the given distance,
    /// ignoring extinction.
    let apparent (absolute: float<mag>) (distance: float<pc>) : float<mag> = absolute + distanceModulus distance
