namespace FSharp.Astro.Units

/// Spectral flux density per unit frequency, and the factors that relate the jansky to the SI unit
/// W/(m^2 Hz).
[<RequireQualifiedAccess>]
module FluxDensity =

    /// W/(m^2 Hz) in one jansky. Exact.
    let siPerJansky: float<W / (m^2 Hz Jy)> = 1e-26<W / (m^2 Hz Jy)>

    /// Janskys in one millijansky.
    let janskysPerMillijansky: float<Jy / mJy> = 1e-3<Jy / mJy>

    /// Janskys in one microjansky.
    let janskysPerMicrojansky: float<Jy / uJy> = 1e-6<Jy / uJy>
