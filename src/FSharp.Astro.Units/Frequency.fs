namespace FSharp.Astro.Units

/// Frequency units and the factors that relate them to the hertz.
[<RequireQualifiedAccess>]
module Frequency =

    /// Hertz in one kilohertz.
    let hertzPerKilohertz: float<Hz / kHz> = 1e3<Hz / kHz>

    /// Hertz in one megahertz.
    let hertzPerMegahertz: float<Hz / MHz> = 1e6<Hz / MHz>

    /// Hertz in one gigahertz.
    let hertzPerGigahertz: float<Hz / GHz> = 1e9<Hz / GHz>

    /// Hertz in one terahertz.
    let hertzPerTerahertz: float<Hz / THz> = 1e12<Hz / THz>
