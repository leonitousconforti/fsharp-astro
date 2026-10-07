namespace FSharp.Astro.Units

/// Mass units and the factors that relate them to the kilogram.
[<RequireQualifiedAccess>]
module Mass =

    /// Kilograms in one gram.
    let kilogramsPerGram: float<kg / g> = 1e-3<kg / g>

    /// Kilograms in one solar mass. Derived from the IAU 2015 nominal GMsun and CODATA 2018 G.
    let kilogramsPerSolarMass: float<kg / Msun> = Constants.Msun / 1.0<Msun>

    /// Kilograms in one Earth mass. Derived from the IAU 2015 nominal GMearth and CODATA 2018 G.
    let kilogramsPerEarthMass: float<kg / Mearth> = Constants.Mearth / 1.0<Mearth>

    /// Kilograms in one Jupiter mass. Derived from the IAU 2015 nominal GMjup and CODATA 2018 G.
    let kilogramsPerJupiterMass: float<kg / Mjup> = Constants.Mjup / 1.0<Mjup>

    /// Converts between two mass units given their factors to kilograms:
    /// `Mass.convert Mass.kilogramsPerSolarMass Mass.kilogramsPerJupiterMass 1.0<Msun>` is about `1048.0<Mjup>`.
    let convert (from: float<kg / 'a>) (into: float<kg / 'b>) (x: float<'a>) : float<'b> = x * from / into
