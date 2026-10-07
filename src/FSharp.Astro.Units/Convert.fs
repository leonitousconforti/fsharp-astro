namespace FSharp.Astro.Units

/// Converting a value from one unit to another.
[<AutoOpen>]
module Conversion =

    /// Converts between two units of one quantity, given each one's factor to
    /// the base unit they share: `convert Length.metersPerParsec
    /// Length.metersPerLightYear 1.0<pc>` is about `3.26<ly>`, and `convert
    /// Time.secondsPerDay Time.secondsPerYear 365.25<d>` is `1.0<yr>`.
    ///
    /// The base unit is a measure parameter like the two units are, so one
    /// function serves every quantity. There is nothing to it but `x * from /
    /// into`, which is worth knowing: a factor is an ordinary number with a
    /// unit on it, and multiplying by one is what a conversion is. This exists
    /// for the times a conversion wants a name, as `|> convert a b` in a chain
    /// does.
    let convert (from: float<'b / 'a>) (into: float<'b / 'c>) (x: float<'a>) : float<'c> = x * from / into
