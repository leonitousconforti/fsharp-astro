namespace FSharp.Astro.Units

open System

/// Angle units, their factors to the radian, and trigonometry that keeps the units honest.
[<RequireQualifiedAccess>]
module Angle =

    /// Radians in one degree.
    let radiansPerDegree: float<rad / deg> = Math.PI / 180.0 * 1.0<rad / deg>

    /// Radians in one arcminute.
    let radiansPerArcminute: float<rad / arcmin> = Math.PI / 10800.0 * 1.0<rad / arcmin>

    /// Radians in one arcsecond.
    let radiansPerArcsecond: float<rad / arcsec> =
        Math.PI / 648000.0 * 1.0<rad / arcsec>

    /// Radians in one milliarcsecond.
    let radiansPerMilliarcsecond: float<rad / mas> =
        radiansPerArcsecond * 1e-3<arcsec / mas>

    /// Radians in one microarcsecond.
    let radiansPerMicroarcsecond: float<rad / uas> =
        radiansPerArcsecond * 1e-6<arcsec / uas>

    /// Radians in one hour of right ascension, which is 15 degrees.
    let radiansPerHourAngle: float<rad / hourangle> =
        Math.PI / 12.0 * 1.0<rad / hourangle>

    /// Sine of an angle in radians.
    let sin (x: float<rad>) : float = Math.Sin(float x)

    /// Cosine of an angle in radians.
    let cos (x: float<rad>) : float = Math.Cos(float x)

    /// Tangent of an angle in radians.
    let tan (x: float<rad>) : float = Math.Tan(float x)

    /// Inverse sine, in radians.
    let asin (x: float) : float<rad> = Math.Asin x * 1.0<rad>

    /// Inverse cosine, in radians.
    let acos (x: float) : float<rad> = Math.Acos x * 1.0<rad>

    /// Inverse tangent, in radians.
    let atan (x: float) : float<rad> = Math.Atan x * 1.0<rad>

    /// Two-argument inverse tangent of two lengths in the same unit, in radians.
    let atan2 (y: float<'u>) (x: float<'u>) : float<rad> = Math.Atan2(float y, float x) * 1.0<rad>

    /// Wraps an angle into [0, period). The period carries the unit, so
    /// `Angle.wrap 360.0<deg>` and `Angle.wrap 24.0<hourangle>` both work.
    let wrap (period: float<'u>) (x: float<'u>) : float<'u> =
        let r = x % period

        if r < 0.0<_> then
            let shifted = r + period
            // A tiny negative remainder can round back up to exactly one period.
            if shifted >= period then 0.0<_> else shifted
        else
            r

    /// Wraps an angle into [-period/2, period/2), the convention for longitudes and hour angles
    /// measured from a meridian.
    let wrapSigned (period: float<'u>) (x: float<'u>) : float<'u> =
        let half = period / 2.0
        wrap period (x + half) - half
