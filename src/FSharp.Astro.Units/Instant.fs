namespace FSharp.Astro.Units

open System

/// A point on an astronomical timeline, held as two doubles whose sum is a Julian Date.
///
/// A Julian Date in one double is coarse. The modern ones are around 2.46 million, where the gap
/// between neighbouring doubles is 4.7e-10 days, or 40 microseconds. That is fine for pointing a
/// telescope and useless for pulsar timing or VLBI, and it does not improve: the exponent is
/// fixed by the size of the number, so every instant this century is quantised at 40 microseconds
/// no matter how the arithmetic is done.
///
/// Splitting the value in two fixes it, which is why ERFA and SOFA take Julian Dates as a pair of
/// doubles. `Day` holds a whole number of days and carries the magnitude; `Fraction` holds the
/// rest and gets an exponent suited to its own size. The representation here is canonical, with
/// `Day` an exact integer and `Fraction` in `[-0.5, 0.5)`, so two instants are equal exactly when
/// they name the same time and comparison is chronological. The resolution is the spacing of
/// doubles near a half, 1.1e-16 days or about 9.6 picoseconds. That is finer than a single double
/// by exactly 2^22, a factor of four million, and it is past anything anyone measures: the most
/// demanding millisecond pulsar timing wants tens of nanoseconds.
///
/// The gain shows up in `Instant.difference`, where the two integer day counts cancel exactly
/// before the fractions are subtracted, so the gap between two instants in the same day comes
/// back to picoseconds rather than to 40 microseconds.
///
/// Construct with `Instant.ofParts`, `Instant.ofJd` or `Epoch.ofDateTimeOffset`. No time scale is
/// modelled: an instant is whatever scale was put into it.
[<Struct>]
type Instant = private {
    day: float<jd>
    fraction: float<jd>
} with

    /// The whole day part, always an exact integer.
    member this.Day = this.day

    /// The fraction of a day, in `[-0.5, 0.5)`.
    member this.Fraction = this.fraction

    /// The Julian Date as a single double. Rounds away the precision the two parts exist to keep.
    member this.JulianDate: float<jd> = this.day + this.fraction

    override this.ToString() =
        $"JD %.9f{float this.day + float this.fraction}"

/// Building and combining `Instant` values.
[<RequireQualifiedAccess>]
module Instant =

    /// Knuth's two-sum: the rounded sum of two doubles and the exact amount that rounding lost.
    /// `a + b = sum + remainder` with no error at all, which is what lets the normal form below
    /// be canonical rather than merely close.
    let private twoSum (a: float) (b: float) : struct (float * float) =
        let sum = a + b
        let bb = sum - a
        struct (sum, (a - (sum - bb)) + (b - bb))

    /// An instant from two parts that sum to a Julian Date. Neither part need be whole: the pair
    /// is renormalised so that `Day` is an integer and `Fraction` lies in `[-0.5, 0.5)`.
    ///
    /// Put the magnitude in the first argument and the detail in the second. `ofParts 2451545.0<jd>
    /// 1e-11<jd>` keeps that last picosecond of a day; writing the same instant as a single
    /// `2451545.00000000001<jd>` loses it before the call even happens.
    let ofParts (day: float<jd>) (fraction: float<jd>) : Instant =
        let struct (sum, remainder) = twoSum (float day) (float fraction)

        if not (Double.IsFinite sum) then
            invalidArg "day" "a Julian Date must be finite"

        // `sum - whole` is exact: they share an exponent and differ by at most a half, so the
        // result is representable and no bits are lost before the remainder is folded back in.
        let whole = Math.Round sum
        let rest = (sum - whole) + remainder

        // Folding the remainder in can push the fraction a hair past the half. One shift fixes
        // it, and both the shift and the subtraction below are exact.
        if rest >= 0.5 then
            {
                day = (whole + 1.0) * 1.0<jd>
                fraction = (rest - 1.0) * 1.0<jd>
            }
        elif rest < -0.5 then
            {
                day = (whole - 1.0) * 1.0<jd>
                fraction = (rest + 1.0) * 1.0<jd>
            }
        else
            { day = whole * 1.0<jd>; fraction = rest * 1.0<jd> }

    /// An instant from a Julian Date in a single double. Cannot recover precision the double has
    /// already lost; use `ofParts` when the instant is known more precisely than 40 microseconds.
    let ofJd (julianDate: float<jd>) : Instant = ofParts julianDate 0.0<jd>

    /// The Julian Date as a single double, for printing or for an interface that wants one.
    /// Lossy by construction.
    let toJd (instant: Instant) : float<jd> = instant.JulianDate

    /// Days from the second instant to the first.
    ///
    /// The day counts are integers and cancel exactly, so for two instants within a day of each
    /// other this is as precise as the fractions themselves, picoseconds rather than the 40
    /// microseconds a single-double Julian Date would allow. Over longer gaps the spacing of the
    /// returned double takes over: a gap of a million days carries its own 10 microseconds.
    let difference (a: Instant) (b: Instant) : float<d> =
        ((a.Day - b.Day) + (a.Fraction - b.Fraction)) * 1.0<d / jd>

    /// The instant a duration after the given one. The whole and fractional days are added
    /// separately so that a long duration does not swamp a short one.
    let add (duration: float<d>) (instant: Instant) : Instant =
        let days = float duration
        let whole = Math.Truncate days
        ofParts (instant.Day + whole * 1.0<jd>) (instant.Fraction + (days - whole) * 1.0<jd>)

    /// The earlier of two instants.
    let earlier (a: Instant) (b: Instant) : Instant = if a <= b then a else b

    /// The later of two instants.
    let later (a: Instant) (b: Instant) : Instant = if a >= b then a else b
