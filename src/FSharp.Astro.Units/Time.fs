namespace FSharp.Astro.Units

/// Time units and the factors that relate them to the second.
///
/// The year here is the Julian year of exactly 365.25 days, which is what the IAU uses for light
/// years, proper motions and ages. Calendar, tropical and sidereal years are different quantities.
[<RequireQualifiedAccess>]
module Time =

    /// Seconds in one millisecond.
    let secondsPerMillisecond: float<s / ms> = 1e-3<s / ms>

    /// Seconds in one minute.
    let secondsPerMinute: float<s / min> = 60.0<s / min>

    /// Seconds in one hour.
    let secondsPerHour: float<s / h> = 3600.0<s / h>

    /// Seconds in one day.
    let secondsPerDay: float<s / d> = 86400.0<s / d>

    /// Seconds in one Julian year. Exact.
    let secondsPerYear: float<s / yr> = 31557600.0<s / yr>

    /// Seconds in a thousand Julian years.
    let secondsPerKiloyear: float<s / kyr> = secondsPerYear * 1e3<yr / kyr>

    /// Seconds in a million Julian years.
    let secondsPerMegayear: float<s / Myr> = secondsPerYear * 1e6<yr / Myr>

    /// Seconds in a billion Julian years.
    let secondsPerGigayear: float<s / Gyr> = secondsPerYear * 1e9<yr / Gyr>
