namespace FSharp.Astro.Units

open System

/// Instants on an astronomical timeline: Julian Dates, Modified Julian Dates,
/// Julian and Besselian epochs, and the sidereal time at Greenwich.
///
/// An instant is not a duration. `float<d>` is a length of time and scales; an
/// `Instant` is a point and does not, so the relations here are offsets rather
/// than the ratios the quantity modules hold, and there is no `convert`. The
/// Julian Date is the pivot: every other form converts through it. Subtract two
/// instants with `Instant.difference` to get a `float<d>` you can hand to
/// `convert`.
///
/// Instants are the two-part `Instant` rather than a single `float<jd>`,
/// because a Julian Date in one double is quantised at 40 microseconds. See
/// `Instant` for why and for what it costs, which is nothing. Call
/// `Instant.ofJd` to bring a plain Julian Date in.
///
/// No time scale is modelled. An instant here is whatever scale you put in, and
/// the arithmetic is uniform, so an instant built from a UTC calendar date
/// carries that scale's leap second discontinuities. The sidereal time
/// functions want UT1 specifically.
[<RequireQualifiedAccess>]
module Epoch =

    /// The Modified Julian Date origin, 1858-11-17T00:00. Exact.
    let mjdOrigin: Instant = Instant.ofParts 2400000.0<jd> 0.5<jd>

    /// J2000.0, 2000-01-01T12:00 TT. The origin of the modern epoch.
    let j2000: Instant = Instant.ofParts 2451545.0<jd> 0.0<jd>

    /// B1950.0, the origin of the Besselian epoch still attached to old
    /// catalogue coordinates.
    let b1950: Instant = Instant.ofParts 2433282.0<jd> 0.4235<jd>

    /// The Unix epoch, 1970-01-01T00:00 UTC. Exact.
    let unixOrigin: Instant = Instant.ofParts 2440587.0<jd> 0.5<jd>

    /// Days in one Julian century, by definition.
    let daysPerJulianCentury: float<d> = 36525.0<d>

    /// Days in one Julian year, by definition.
    let daysPerJulianYear: float<d / jyear> = 365.25<d / jyear>

    /// Days in one Besselian year, the tropical year at B1900.0 used by
    /// Newcomb.
    let daysPerBesselianYear: float<d / byear> = 365.242198781<d / byear>

    /// Julian centuries from J2000.0, the argument of most of the IAU
    /// polynomials.
    let julianCenturies (instant: Instant) : float =
        Instant.difference instant j2000 / daysPerJulianCentury

    /// Modified Julian Date of an instant, JD - 2400000.5. A single double, so
    /// lossy in the same way `Instant.toJd` is, though less so: a Modified
    /// Julian Date is a fiftieth the size of a Julian Date and resolves 630
    /// nanoseconds rather than 40 microseconds.
    let toMjd (instant: Instant) : float<mjd> =
        Instant.difference instant mjdOrigin * 1.0<mjd / d>

    /// Instant of a Modified Julian Date held as two parts that sum, the whole
    /// days and the fraction of a day. The precise form: a single `float<mjd>`
    /// cannot carry a microsecond, since its own spacing at 51544.5 is 630
    /// nanoseconds.
    let ofMjdParts (day: float<mjd>) (fraction: float<mjd>) : Instant =
        let whole = Math.Truncate(float day)

        Instant.ofParts
            (mjdOrigin.Day + whole * 1.0<jd>)
            (mjdOrigin.Fraction + (float day - whole) * 1.0<jd> + float fraction * 1.0<jd>)

    /// Instant of a Modified Julian Date. The whole and fractional days are
    /// split apart so that nothing is lost inside this call, but a Modified
    /// Julian Date already rounded into one double arrives rounded; use
    /// `ofMjdParts` when the time is known better than a microsecond.
    let ofMjd (modified: float<mjd>) : Instant = ofMjdParts modified 0.0<mjd>

    /// Julian epoch of an instant. J2000.0 is 2451545.0, and a Julian year is
    /// exactly 365.25 days, so 2001.0 is one such year later.
    let toJulianEpoch (instant: Instant) : float<jyear> =
        2000.0<jyear> + Instant.difference instant j2000 / daysPerJulianYear

    /// Instant of a Julian epoch. `Epoch.ofJulianEpoch 2000.0<jyear>` is
    /// `Epoch.j2000`.
    let ofJulianEpoch (epoch: float<jyear>) : Instant =
        Instant.add ((epoch - 2000.0<jyear>) * daysPerJulianYear) j2000

    /// Besselian epoch of an instant, B = 1900.0 + (JD - 2415020.31352) /
    /// 365.242198781. Besselian epochs are obsolete for new work but B1950.0 is
    /// still printed on old positions.
    let private besselianOrigin = Instant.ofParts 2415020.0<jd> 0.31352<jd>

    /// Besselian epoch of an instant.
    let toBesselianEpoch (instant: Instant) : float<byear> =
        1900.0<byear>
        + Instant.difference instant besselianOrigin / daysPerBesselianYear

    /// Instant of a Besselian epoch.
    let ofBesselianEpoch (epoch: float<byear>) : Instant =
        Instant.add ((epoch - 1900.0<byear>) * daysPerBesselianYear) besselianOrigin

    // -----------------------------------------------------------------------------------------
    // The civil calendar. .NET counts 100 nanosecond ticks from 0001-01-01,
    // which is JD 1721425.5, so the conversion is exact in both directions to
    // the tick: the whole days go into the day part and the ticks within the
    // day into the fraction, and nothing is ever rounded through a number of
    // the size of a Julian Date.
    // -----------------------------------------------------------------------------------------

    let private ticksOrigin = Instant.ofParts 1721425.0<jd> 0.5<jd>
    let private ticksPerDay = 864000000000L

    /// Instant of a point on the civil calendar, exact to the 100 nanosecond
    /// tick. Leap seconds are not modelled, so this is a JD(UTC): the second a
    /// leap second is inserted maps to the same Julian Date as the one before
    /// it.
    let ofDateTimeOffset (instant: DateTimeOffset) : Instant =
        let ticks = instant.UtcTicks

        Instant.ofParts
            (ticksOrigin.Day + float (ticks / ticksPerDay) * 1.0<jd>)
            (ticksOrigin.Fraction + float (ticks % ticksPerDay) / float ticksPerDay * 1.0<jd>)

    /// Civil calendar point of an instant, in UTC, rounded to the 100
    /// nanosecond tick.
    let toDateTimeOffset (instant: Instant) : DateTimeOffset =
        // The day counts are integers and cancel exactly; only the fraction is
        // scaled, and it is small enough that the tick count stays well inside
        // what a double holds exactly.
        let days = int64 (float (instant.Day - ticksOrigin.Day))

        let withinDay =
            int64 (Math.Round(float (instant.Fraction - ticksOrigin.Fraction) * float ticksPerDay))

        DateTimeOffset(days * ticksPerDay + withinDay, TimeSpan.Zero)

    /// Seconds since the Unix epoch.
    let toUnixSeconds (instant: Instant) : float<s> =
        Instant.difference instant unixOrigin * Time.secondsPerDay

    /// Instant of a count of seconds since the Unix epoch.
    let ofUnixSeconds (seconds: float<s>) : Instant =
        Instant.add (seconds / Time.secondsPerDay) unixOrigin

    // -----------------------------------------------------------------------------------------
    // Sidereal time. These take UT1, the time scale tied to the actual rotation
    // of the Earth. Feeding them UTC is good to within the 0.9 s that DUT1 is
    // kept below, which is a quarter of an arcminute of hour angle, and is fine
    // for pointing but not for astrometry.
    // -----------------------------------------------------------------------------------------

    /// Ratio of a mean solar day to the Earth's rotation period measured
    /// against the fixed stars, the rate of the IAU 2000 expression for the
    /// Earth rotation angle.
    let stellarPerSolar: float = 1.00273781191135448

    /// Length of the mean stellar day in SI seconds, about 86164.0989 s. This
    /// is the Earth's rotation period proper.
    let meanStellarDay: float<s> = Time.secondsPerDay * 1.0<d> / stellarPerSolar

    /// Coefficients of the IAU 2006 polynomial for the precession in right
    /// ascension accumulated since J2000, in arcseconds by ascending power of
    /// Julian centuries (Capitaine, Wallace and Chapront 2003). Added to the
    /// Earth rotation angle it gives Greenwich mean sidereal time.
    let private precessionSeries = [| 0.014506; 4612.156534; 1.3915817; -0.00000044; -0.000029956; -0.0000000368 |]

    /// Ratio of a mean solar day to the mean sidereal day, the rate at J2000 of
    /// the IAU 2006 expression for Greenwich mean sidereal time: the rotation
    /// rate plus the 4612 arcseconds a century of precession in right
    /// ascension. Slightly larger than `stellarPerSolar`: sidereal time is
    /// measured from the equinox, and the equinox precesses westward, so the
    /// Earth returns to it a little before it returns to a fixed star.
    let siderealPerSolar: float = stellarPerSolar + 4612.156534 / 1296000.0 / 36525.0

    /// Length of the mean sidereal day in SI seconds, about 86164.0905 s. Eight
    /// milliseconds shorter than `meanStellarDay`, which is the precession in a
    /// day.
    let meanSiderealDay: float<s> = Time.secondsPerDay * 1.0<d> / siderealPerSolar

    /// Earth rotation angle, the IAU 2000 replacement for Greenwich sidereal
    /// time. A linear function of UT1 by construction, wrapped into `[0, 2
    /// pi)`.
    let earthRotationAngle (ut1: Instant) : float<rad> =
        let days = Instant.difference ut1 j2000 / 1.0<d>
        let turns = 0.7790572732640 + stellarPerSolar * days
        Angle.wrap (2.0 * Math.PI * 1.0<rad>) (2.0 * Math.PI * turns * 1.0<rad>)

    /// Greenwich mean sidereal time from UT1, by the IAU 2006 expression: the
    /// Earth rotation angle plus the precession in right ascension accumulated
    /// since J2000. Wrapped into `[0h, 24h)`. "Mean" is without the nutation in
    /// longitude; adding the equation of the equinoxes gives apparent sidereal
    /// time.
    ///
    /// The polynomial's argument is strictly TT rather than UT1. No time scale
    /// is modelled here, so the one instant serves for both, which moves the
    /// result by a tenth of a milliarcsecond: the polynomial grows by 4612
    /// arcseconds a century and TT leads UT1 by about a minute.
    let greenwichMeanSiderealTime (ut1: Instant) : float<hourangle> =
        let precession =
            Polynomial.horner precessionSeries (julianCenturies ut1) * 1.0<arcsec>
            |> convert Angle.radiansPerArcsecond Angle.radiansPerHourAngle

        Angle.wrap 24.0<hourangle> (earthRotationAngle ut1 / Angle.radiansPerHourAngle + precession)

    /// Mean obliquity of the ecliptic, the tilt of the Earth's axis, by the IAU
    /// 1980 polynomial. About 23.4393 degrees and shrinking by 47 arcseconds a
    /// century.
    let meanObliquity (instant: Instant) : float<rad> =
        let t = julianCenturies instant

        let arcseconds = 84381.448 - 46.8150 * t - 0.00059 * t * t + 0.001813 * t * t * t

        arcseconds * 1.0<arcsec> * Angle.radiansPerArcsecond

    /// Equation of the equinoxes, the nutation of the equinox expressed as a
    /// time. The difference between apparent and mean sidereal time, never more
    /// than about a second.
    ///
    /// This is the two term approximation rather than the full nutation series,
    /// so it is good to about 0.1 s of time, or an arcsecond and a half of hour
    /// angle. That is ample for pointing and not enough for astrometry.
    let equationOfTheEquinoxes (instant: Instant) : float<hourangle> =
        let days = Instant.difference instant j2000 / 1.0<d>

        let toRadians (degrees: float) =
            Angle.wrap (2.0 * Math.PI * 1.0<rad>) (degrees * 1.0<deg> * Angle.radiansPerDegree)

        // Longitude of the ascending node of the Moon's mean orbit, and the
        // Sun's mean longitude.
        let node = toRadians (125.04 - 0.052954 * days)
        let sun = toRadians (280.47 + 0.98565 * days)

        let nutationInLongitude =
            (-0.000319 * Angle.sin node - 0.000024 * Angle.sin (2.0 * sun)) * 1.0<hourangle>

        nutationInLongitude * Angle.cos (meanObliquity instant)

    /// Greenwich apparent sidereal time, the mean sidereal time plus the
    /// equation of the equinoxes. The hour angle of the true equinox, which is
    /// what a telescope tracks.
    let greenwichApparentSiderealTime (ut1: Instant) : float<hourangle> =
        Angle.wrap 24.0<hourangle> (greenwichMeanSiderealTime ut1 + equationOfTheEquinoxes ut1)

    /// Local mean sidereal time at a longitude measured positive east of
    /// Greenwich.
    let localMeanSiderealTime (eastLongitude: float<deg>) (ut1: Instant) : float<hourangle> =
        let offset = convert Angle.radiansPerDegree Angle.radiansPerHourAngle eastLongitude

        Angle.wrap 24.0<hourangle> (greenwichMeanSiderealTime ut1 + offset)

    /// Local apparent sidereal time at a longitude measured positive east of
    /// Greenwich.
    let localApparentSiderealTime (eastLongitude: float<deg>) (ut1: Instant) : float<hourangle> =
        Angle.wrap 24.0<hourangle> (localMeanSiderealTime eastLongitude ut1 + equationOfTheEquinoxes ut1)

    /// Hour angle of a source at the given local sidereal time, LST - RA,
    /// wrapped into `[-12h, 12h)` so that negative is east of the meridian and
    /// positive is west.
    let hourAngle (siderealTime: float<hourangle>) (rightAscension: float<hourangle>) : float<hourangle> =
        Angle.wrapSigned 24.0<hourangle> (siderealTime - rightAscension)
