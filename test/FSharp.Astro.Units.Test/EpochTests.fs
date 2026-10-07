module EpochTests

open System
open Xunit
open FsCheck
open FsCheck.FSharp
open FSharp.Astro.Units
open Helpers

/// Instants scattered a few thousand years either side of J2000.
let private instants: Gen<Instant> =
    Gen.choose (-2_000_000, 2_000_000)
    |> Gen.map (fun n -> Instant.add (float n / 1000.0 * 1.0<d>) Epoch.j2000)

/// Asserts that two instants are the same to within a tolerance in days.
let private sameInstant (tolerance: float<d>) (expected: Instant) (actual: Instant) =
    let gap = abs (Instant.difference actual expected)
    Assert.True(gap <= tolerance, $"instants differ by {float gap} d, more than {float tolerance}")

[<Fact>]
let ``the Modified Julian Date drops the leading digits and the half day`` () =
    close 1e-15 51544.5<mjd> (Epoch.toMjd Epoch.j2000)
    sameInstant 1e-12<d> Epoch.j2000 (Epoch.ofMjd 51544.5<mjd>)
    close 1e-15 0.0<mjd> (Epoch.toMjd Epoch.mjdOrigin)

[<Fact>]
let ``the Modified Julian Date origin is a whole day before noon`` () =
    close 1e-15 -0.5<d> (Instant.difference Epoch.mjdOrigin (Instant.ofJd 2400001.0<jd>))

[<Fact>]
let ``B1950 is the Besselian epoch 1950`` () =
    close 1e-9 1950.0<byear> (Epoch.toBesselianEpoch Epoch.b1950)
    // The conventional B1950.0 Julian Date and the Newcomb formula disagree by about 3.5 s, which
    // is a rounding carried in the conventional value rather than a difference of definition.
    sameInstant 5e-5<d> Epoch.b1950 (Epoch.ofBesselianEpoch 1950.0<byear>)

[<Fact>]
let ``epoch conversions round trip`` () =
    check (
        forAll
            instants
            (fun instant ->
                let viaMjd = Epoch.ofMjd (Epoch.toMjd instant)
                let viaJulian = Epoch.ofJulianEpoch (Epoch.toJulianEpoch instant)
                let viaBesselian = Epoch.ofBesselianEpoch (Epoch.toBesselianEpoch instant)

                abs (Instant.difference viaMjd instant) < 1e-9<d>
                && abs (Instant.difference viaJulian instant) < 1e-9<d>
                && abs (Instant.difference viaBesselian instant) < 1e-9<d>
            )
    )

[<Fact>]
let ``a Modified Julian Date needs both parts to carry a microsecond`` () =
    let microsecond = 1.0 / 86400.0 / 1e6 * 1.0<mjd>
    let a = Epoch.ofMjd 51544.5<mjd>

    // One double cannot do it: the spacing of doubles at 51544.5 is 630 ns, so a microsecond
    // added to the literal survives only to about one bit.
    let viaOneDouble =
        Instant.difference (Epoch.ofMjd (51544.5<mjd> + microsecond)) a
        * Time.secondsPerDay
        * 1e6

    Assert.True(
        abs (viaOneDouble - 1.0<s>) > 0.1<s>,
        $"expected the microsecond to be mangled, got {float viaOneDouble}"
    )

    // Two parts carry it intact.
    let viaTwoParts =
        Instant.difference (Epoch.ofMjdParts 51544.5<mjd> microsecond) a
        * Time.secondsPerDay
        * 1e6

    Assert.True(abs (viaTwoParts - 1.0<s>) < 1e-6<s>, $"gap was {float viaTwoParts} us")

[<Fact>]
let ``the Unix epoch is JD 2440587.5`` () =
    let unix = DateTimeOffset(1970, 1, 1, 0, 0, 0, TimeSpan.Zero)
    sameInstant 1e-15<d> (Instant.ofJd 2440587.5<jd>) (Epoch.ofDateTimeOffset unix)
    close 1e-15 0.0<s> (Epoch.toUnixSeconds Epoch.unixOrigin)

[<Fact>]
let ``J2000 is noon on the first of January 2000`` () =
    let noon = DateTimeOffset(2000, 1, 1, 12, 0, 0, TimeSpan.Zero)
    sameInstant 1e-15<d> Epoch.j2000 (Epoch.ofDateTimeOffset noon)
    Assert.Equal(noon, Epoch.toDateTimeOffset Epoch.j2000)

[<Fact>]
let ``calendar instants round trip to the tick`` () =
    // 100 nanosecond ticks, exactly, rather than the millisecond a single-double Julian Date
    // would have forced.
    check (
        forAll
            (Gen.choose (-2_000_000_000, 2_000_000_000))
            (fun seconds ->
                let moment =
                    DateTimeOffset(1970, 1, 1, 0, 0, 0, TimeSpan.Zero).AddSeconds(float seconds).AddTicks(1234567L)

                Epoch.toDateTimeOffset (Epoch.ofDateTimeOffset moment) = moment
            )
    )

[<Fact>]
let ``Greenwich mean sidereal time at J2000 is 18h 41m 50.55s`` () =
    // The Earth rotation angle at J2000, 0.7790572732640 turns, plus the constant term of the IAU
    // 2006 precession polynomial, 0.014506 arcseconds.
    let expected = (0.7790572732640 * 24.0 + 0.014506 / 15.0 / 3600.0) * 1.0<hourangle>

    close 1e-12 expected (Epoch.greenwichMeanSiderealTime Epoch.j2000)
    close 1e-9 18.69737483<hourangle> (Epoch.greenwichMeanSiderealTime Epoch.j2000)

[<Fact>]
let ``sidereal time advances by a sidereal day in a solar day`` () =
    let first = Epoch.greenwichMeanSiderealTime Epoch.j2000
    let second = Epoch.greenwichMeanSiderealTime (Instant.add 1.0<d> Epoch.j2000)
    let gained = Angle.wrap 24.0<hourangle> (second - first)
    // A solar day is one sidereal day plus about 3m 56s of sidereal time.
    close 1e-6 (24.0<hourangle> * (Epoch.siderealPerSolar - 1.0)) gained

[<Fact>]
let ``sidereal time stays in range`` () =
    check (
        forAll
            instants
            (fun instant ->
                let t = Epoch.greenwichMeanSiderealTime instant
                t >= 0.0<hourangle> && t < 24.0<hourangle>
            )
    )

[<Fact>]
let ``longitude shifts local sidereal time by an hour every fifteen degrees`` () =
    let greenwich = Epoch.greenwichMeanSiderealTime Epoch.j2000
    let local = Epoch.localMeanSiderealTime 15.0<deg> Epoch.j2000
    close 1e-12 1.0<hourangle> (Angle.wrap 24.0<hourangle> (local - greenwich))

[<Fact>]
let ``the Earth rotation angle at J2000 is 280.46 degrees`` () =
    let era = Epoch.earthRotationAngle Epoch.j2000 / Angle.radiansPerDegree
    close 1e-9 280.46061837<deg> era

[<Fact>]
let ``sidereal time resolves a millisecond of UT1`` () =
    // The whole point of the two-part instant: a millisecond apart at a Julian Date of two and a
    // half million still comes through as a millisecond of sidereal time.
    let millisecond = 1.0<s> / Time.secondsPerDay
    let a = Epoch.greenwichMeanSiderealTime Epoch.j2000

    let b =
        Epoch.greenwichMeanSiderealTime (Instant.add (millisecond / 1000.0) Epoch.j2000)

    let gained = (b - a) * 3600.0 * 1000.0 // milliseconds of sidereal time
    close 1e-3 (Epoch.siderealPerSolar * 1.0<hourangle>) gained

[<Fact>]
let ``a mean sidereal day is 86164.0905 seconds`` () =
    close 1e-9 86164.0905308<s> Epoch.meanSiderealDay
    // The stellar day is eight milliseconds longer: the equinox precesses to meet the Earth.
    close 1e-12 86164.09890369035<s> Epoch.meanStellarDay
    close 1e-6 8.372357e-3<s> (Epoch.meanStellarDay - Epoch.meanSiderealDay)

[<Fact>]
let ``the hour angle is zero on the meridian and signed either side`` () =
    close 1e-15 0.0<hourangle> (Epoch.hourAngle 6.0<hourangle> 6.0<hourangle>)
    close 1e-15 -1.0<hourangle> (Epoch.hourAngle 6.0<hourangle> 7.0<hourangle>)
    close 1e-15 1.0<hourangle> (Epoch.hourAngle 0.0<hourangle> 23.0<hourangle>)

[<Fact>]
let ``the mean obliquity is 23.4393 degrees at J2000`` () =
    let obliquity = Epoch.meanObliquity Epoch.j2000 / Angle.radiansPerDegree
    close 1e-6 23.439291111111113<deg> obliquity

[<Fact>]
let ``the obliquity shrinks by 47 arcseconds a century`` () =
    let now = Epoch.meanObliquity Epoch.j2000
    let later = Epoch.meanObliquity (Instant.add 36525.0<d> Epoch.j2000)
    let change = (later - now) / Angle.radiansPerArcsecond
    close 1e-3 -46.8156<arcsec> change

[<Fact>]
let ``the equation of the equinoxes never exceeds a second and a half`` () =
    check (forAll instants (fun instant -> abs (Epoch.equationOfTheEquinoxes instant) * 3600.0 < 1.5<hourangle>))
