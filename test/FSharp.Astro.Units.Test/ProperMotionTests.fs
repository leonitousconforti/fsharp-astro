module ProperMotionTests

open System
open Xunit
open FsCheck.FSharp
open FSharp.Astro.Units
open Helpers

let private deg (x: float) : float<rad> = x * 1.0<deg> * Angle.radiansPerDegree

let private masPerYear (x: float) : float<rad / yr> =
    x * 1.0<mas / yr> * Angle.radiansPerMilliarcsecond

[<Fact>]
let ``proper motion points where its components say`` () =
    // Due north, due east, due south, due west.
    close 1e-12 0.0<rad> (ProperMotion.positionAngle (masPerYear 0.0) (masPerYear 1.0))
    close 1e-12 (deg 90.0) (ProperMotion.positionAngle (masPerYear 1.0) (masPerYear 0.0))
    close 1e-12 (deg 180.0) (ProperMotion.positionAngle (masPerYear 0.0) (masPerYear -1.0))
    close 1e-12 (deg 270.0) (ProperMotion.positionAngle (masPerYear -1.0) (masPerYear 0.0))

[<Fact>]
let ``proper motion moves a source by its total times the interval`` () =
    check (
        forAll
            (Gen.zip (Gen.choose (-500, 500)) (Gen.choose (-500, 500)))
            (fun (a, b) ->
                let muLon = masPerYear (float a)
                let muLat = masPerYear (float b)
                let interval = 50.0<yr>
                let lon, lat = ProperMotion.apply interval (deg 45.0) (deg 30.0) muLon muLat
                let moved = Spherical.separation (deg 45.0) (deg 30.0) lon lat
                // Absolute, not relative: the error is an ulp of the longitude the offset was
                // added to, which is a fixed 4e-16 rad however short the arc is.
                abs (moved - ProperMotion.total muLon muLat * interval) < 1e-14<rad>
            )
    )

[<Fact>]
let ``a negative interval moves the same distance the other way`` () =
    check (
        forAll
            (Gen.zip (Gen.choose (-2000, 2000)) (Gen.choose (-2000, 2000)))
            (fun (a, b) ->
                let muLon = masPerYear (float a)
                let muLat = masPerYear (float b)
                let lon, lat = deg 200.0, deg -40.0
                let arc = ProperMotion.total muLon muLat * 75.0<yr>
                let aheadLon, aheadLat = ProperMotion.apply 75.0<yr> lon lat muLon muLat
                let behindLon, behindLat = ProperMotion.apply -75.0<yr> lon lat muLon muLat

                // Equidistant from the start, on opposite sides, and so twice as far apart.
                abs (Spherical.separation lon lat aheadLon aheadLat - arc) < 1e-14<rad>
                && abs (Spherical.separation lon lat behindLon behindLat - arc) < 1e-14<rad>
                && abs (Spherical.separation aheadLon aheadLat behindLon behindLat - 2.0 * arc) < 2e-14<rad>
            )
    )

[<Fact>]
let ``transporting back with the same components does not quite return the source`` () =
    // The components are tied to the local north, which turns as the source moves, so the
    // backward step runs along a slightly different great circle. The gap is the convergence of
    // the meridians over the arc, not a defect in the transport.
    let muLon = masPerYear 2000.0
    let muLat = masPerYear 2000.0
    let lon, lat = deg 200.0, deg -40.0
    let aheadLon, aheadLat = ProperMotion.apply 75.0<yr> lon lat muLon muLat
    let backLon, backLat = ProperMotion.apply -75.0<yr> aheadLon aheadLat muLon muLat
    let gap = Spherical.separation lon lat backLon backLat
    Assert.InRange(float gap, 1e-9, 1e-5)

    // Straight north is a meridian, the one path whose azimuth does not change along the way,
    // so there the round trip is exact wherever it starts.
    let northLon, northLat = ProperMotion.apply 75.0<yr> lon lat (masPerYear 0.0) muLat

    let backNorthLon, backNorthLat =
        ProperMotion.apply -75.0<yr> northLon northLat (masPerYear 0.0) muLat

    Assert.True(Spherical.separation lon lat backNorthLon backNorthLat < 1e-14<rad>)

[<Fact>]
let ``Barnard's star moves a quarter of a degree in a century`` () =
    // The largest proper motion known, 10.3 arcsec a year, almost due north.
    let muLon = -798.58<mas / yr> * Angle.radiansPerMilliarcsecond
    let muLat = 10328.12<mas / yr> * Angle.radiansPerMilliarcsecond
    let total = ProperMotion.total muLon muLat
    close 1e-12 (10358.947473117141<mas / yr> * Angle.radiansPerMilliarcsecond) total

    let lon, lat =
        ProperMotion.atEpoch 2000.0<jyear> 2100.0<jyear> (deg 269.45) (deg 4.69) muLon muLat

    let moved = Spherical.separation (deg 269.45) (deg 4.69) lon lat
    close 1e-9 (0.2877485409199206<deg> * Angle.radiansPerDegree) moved

[<Fact>]
let ``proper motion past the pole stays on the sphere`` () =
    // Five degrees short of the pole, moving north a degree a year for ten years.
    let mu = 1.0<deg / yr> * Angle.radiansPerDegree
    let lon, lat = ProperMotion.apply 10.0<yr> (deg 0.0) (deg 85.0) (masPerYear 0.0) mu
    // Over the top and five degrees down the other side.
    close 1e-12 (deg 85.0) lat
    close 1e-12 (deg 180.0) lon

[<Fact>]
let ``tangential velocity is 4.74 km per second per arcsecond per year per parsec`` () =
    let motion = 1.0<arcsec / yr> * Angle.radiansPerArcsecond
    let velocity = ProperMotion.tangentialVelocity 1.0<pc> motion
    close 1e-12 (4.740470463533348<km / s> * Length.metersPerKilometer) velocity
