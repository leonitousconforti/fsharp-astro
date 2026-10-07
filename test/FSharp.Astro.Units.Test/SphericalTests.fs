module SphericalTests

open System
open Xunit
open FsCheck
open FsCheck.FSharp
open FSharp.Astro.Units
open Helpers

let private deg (x: float) : float<rad> = x * 1.0<deg> * Angle.radiansPerDegree
let private inDegrees (x: float<rad>) : float<deg> = x / Angle.radiansPerDegree

/// Longitudes and latitudes spread over the whole sphere.
let private points: Gen<float<rad> * float<rad>> = gen {
    let! longitude = Gen.choose (0, 359_999) |> Gen.map (fun n -> deg (float n / 1000.0))
    let! latitude =
        Gen.choose (-89_999, 89_999) |> Gen.map (fun n -> deg (float n / 1000.0))
    return longitude, latitude
}

[<Fact>]
let ``a point is no distance from itself`` () =
    close 1e-15 0.0<rad> (Spherical.separation (deg 10.0) (deg 20.0) (deg 10.0) (deg 20.0))

[<Fact>]
let ``separation along the equator is the difference in longitude`` () =
    close 1e-12 (deg 90.0) (Spherical.separation (deg 0.0) (deg 0.0) (deg 90.0) (deg 0.0))

[<Fact>]
let ``separation along a meridian is the difference in latitude`` () =
    close 1e-12 (deg 50.0) (Spherical.separation (deg 42.0) (deg -20.0) (deg 42.0) (deg 30.0))

[<Fact>]
let ``the poles are half a turn apart`` () =
    close 1e-12 (deg 180.0) (Spherical.separation (deg 0.0) (deg 90.0) (deg 123.0) (deg -90.0))

[<Fact>]
let ``a small separation keeps its precision`` () =
    // The spherical law of cosines would return a separation this small to about five
    // significant figures, because cos d differs from one by 1e-17 here. This returns all of it.
    //
    // The second latitude is the separation itself rather than an offset from a larger angle: a
    // double cannot hold "45 degrees plus a milliarcsecond" to better than a part in 1e8, so
    // writing it that way would measure the rounding of the input instead of the formula.
    let mas = 1.0<mas> * Angle.radiansPerMilliarcsecond
    close 1e-15 mas (Spherical.separation (deg 30.0) 0.0<rad> (deg 30.0) mas)

    let uas = 1.0<uas> * Angle.radiansPerMicroarcsecond
    close 1e-15 uas (Spherical.separation (deg 30.0) 0.0<rad> (deg 30.0) uas)

[<Fact>]
let ``separation is symmetric`` () =
    check (
        forAll
            (Gen.zip points points)
            (fun ((lon1, lat1), (lon2, lat2)) ->
                within 1e-12 (Spherical.separation lon1 lat1 lon2 lat2) (Spherical.separation lon2 lat2 lon1 lat1)
            )
    )

[<Fact>]
let ``separation lies between zero and half a turn`` () =
    check (
        forAll
            (Gen.zip points points)
            (fun ((lon1, lat1), (lon2, lat2)) ->
                let d = Spherical.separation lon1 lat1 lon2 lat2
                d >= 0.0<rad> && d <= Math.PI * 1.0<rad>
            )
    )

[<Fact>]
let ``the separation of three points obeys the triangle inequality`` () =
    check (
        forAll
            (Gen.zip3 points points points)
            (fun ((lon1, lat1), (lon2, lat2), (lon3, lat3)) ->
                let a = Spherical.separation lon1 lat1 lon2 lat2
                let b = Spherical.separation lon2 lat2 lon3 lat3
                let c = Spherical.separation lon1 lat1 lon3 lat3
                c <= a + b + 1e-12<rad>
            )
    )

[<Fact>]
let ``position angle is north, east, south and west in turn`` () =
    let lon, lat = deg 0.0, deg 0.0
    close 1e-12 0.0<deg> (inDegrees (Spherical.positionAngle lon lat (deg 0.0) (deg 10.0)))
    close 1e-12 90.0<deg> (inDegrees (Spherical.positionAngle lon lat (deg 10.0) (deg 0.0)))
    close 1e-12 180.0<deg> (inDegrees (Spherical.positionAngle lon lat (deg 0.0) (deg -10.0)))
    close 1e-12 270.0<deg> (inDegrees (Spherical.positionAngle lon lat (deg -10.0) (deg 0.0)))

[<Fact>]
let ``offset inverts separation and position angle`` () =
    check (
        forAll
            (Gen.zip points points)
            (fun ((lon1, lat1), (lon2, lat2)) ->
                let d = Spherical.separation lon1 lat1 lon2 lat2
                let pa = Spherical.positionAngle lon1 lat1 lon2 lat2
                let lon, lat = Spherical.offset lon1 lat1 pa d
                // Compare as a separation rather than coordinate by coordinate: longitude is
                // degenerate at the pole, and the point is what matters.
                Spherical.separation lon lat lon2 lat2 < 1e-9<rad>
            )
    )

[<Fact>]
let ``offset moves exactly as far as asked`` () =
    check (
        forAll
            (Gen.zip points (Gen.zip (Gen.choose (0, 359)) (Gen.choose (1, 17_900))))
            (fun ((lon, lat), (pa, d)) ->
                let distance = deg (float d / 100.0)
                let lon2, lat2 = Spherical.offset lon lat (deg (float pa)) distance
                within 1e-9 distance (Spherical.separation lon lat lon2 lat2)
            )
    )

[<Fact>]
let ``offset crosses the pole correctly`` () =
    // Starting ten degrees short of the pole and moving twenty degrees north goes over the top
    // and back down the far side, which a coordinate increment would get wrong.
    let lon, lat = Spherical.offset (deg 0.0) (deg 80.0) (deg 0.0) (deg 20.0)
    close 1e-12 80.0<deg> (inDegrees lat)
    close 1e-12 180.0<deg> (inDegrees lon)

[<Fact>]
let ``the midpoint is equidistant from both ends`` () =
    check (
        forAll
            (Gen.zip points points)
            (fun ((lon1, lat1), (lon2, lat2)) ->
                let lon, lat = Spherical.midpoint lon1 lat1 lon2 lat2
                let a = Spherical.separation lon1 lat1 lon lat
                let b = Spherical.separation lon lat lon2 lat2
                abs (a - b) < 1e-9<rad>
            )
    )

[<Fact>]
let ``a cap of half a turn is the whole sphere`` () =
    close 1e-15 Spherical.fullSphere (Spherical.capSolidAngle (Math.PI * 1.0<rad>))
    close 1e-15 (Spherical.fullSphere / 2.0) (Spherical.capSolidAngle (Math.PI / 2.0 * 1.0<rad>))
    close 1e-15 0.0<sr> (Spherical.capSolidAngle 0.0<rad>)

[<Fact>]
let ``a small cap is pi r squared`` () =
    // The exact cap is pi r^2 (1 - r^2/12 + ...), so at an arcminute the two differ by 7e-9.
    let radius = 1.0<arcmin> * Angle.radiansPerArcminute
    close 1e-8 (Math.PI * radius * radius * 1.0<sr / rad^2>) (Spherical.capSolidAngle radius)
    let ratio =
        Spherical.capSolidAngle radius / (Math.PI * radius * radius * 1.0<sr / rad^2>)
    close 1e-3 (1.0 - float (radius * radius) / 12.0) ratio

[<Fact>]
let ``cap radius inverts cap solid angle`` () =
    // Up to a half turn, where the inversion is ill conditioned and keeps about eleven digits.
    check (
        forAll
            (Gen.choose (1, 180_000) |> Gen.map (fun n -> deg (float n / 1000.0)))
            (fun radius -> within 1e-10 radius (Spherical.capRadius (Spherical.capSolidAngle radius)))
    )

    // Up to a quarter turn, which is every aperture anyone has, it is exact.
    check (
        forAll
            (Gen.choose (1, 90_000) |> Gen.map (fun n -> deg (float n / 1000.0)))
            (fun radius -> within 1e-14 radius (Spherical.capRadius (Spherical.capSolidAngle radius)))
    )

[<Fact>]
let ``cap radius survives a solid angle at or past the whole sphere`` () =
    close 1e-15 (Math.PI * 1.0<rad>) (Spherical.capRadius Spherical.fullSphere)
    // Rounding can push a computed solid angle a hair over 4 pi; that must not come back NaN.
    close 1e-15 (Math.PI * 1.0<rad>) (Spherical.capRadius (Spherical.fullSphere * 1.0000001))
    close 1e-15 0.0<rad> (Spherical.capRadius -1.0<sr>)

[<Fact>]
let ``a band between the poles over all longitudes is the whole sphere`` () =
    let whole =
        Spherical.bandSolidAngle (2.0 * Math.PI * 1.0<rad>) (deg -90.0) (deg 90.0)

    close 1e-15 Spherical.fullSphere whole

[<Fact>]
let ``a square degree is the same either way round`` () =
    // A band one degree wide in longitude and one in latitude about the equator, against the
    // product of two degrees in radians.
    let side = deg 1.0
    let band = Spherical.bandSolidAngle side (deg -0.5) (deg 0.5)
    close 1e-4 (side * side * 1.0<sr / rad^2>) band
