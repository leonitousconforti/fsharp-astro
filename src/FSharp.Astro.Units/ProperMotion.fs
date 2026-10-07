namespace FSharp.Astro.Units

/// Proper motion: the apparent angular drift of a source across the sky, and the position it
/// carries the source to over an epoch gap.
///
/// The component along the longitude is the catalogue one, `mu_alpha* = mu_alpha cos delta`,
/// already multiplied by the cosine of the latitude. That is what Hipparcos, Gaia and every
/// modern catalogue tabulate, and it is an angular rate on the sky rather than a rate of change
/// of a coordinate, so the two components can be squared and added.
///
/// Positions are transported along the great circle the motion points down rather than by adding
/// to each coordinate. The two agree to a part in `(mu dt)^2` over short gaps, but only the great
/// circle stays correct near the pole, where a coordinate increment diverges.
///
/// The components are tied to the local north, which rotates as the source moves, so they are
/// only valid at the position they were measured at. Transporting a source forward and then back
/// with the same pair of components does not return it exactly to where it started: the error is
/// the convergence of the meridians over the arc, about `(mu dt)^2 tan(latitude)`, which is half
/// an arcsecond for the fastest star over a century and negligible for anything else. Recomputing
/// the components at the new position is what a rigorous reduction does.
///
/// Radial velocity is not modelled, so there is no perspective acceleration: a source approaching
/// us has a proper motion that grows, and over a century that matters for the few dozen nearest
/// stars. Parallax and aberration are not modelled either; these are barycentric positions.
[<RequireQualifiedAccess>]
module ProperMotion =

    /// Total proper motion from its two components, the length of the vector they form.
    let total (alongLongitude: float<rad / yr>) (alongLatitude: float<rad / yr>) : float<rad / yr> =
        sqrt (alongLongitude * alongLongitude + alongLatitude * alongLatitude)

    /// Position angle the motion points along, east of north, wrapped into `[0, 2 pi)`.
    let positionAngle (alongLongitude: float<rad / yr>) (alongLatitude: float<rad / yr>) : float<rad> =
        Angle.wrap (2.0 * System.Math.PI * 1.0<rad>) (Angle.atan2 alongLongitude alongLatitude)

    /// Position of a source after an interval, transported along the great circle its motion
    /// points down. Returns the new longitude and latitude.
    let apply
        (interval: float<yr>)
        (longitude: float<rad>)
        (latitude: float<rad>)
        (alongLongitude: float<rad / yr>)
        (alongLatitude: float<rad / yr>)
        : float<rad> * float<rad> =
        let distance = total alongLongitude alongLatitude * interval
        let direction = positionAngle alongLongitude alongLatitude

        // A negative interval runs the motion backwards, which is a positive distance in the
        // opposite direction rather than a negative separation.
        if distance < 0.0<rad> then
            Spherical.offset
                longitude
                latitude
                (Angle.wrap (2.0 * System.Math.PI * 1.0<rad>) (direction + System.Math.PI * 1.0<rad>))
                -distance
        else
            Spherical.offset longitude latitude direction distance

    /// Position of a source at one Julian epoch given its position at another. A `jyear` is
    /// exactly a Julian year, so the gap is the difference.
    let atEpoch
        (from: float<jyear>)
        (into: float<jyear>)
        (longitude: float<rad>)
        (latitude: float<rad>)
        (alongLongitude: float<rad / yr>)
        (alongLatitude: float<rad / yr>)
        : float<rad> * float<rad> =
        apply ((into - from) * 1.0<yr / jyear>) longitude latitude alongLongitude alongLatitude

    /// Velocity across the line of sight of a source at a distance, `v = mu d`. The classic
    /// 4.74 km/s per arcsecond per year per parsec falls out of the units.
    let tangentialVelocity (distance: float<pc>) (motion: float<rad / yr>) : float<m / s> =
        motion / 1.0<rad> * (distance * Length.metersPerParsec) / Time.secondsPerYear

    /// Proper motion of a source at a distance moving across the line of sight at a velocity, the
    /// inverse of `tangentialVelocity`.
    let ofTangentialVelocity (distance: float<pc>) (velocity: float<m / s>) : float<rad / yr> =
        velocity * 1.0<rad> * Time.secondsPerYear / (distance * Length.metersPerParsec)
