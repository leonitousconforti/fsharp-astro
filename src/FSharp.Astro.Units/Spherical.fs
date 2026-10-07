namespace FSharp.Astro.Units

open System

/// Geometry on the celestial sphere: separations, position angles, and the point a given offset
/// away from another.
///
/// Every angle here is in radians, because these are trigonometry and not bookkeeping. Convert on
/// the way in and out with `Angle.convert`. The first coordinate is a longitude measured eastward
/// and the second a latitude measured from the equator: right ascension and declination, galactic
/// longitude and latitude, ecliptic longitude and latitude. Nothing here knows which frame it is,
/// and nothing here transforms between frames.
///
/// The sphere is a unit sphere, so a separation is an angle and never a length.
[<RequireQualifiedAccess>]
module Spherical =

    /// Angular separation between two points, by the Vincenty formula.
    ///
    /// The obvious spherical law of cosines, `cos d = sin a sin b + cos a cos b cos dlon`, loses
    /// half its significant figures on the separations astronomy cares about most: at an
    /// arcsecond, `cos d` differs from one by 1e-11, so a double keeps about five digits of the
    /// answer. The haversine form fixes that but degrades for nearly antipodal points. Vincenty is
    /// an `atan2` of two well conditioned quantities and is accurate at both ends.
    let separation
        (longitude1: float<rad>)
        (latitude1: float<rad>)
        (longitude2: float<rad>)
        (latitude2: float<rad>)
        : float<rad> =
        let dLongitude = longitude2 - longitude1
        let sinDLongitude = Angle.sin dLongitude
        let cosDLongitude = Angle.cos dLongitude
        let sin1, cos1 = Angle.sin latitude1, Angle.cos latitude1
        let sin2, cos2 = Angle.sin latitude2, Angle.cos latitude2

        let east = cos2 * sinDLongitude
        let north = cos1 * sin2 - sin1 * cos2 * cosDLongitude
        let along = sin1 * sin2 + cos1 * cos2 * cosDLongitude

        Math.Atan2(sqrt (east * east + north * north), along) * 1.0<rad>

    /// Position angle of the second point as seen from the first, measured east of north and
    /// wrapped into `[0, 2 pi)`. The convention binary star and proper motion catalogues use.
    let positionAngle
        (longitude1: float<rad>)
        (latitude1: float<rad>)
        (longitude2: float<rad>)
        (latitude2: float<rad>)
        : float<rad> =
        let dLongitude = longitude2 - longitude1
        let sin1, cos1 = Angle.sin latitude1, Angle.cos latitude1
        let sin2, cos2 = Angle.sin latitude2, Angle.cos latitude2

        let east = cos2 * Angle.sin dLongitude
        let north = cos1 * sin2 - sin1 * cos2 * Angle.cos dLongitude

        Angle.wrap (2.0 * Math.PI * 1.0<rad>) (Math.Atan2(east, north) * 1.0<rad>)

    /// The point a given separation away from another along a given position angle, east of north.
    /// The inverse of `separation` and `positionAngle` taken together, and the right way to move a
    /// position across the sphere: it stays correct at the pole, where adding to the longitude
    /// does not.
    let offset
        (longitude: float<rad>)
        (latitude: float<rad>)
        (positionAngle: float<rad>)
        (separation: float<rad>)
        : float<rad> * float<rad> =
        let sinLat, cosLat = Angle.sin latitude, Angle.cos latitude
        let sinSep, cosSep = Angle.sin separation, Angle.cos separation

        let sinNew = sinLat * cosSep + cosLat * sinSep * Angle.cos positionAngle
        // Math.Clamp rather than min and max: `min` the function is shadowed inside this
        // namespace by `min` the measure, the minute.
        let newLatitude = Angle.asin (Math.Clamp(sinNew, -1.0, 1.0))

        let east = Angle.sin positionAngle * sinSep * cosLat
        let north = cosSep - sinLat * sinNew
        let newLongitude = longitude + Math.Atan2(east, north) * 1.0<rad>

        Angle.wrap (2.0 * Math.PI * 1.0<rad>) newLongitude, newLatitude

    /// Midpoint of the great circle arc between two points.
    let midpoint
        (longitude1: float<rad>)
        (latitude1: float<rad>)
        (longitude2: float<rad>)
        (latitude2: float<rad>)
        : float<rad> * float<rad> =
        let half = separation longitude1 latitude1 longitude2 latitude2 / 2.0
        offset longitude1 latitude1 (positionAngle longitude1 latitude1 longitude2 latitude2) half

    /// Solid angle of a spherical cap of the given angular radius, `2 pi (1 - cos r)`. The solid
    /// angle of a circular aperture or a field of view.
    let capSolidAngle (radius: float<rad>) : float<sr> =
        // The versine written as a half-angle sine, so that a small radius keeps its precision.
        let sinHalf = Angle.sin (radius / 2.0)
        4.0 * Math.PI * sinHalf * sinHalf * 1.0<sr>

    /// Angular radius of a spherical cap of the given solid angle, the inverse of `capSolidAngle`.
    ///
    /// Ill conditioned as the cap approaches the whole sphere, where `asin` is vertical: a cap a
    /// thousandth of a degree short of everything comes back with about eleven digits rather than
    /// fifteen. Caps that size are not apertures, so this matters to nobody, but it is why the
    /// round trip is not exact at the top of the range.
    let capRadius (solidAngle: float<sr>) : float<rad> =
        let ratio = float solidAngle / (4.0 * Math.PI)
        2.0 * Angle.asin (sqrt (Math.Clamp(ratio, 0.0, 1.0)))

    /// Solid angle of a patch bounded by two longitudes and two latitudes,
    /// `dlon (sin lat2 - sin lat1)`. A survey footprint defined by coordinate ranges.
    let bandSolidAngle (longitudeWidth: float<rad>) (latitude1: float<rad>) (latitude2: float<rad>) : float<sr> =
        longitudeWidth * (Angle.sin latitude2 - Angle.sin latitude1) * 1.0<rad>

    /// Whole sphere, `4 pi` steradians.
    let fullSphere: float<sr> = 4.0 * Math.PI * 1.0<sr>
