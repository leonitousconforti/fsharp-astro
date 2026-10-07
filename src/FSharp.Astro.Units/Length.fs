namespace FSharp.Astro.Units

open System

/// Length units and the factors that relate them to the meter.
[<RequireQualifiedAccess>]
module Length =

    /// Meters in one kilometer.
    let metersPerKilometer: float<m / km> = 1e3<m / km>

    /// Meters in one centimeter.
    let metersPerCentimeter: float<m / cm> = 1e-2<m / cm>

    /// Meters in one millimeter.
    let metersPerMillimeter: float<m / mm> = 1e-3<m / mm>

    /// Meters in one micrometer.
    let metersPerMicrometer: float<m / um> = 1e-6<m / um>

    /// Meters in one nanometer.
    let metersPerNanometer: float<m / nm> = 1e-9<m / nm>

    /// Meters in one angstrom.
    let metersPerAngstrom: float<m / AA> = 1e-10<m / AA>

    /// Meters in one astronomical unit. Exact, IAU 2012 Resolution B2.
    let metersPerAu: float<m / au> = 149597870700.0<m / au>

    /// Meters in one light year, c times one Julian year. Exact.
    let metersPerLightYear: float<m / ly> =
        Constants.c * Time.secondsPerYear * 1.0<yr / ly>

    /// Meters in one parsec. The IAU 2015 Resolution B2 definition is exactly 648000/pi au.
    let metersPerParsec: float<m / pc> = 648000.0 / Math.PI * metersPerAu * 1.0<au / pc>

    /// Meters in one kiloparsec.
    let metersPerKiloparsec: float<m / kpc> = metersPerParsec * 1e3<pc / kpc>

    /// Meters in one megaparsec.
    let metersPerMegaparsec: float<m / Mpc> = metersPerParsec * 1e6<pc / Mpc>

    /// Meters in one gigaparsec.
    let metersPerGigaparsec: float<m / Gpc> = metersPerParsec * 1e9<pc / Gpc>

    /// Meters in one nominal solar radius.
    let metersPerSolarRadius: float<m / Rsun> = Constants.Rsun / 1.0<Rsun>

    /// Meters in one nominal equatorial Earth radius.
    let metersPerEarthRadius: float<m / Rearth> = Constants.Rearth / 1.0<Rearth>

    /// Meters in one nominal equatorial Jupiter radius.
    let metersPerJupiterRadius: float<m / Rjup> = Constants.Rjup / 1.0<Rjup>

    /// Distance from a trigonometric parallax, d = 1/p. Valid for the small angles where the
    /// parsec is defined; a parallax of 1 arcsec is 1 pc.
    let ofParallax (parallax: float<arcsec>) : float<pc> = 1.0<pc arcsec> / parallax

    /// Trigonometric parallax of a source at the given distance, p = 1/d.
    let parallax (distance: float<pc>) : float<arcsec> = 1.0<pc arcsec> / distance
