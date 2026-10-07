namespace FSharp.Astro.Units

/// The convention relating a spectral shift to a velocity. The three disagree
/// by terms of order `(v/c)^2`, which is a part in ten thousand at 3000 km/s,
/// so a velocity is meaningless without the convention that produced it. FITS
/// writes it in `VELREF` or in the `CTYPEn` suffix.
type DopplerConvention =
    /// `v = c (nu0 - nu) / nu0`. Linear in frequency, so a uniform channel
    /// width in frequency is a uniform channel width in velocity. The radio and
    /// millimetre convention.
    | Radio
    /// `v = c (lambda - lambda0) / lambda0`. Linear in wavelength, and equal to
    /// `c z` exactly. The optical convention.
    | Optical
    /// `v = c (nu0^2 - nu^2) / (nu0^2 + nu^2)`. The special relativistic
    /// Doppler shift, the only one of the three that is a velocity in the
    /// physical sense.
    | Relativistic

/// Doppler shifts between a velocity and a spectral shift about a rest
/// frequency or wavelength.
///
/// Everything goes through the redshift `z = lambda/lambda0 - 1 = nu0/nu - 1`,
/// which is convention free: the conventions are three different maps from `z`
/// to a velocity. Positive velocity is recession and positive `z` is a shift to
/// the red, so a line observed below its rest frequency has both positive.
[<RequireQualifiedAccess>]
module Doppler =

    /// Fraction of the speed of light corresponding to a redshift, under a
    /// convention.
    ///
    /// `Relativistic` approaches but never reaches one. It returns exactly one
    /// past a redshift of about 1e8, where `1 - beta`, which goes as
    /// `2/(1+z)^2`, drops below the spacing of the doubles near one. No
    /// rearrangement of the formula avoids that: one is the nearest double to
    /// the answer. `Radio` exceeds one above a redshift of one, which is not a
    /// bug either but the convention being used outside the regime it means
    /// anything in.
    let betaOfRedshift (convention: DopplerConvention) (z: float) : float =
        match convention with
        | Radio -> z / (1.0 + z)
        | Optical -> z
        | Relativistic ->
            let r = (1.0 + z) * (1.0 + z)
            (r - 1.0) / (r + 1.0)

    /// Redshift corresponding to a fraction of the speed of light, under a
    /// convention. The inverse of `betaOfRedshift`.
    let redshiftOfBeta (convention: DopplerConvention) (beta: float) : float =
        match convention with
        | Radio -> beta / (1.0 - beta)
        | Optical -> beta
        | Relativistic -> sqrt ((1.0 + beta) / (1.0 - beta)) - 1.0

    /// Velocity corresponding to a redshift, under a convention.
    let velocityOfRedshift (convention: DopplerConvention) (z: float) : float<m / s> =
        Constants.c * betaOfRedshift convention z

    /// Redshift corresponding to a velocity, under a convention.
    let redshiftOfVelocity (convention: DopplerConvention) (velocity: float<m / s>) : float =
        redshiftOfBeta convention (velocity / Constants.c)

    /// Redshift of an observed wavelength about a rest wavelength,
    /// `lambda/lambda0 - 1`.
    let redshiftOfWavelength (rest: float<'u>) (observed: float<'u>) : float = observed / rest - 1.0

    /// Observed wavelength of a line at a redshift, `lambda0 (1 + z)`.
    let wavelengthOfRedshift (rest: float<'u>) (z: float) : float<'u> = rest * (1.0 + z)

    /// Redshift of an observed frequency about a rest frequency, `nu0/nu - 1`.
    let redshiftOfFrequency (rest: float<'u>) (observed: float<'u>) : float = rest / observed - 1.0

    /// Observed frequency of a line at a redshift, `nu0 / (1 + z)`.
    let frequencyOfRedshift (rest: float<'u>) (z: float) : float<'u> = rest / (1.0 + z)

    /// Velocity of a source whose line is observed at the given wavelength,
    /// under a convention. The rest and observed wavelengths share a unit, so
    /// `1420.0<MHz>` and `21.1<cm>` both work as long as both arguments agree.
    let velocityOfWavelength (convention: DopplerConvention) (rest: float<'u>) (observed: float<'u>) : float<m / s> =
        velocityOfRedshift convention (redshiftOfWavelength rest observed)

    /// Wavelength at which a line of the given rest wavelength is observed from
    /// a source with the given velocity, under a convention.
    let wavelengthOfVelocity (convention: DopplerConvention) (rest: float<'u>) (velocity: float<m / s>) : float<'u> =
        wavelengthOfRedshift rest (redshiftOfVelocity convention velocity)

    /// Velocity of a source whose line is observed at the given frequency,
    /// under a convention.
    let velocityOfFrequency (convention: DopplerConvention) (rest: float<'u>) (observed: float<'u>) : float<m / s> =
        velocityOfRedshift convention (redshiftOfFrequency rest observed)

    /// Frequency at which a line of the given rest frequency is observed from a
    /// source with the given velocity, under a convention.
    let frequencyOfVelocity (convention: DopplerConvention) (rest: float<'u>) (velocity: float<m / s>) : float<'u> =
        frequencyOfRedshift rest (redshiftOfVelocity convention velocity)

    /// Converts a velocity from one convention to another at the same spectral
    /// shift.
    let convert (from: DopplerConvention) (into: DopplerConvention) (velocity: float<m / s>) : float<m / s> =
        velocityOfRedshift into (redshiftOfVelocity from velocity)

    /// Width in velocity of a channel of the given width in frequency, about a
    /// rest frequency. Under `Radio` this is independent of where in the band
    /// the channel sits, which is why spectral line work uses that convention.
    let velocityWidthOfFrequencyWidth
        (convention: DopplerConvention)
        (rest: float<'u>)
        (centre: float<'u>)
        (width: float<'u>)
        : float<m / s> =
        let lower = velocityOfFrequency convention rest (centre + width / 2.0)
        let upper = velocityOfFrequency convention rest (centre - width / 2.0)
        upper - lower
