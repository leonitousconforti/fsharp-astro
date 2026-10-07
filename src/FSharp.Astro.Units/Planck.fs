namespace FSharp.Astro.Units

open System

/// The Planck function, its Rayleigh-Jeans limit, and brightness temperature.
///
/// Specific intensity, or surface brightness, is a power per unit area per unit
/// bandwidth per unit solid angle: `float<W/(m^2 Hz sr)>` per unit frequency,
/// `float<W/(m^3 sr)>` per unit wavelength. The steradian is what distinguishes
/// it from the flux density a point source delivers; multiply by the solid
/// angle the source subtends to get back to `float<W/(m^2 Hz)>`.
///
/// Brightness temperature is the temperature a black body would need to have
/// this intensity. Radio astronomy quotes intensities that way because at
/// centimetre wavelengths `h nu << kB T` and the Planck function is linear in
/// temperature, so the temperature is just a relabelled intensity and
/// beam-filling sources add. That is the Rayleigh-Jeans limit, and
/// `rayleighJeansTemperature` is the one radio work means by "brightness
/// temperature". `brightnessTemperature` inverts the full Planck function
/// instead and is the one to use once `h nu / kB T` is not small, which for a
/// 30 K cloud is already a tenth at 90 GHz.
[<RequireQualifiedAccess>]
module Planck =

    /// Root of the Wien displacement law in frequency, the `x` solving `3(1 -
    /// e^-x) = x`.
    let private wienFrequencyRoot = 2.8214393721220787

    /// A steradian's worth of unit bookkeeping. Intensity is per unit solid
    /// angle, but the radian is dimensionless in SI, so the `sr` has to be
    /// attached rather than derived.
    let private perSteradian: float<1 / sr> = 1.0<1 / sr>

    /// Specific intensity of a black body per unit frequency, `2 h nu^3 / c^2 /
    /// (e^(h nu / kB T) - 1)`.
    let bNu (temperature: float<K>) (frequency: float<Hz>) : float<W / (m^2 Hz sr)> =
        let x = Constants.h * frequency / (Constants.kB * temperature)

        2.0 * Constants.h * frequency * frequency * frequency
        / (Constants.c * Constants.c)
        / (exp x - 1.0)
        * perSteradian

    /// Specific intensity of a black body per unit wavelength, `2 h c^2 /
    /// lambda^5 / (e^(h c / lambda kB T) - 1)`.
    let bLambda (temperature: float<K>) (wavelength: float<m>) : float<W / (m^3 sr)> =
        let x = Constants.h * Constants.c / (wavelength * Constants.kB * temperature)

        let squared = wavelength * wavelength

        2.0 * Constants.h * Constants.c * Constants.c
        / (squared * squared * wavelength)
        / (exp x - 1.0)
        * perSteradian

    /// Rayleigh-Jeans limit of the Planck function per unit frequency, `2 nu^2
    /// kB T / c^2`. Within a percent of `bNu` while `h nu / kB T` is below
    /// about 0.02.
    let rayleighJeansNu (temperature: float<K>) (frequency: float<Hz>) : float<W / (m^2 Hz sr)> =
        2.0 * frequency * frequency * Constants.kB * temperature
        / (Constants.c * Constants.c)
        * perSteradian

    /// Rayleigh-Jeans limit of the Planck function per unit wavelength, `2 c kB
    /// T / lambda^4`.
    let rayleighJeansLambda (temperature: float<K>) (wavelength: float<m>) : float<W / (m^3 sr)> =
        let squared = wavelength * wavelength

        2.0 * Constants.c * Constants.kB * temperature / (squared * squared)
        * perSteradian

    /// Wien limit of the Planck function per unit frequency, `2 h nu^3 / c^2
    /// e^(-h nu / kB T)`. The approximation on the short wavelength side, where
    /// the exponential dominates.
    let wienNu (temperature: float<K>) (frequency: float<Hz>) : float<W / (m^2 Hz sr)> =
        let x = Constants.h * frequency / (Constants.kB * temperature)

        2.0 * Constants.h * frequency * frequency * frequency
        / (Constants.c * Constants.c)
        * exp -x
        * perSteradian

    /// Brightness temperature of an intensity per unit frequency, inverting the
    /// Planck function exactly: `T = h nu / kB ln(1 + 2 h nu^3 / c^2 I)`.
    ///
    /// Returns zero for an intensity of zero, which is also what `bNu`
    /// underflows to once `h nu / kB T` passes 709 and the exponential
    /// overflows: about 7 mK at 100 GHz, 70 mK at 1 THz. Round tripping a
    /// temperature through `bNu` and back is exact well above that floor and
    /// gives zero below it. `rayleighJeansTemperature` is linear and has no
    /// such floor, which is another reason radio work uses it.
    let brightnessTemperature (frequency: float<Hz>) (intensity: float<W / (m^2 Hz sr)>) : float<K> =
        let numerator =
            2.0 * Constants.h * frequency * frequency * frequency
            / (Constants.c * Constants.c)
            * perSteradian

        Constants.h * frequency / (Constants.kB * log (1.0 + numerator / intensity))

    /// Rayleigh-Jeans brightness temperature of an intensity per unit
    /// frequency, `T = I c^2 / 2 kB nu^2`. Linear in the intensity, and what
    /// radio astronomy means by a temperature in kelvin on a map.
    let rayleighJeansTemperature (frequency: float<Hz>) (intensity: float<W / (m^2 Hz sr)>) : float<K> =
        intensity / perSteradian * Constants.c * Constants.c
        / (2.0 * Constants.kB * frequency * frequency)

    /// Flux density from a source of uniform brightness temperature filling a
    /// solid angle, `B_nu(T) Omega`. The solid angle of a Gaussian beam of full
    /// width at half maximum `theta` is `pi theta^2 / (4 ln 2)`.
    let fluxDensity (solidAngle: float<sr>) (temperature: float<K>) (frequency: float<Hz>) : float<W / (m^2 Hz)> =
        bNu temperature frequency * solidAngle

    /// Rayleigh-Jeans brightness temperature of a flux density spread over a
    /// solid angle, the jansky per beam to kelvin conversion.
    let rayleighJeansTemperatureOfFluxDensity
        (solidAngle: float<sr>)
        (frequency: float<Hz>)
        (flux: float<W / (m^2 Hz)>)
        : float<K> =
        rayleighJeansTemperature frequency (flux / solidAngle)

    /// Solid angle of a circular Gaussian beam of the given full width at half
    /// maximum, `pi theta^2 / (4 ln 2)`.
    let gaussianBeamSolidAngle (fullWidthHalfMaximum: float<rad>) : float<sr> =
        Math.PI * fullWidthHalfMaximum * fullWidthHalfMaximum / (4.0 * log 2.0)

    /// Frequency at which `bNu` peaks for a given temperature, by the Wien
    /// displacement law. Not the same spectral point as `peakWavelength`: `c /
    /// peakFrequency` is about 1.76 times `peakWavelength`, because a peak per
    /// unit frequency is not a peak per unit wavelength.
    let peakFrequency (temperature: float<K>) : float<Hz> =
        wienFrequencyRoot * Constants.kB * temperature / Constants.h

    /// Wavelength at which `bLambda` peaks for a given temperature, by the Wien
    /// displacement law.
    let peakWavelength (temperature: float<K>) : float<m> = Constants.bWien / temperature

    /// Intensity of a black body integrated over all frequencies, `sigma T^4 /
    /// pi`. Multiplying by pi gives the flux leaving the surface, which is the
    /// Stefan-Boltzmann law.
    let integratedIntensity (temperature: float<K>) : float<W / (m^2 sr)> =
        let squared = temperature * temperature

        Constants.sigmaSB * squared * squared / Math.PI * perSteradian
