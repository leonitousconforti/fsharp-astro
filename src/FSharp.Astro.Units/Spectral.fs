namespace FSharp.Astro.Units

/// Relations between the wavelength, frequency and energy of light in vacuum, and between flux
/// densities per unit frequency and per unit wavelength.
[<RequireQualifiedAccess>]
module Spectral =

    /// Frequency of light with the given vacuum wavelength, c / lambda.
    let frequencyOfWavelength (wavelength: float<m>) : float<Hz> = Constants.c / wavelength

    /// Vacuum wavelength of light with the given frequency, c / nu.
    let wavelengthOfFrequency (frequency: float<Hz>) : float<m> = Constants.c / frequency

    /// Photon energy at the given frequency, h nu.
    let energyOfFrequency (frequency: float<Hz>) : float<J> = Constants.h * frequency

    /// Frequency of a photon with the given energy, E / h.
    let frequencyOfEnergy (energy: float<J>) : float<Hz> = energy / Constants.h

    /// Photon energy at the given vacuum wavelength, h c / lambda.
    let energyOfWavelength (wavelength: float<m>) : float<J> = Constants.h * Constants.c / wavelength

    /// Vacuum wavelength of a photon with the given energy, h c / E.
    let wavelengthOfEnergy (energy: float<J>) : float<m> = Constants.h * Constants.c / energy

    /// Flux density per unit wavelength equivalent to a flux density per unit frequency at the
    /// given wavelength, f_lambda = f_nu c / lambda^2.
    let perWavelengthOfPerFrequency (perFrequency: float<W / (m^2 Hz)>) (wavelength: float<m>) : float<W / m^3> =
        perFrequency * Constants.c / (wavelength * wavelength)

    /// Flux density per unit frequency equivalent to a flux density per unit wavelength at the
    /// given wavelength, f_nu = f_lambda lambda^2 / c.
    let perFrequencyOfPerWavelength (perWavelength: float<W / m^3>) (wavelength: float<m>) : float<W / (m^2 Hz)> =
        perWavelength * wavelength * wavelength / Constants.c
