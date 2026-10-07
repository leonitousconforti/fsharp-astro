namespace FSharp.Astro.Units

/// The shape of an interstellar extinction curve, the run of `A(lambda)/A(V)`
/// with wavelength.
type ExtinctionLaw =
    /// Cardelli, Clayton and Mathis 1989, the mean Galactic curve from 0.1 to
    /// 3.3 micrometres parameterised by `R_V` alone. Valid for `1/lambda`
    /// between 0.3 and 10 per micrometre.
    | Cardelli1989

/// Interstellar extinction: how much dust between here and a source dims it,
/// and how much more it dims the blue end than the red.
///
/// Extinction only ever makes a magnitude larger. It is wavelength dependent,
/// which is the whole subject: the difference between the extinction in two
/// bands is a reddening, and the ratio of the extinction in V to the reddening
/// across B and V is `R_V`, which runs about 3.1 in the diffuse interstellar
/// medium and up to 5 or so in dense clouds. Fix `R_V` and an extinction curve
/// gives the extinction at every wavelength from the extinction at one.
[<RequireQualifiedAccess>]
module Extinction =

    /// Ratio of total to selective extinction, `R_V = A_V / E(B-V)`, averaged
    /// over the diffuse Galactic interstellar medium. Dense clouds run higher.
    let rvDiffuse: float = 3.1

    // -----------------------------------------------------------------------------------------
    // Extinction as a scalar, independent of any curve.
    // -----------------------------------------------------------------------------------------

    /// Extinction from a column and a coefficient in magnitudes per unit
    /// column. The column can be anything the coefficient is quoted against: a
    /// hydrogen column in `float<1/m^2>`, a dust surface density in
    /// `float<kg/m^2>`, a path length in `float<pc>`.
    let ofColumn (coefficient: float<mag / 'u>) (column: float<'u>) : float<mag> = coefficient * column

    /// Extinction from a reddening and a ratio of total to selective
    /// extinction, `A = R E(B-V)`.
    let ofColourExcess (ratio: float) (excess: float<mag>) : float<mag> = ratio * excess

    /// Reddening from an extinction and a ratio of total to selective
    /// extinction, `E(B-V) = A / R`.
    let colourExcess (ratio: float) (extinction: float<mag>) : float<mag> = extinction / ratio

    /// Reddening as the difference between an observed and an intrinsic colour
    /// index.
    let colourExcessOfColours (observed: float<mag>) (intrinsic: float<mag>) : float<mag> = observed - intrinsic

    /// Magnitude of a source seen through the given extinction. Extinction
    /// dims, so this adds.
    let extinguish (extinction: float<mag>) (magnitude: float<mag>) : float<mag> = magnitude + extinction

    /// Magnitude a source would have without the given extinction.
    let deredden (extinction: float<mag>) (magnitude: float<mag>) : float<mag> = magnitude - extinction

    /// Fraction of the flux that survives the given extinction, `10^(-0.4 A)`.
    let transmission (extinction: float<mag>) : float = Magnitude.fluxRatio extinction

    /// Extinction corresponding to an optical depth, `A = 2.5 log10(e) tau`,
    /// about `1.086 tau`.
    let ofOpticalDepth (opticalDepth: float) : float<mag> =
        2.5 * log10 (exp 1.0) * opticalDepth * 1.0<mag>

    /// Optical depth corresponding to an extinction, `tau = A / 2.5 log10(e)`.
    let opticalDepth (extinction: float<mag>) : float =
        float extinction / (2.5 * log10 (exp 1.0))

    // -----------------------------------------------------------------------------------------
    // Extinction curves.
    // -----------------------------------------------------------------------------------------

    /// Shortest wavelength any curve here covers, `1/10` micrometres.
    let shortestWavelength: float<m> = 1e-7<m>

    /// Longest wavelength any curve here covers, `1/0.3` micrometres.
    let longestWavelength: float<m> = 1e-6<m> / 0.3

    /// Coefficients of CCM89 equation 3a, the optical and near infrared `a`,
    /// by ascending power of `y = 1/lambda - 1.82`.
    let private opticalA = [| 1.0; 0.17699; -0.50447; -0.02427; 0.72085; 0.01979; -0.77530; 0.32999 |]

    /// Coefficients of CCM89 equation 3b, the optical and near infrared `b`,
    /// by ascending power of `y = 1/lambda - 1.82`.
    let private opticalB = [| 0.0; 1.41338; 2.28305; 1.07233; -5.38434; -0.62251; 5.30260; -2.09002 |]

    /// Coefficients of CCM89 equation 5, the far ultraviolet `a`, by ascending
    /// power of `y = 1/lambda - 8`.
    let private farUltravioletA = [| -1.073; -0.628; 0.137; -0.070 |]

    /// Coefficients of CCM89 equation 5, the far ultraviolet `b`, by ascending
    /// power of `y = 1/lambda - 8`.
    let private farUltravioletB = [| 13.670; 4.257; -0.420; 0.374 |]

    /// The two CCM89 coefficients at an inverse wavelength in reciprocal
    /// micrometres, such that `A(lambda)/A(V)` is `a + b/R_V`. Cardelli,
    /// Clayton and Mathis 1989, equations 2 to 5.
    let private cardelli (x: float) : float * float =
        if x < 1.1 then
            // Infrared, equations 2a and 2b.
            let p = x ** 1.61
            0.574 * p, -0.527 * p
        elif x < 3.3 then
            // Optical and near infrared, equations 3a and 3b, about the 1.82
            // that normalises the curve to exactly one in V.
            let y = x - 1.82
            Polynomial.horner opticalA y, Polynomial.horner opticalB y
        elif x < 8.0 then
            // Ultraviolet, equation 4, with the 2175 angstrom bump in the
            // Lorentzian terms and the far ultraviolet rise folded in above 5.9
            // per micrometre.
            let fa, fb =
                if x < 5.9 then
                    0.0, 0.0
                else
                    let d = x - 5.9
                    -0.04473 * d * d - 0.009779 * d * d * d, 0.2130 * d * d + 0.1207 * d * d * d

            let a = 1.752 - 0.316 * x - 0.104 / ((x - 4.67) * (x - 4.67) + 0.341) + fa
            let b = -3.090 + 1.825 * x + 1.206 / ((x - 4.62) * (x - 4.62) + 0.263) + fb
            a, b
        else
            // Far ultraviolet, equation 5.
            let y = x - 8.0
            Polynomial.horner farUltravioletA y, Polynomial.horner farUltravioletB y

    /// `A(lambda)/A(V)` for a curve and a ratio of total to selective
    /// extinction, or `None` if the wavelength is outside the curve's range.
    let tryRelativeToV (law: ExtinctionLaw) (ratio: float) (wavelength: float<m>) : float option =
        if wavelength < shortestWavelength || wavelength > longestWavelength then
            None
        else
            match law with
            | Cardelli1989 ->
                // Reciprocal micrometres, the variable every extinction curve
                // is written in.
                let x = 1e-6<m> / wavelength
                let a, b = cardelli x
                Some(a + b / ratio)

    /// `A(lambda)/A(V)` for a curve and a ratio of total to selective
    /// extinction. One in V by construction, larger in the blue and smaller in
    /// the red. Raises if the wavelength is outside the curve's range.
    let relativeToV (law: ExtinctionLaw) (ratio: float) (wavelength: float<m>) : float =
        match tryRelativeToV law ratio wavelength with
        | Some value -> value
        | None ->
            invalidArg
                "wavelength"
                $"%g{float wavelength} m is outside the range of the extinction curve, \
                  %g{float shortestWavelength} to %g{float longestWavelength} m"

    /// Extinction at a wavelength, given the extinction in V and a curve.
    let atWavelength (law: ExtinctionLaw) (ratio: float) (av: float<mag>) (wavelength: float<m>) : float<mag> =
        av * relativeToV law ratio wavelength

    /// Extinction in a band, given the extinction in V and a curve. Uses the
    /// band's effective wavelength, which is a good approximation while the
    /// band is narrow and the curve across it is not steep; it is worst in U,
    /// where both fail.
    let inBand (law: ExtinctionLaw) (ratio: float) (av: float<mag>) (band: PhotometricBand) : float<mag> =
        atWavelength law ratio av band.EffectiveWavelength

    /// Reddening across two bands from the extinction in V, `E(1-2) = A_1 -
    /// A_2`.
    let bandColourExcess
        (law: ExtinctionLaw)
        (ratio: float)
        (av: float<mag>)
        (blue: PhotometricBand)
        (red: PhotometricBand)
        : float<mag> =
        inBand law ratio av blue - inBand law ratio av red
