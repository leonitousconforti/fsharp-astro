namespace FSharp.Astro.Units

/// A photometric passband, carrying what is needed to turn a magnitude in it
/// into a flux density and back: the wavelength the band is effectively centred
/// on, and the flux density per unit frequency of a source of zero Vega
/// magnitude.
///
/// The AB offset is derived from the Vega zero point rather than stored, so the
/// two systems a band supports cannot drift apart. Build your own band for a
/// filter not listed in `Photometry.Bands`.
type PhotometricBand = {
    /// Conventional name, as it appears in a catalogue column.
    Name: string
    /// Effective wavelength of the band.
    EffectiveWavelength: float<m>
    /// Flux density per unit frequency of a source of zero magnitude in the
    /// Vega system.
    VegaZeroPoint: float<Jy>
}

/// Photometric systems: AB, ST and Vega zero points, per-band conversions,
/// colour indices and bolometric magnitudes. Extinction lives in `Extinction`.
///
/// AB and ST are defined by a single number each, because each fixes a flat
/// spectrum: a zeroth AB magnitude is 3630.78 Jy at every frequency, a zeroth
/// ST magnitude is 3.6308e-9 erg/(s cm^2 AA) at every wavelength, and the two
/// agree at 5475.3 AA by construction. Vega is not: it is defined by the
/// spectrum of a particular star, so it has no single zero point and every band
/// needs its own. That is the whole of the difference, and it is why `ab` and
/// `st` take only a flux density while `vega` also takes a band.
[<RequireQualifiedAccess>]
module Photometry =

    /// Zero point of the AB system, the flux density per unit frequency of a
    /// zeroth magnitude source at every frequency: `10^(-0.4 * 48.60)` erg/(s
    /// cm^2 Hz), or 3630.78 Jy, from the definition `m = -2.5 log10 f_nu -
    /// 48.60` of Oke and Gunn 1983. The same value as `Magnitude.abZeroPoint`.
    let abZeroPoint: float<Jy> = Magnitude.abZeroPoint

    /// Zero point of the ST system, the flux density per unit wavelength of a
    /// zeroth magnitude source at every wavelength: `10^(-0.4 * 21.10)` erg/(s
    /// cm^2 AA), or 3.6308e-9, from the definition `m = -2.5 log10 f_lambda -
    /// 21.10` with `f_lambda` in cgs. The HST system. One erg/(s cm^2 AA) is
    /// exactly 1e7 W/m^3.
    let stZeroPoint: float<W / m^3> = 10.0 ** (-0.4 * 21.10) * 1e7<W / m^3>

    /// Wavelength at which the AB and ST systems give the same magnitude, about
    /// 5475 AA, near the middle of V. Derived from the two zero points rather
    /// than quoted, so it stays the wavelength at which these two in fact
    /// cross.
    let abStPivotWavelength: float<m> =
        sqrt (abZeroPoint * FluxDensity.siPerJansky * Constants.c / stZeroPoint)

    /// AB magnitude of a flux density per unit frequency.
    let ab (fluxDensity: float<Jy>) : float<mag> =
        Magnitude.ofFluxRatio fluxDensity abZeroPoint

    /// Flux density per unit frequency of a source of the given AB magnitude.
    let abFluxDensity (magnitude: float<mag>) : float<Jy> =
        abZeroPoint * Magnitude.fluxRatio magnitude

    /// ST magnitude of a flux density per unit wavelength.
    let st (fluxDensity: float<W / m^3>) : float<mag> =
        Magnitude.ofFluxRatio fluxDensity stZeroPoint

    /// Flux density per unit wavelength of a source of the given ST magnitude.
    let stFluxDensity (magnitude: float<mag>) : float<W / m^3> =
        stZeroPoint * Magnitude.fluxRatio magnitude

    /// Vega magnitude of a flux density per unit frequency in a band.
    let vega (band: PhotometricBand) (fluxDensity: float<Jy>) : float<mag> =
        Magnitude.ofFluxRatio fluxDensity band.VegaZeroPoint

    /// Flux density per unit frequency of a source of the given Vega magnitude
    /// in a band.
    let vegaFluxDensity (band: PhotometricBand) (magnitude: float<mag>) : float<Jy> =
        band.VegaZeroPoint * Magnitude.fluxRatio magnitude

    /// The band's offset between the systems, `m_AB - m_Vega`. Near zero in V,
    /// where Vega was tied to the AB scale, and close to two magnitudes in K.
    let abOffset (band: PhotometricBand) : float<mag> =
        Magnitude.ofFluxRatio band.VegaZeroPoint abZeroPoint

    /// AB magnitude equivalent to a Vega magnitude in a band.
    let abOfVega (band: PhotometricBand) (magnitude: float<mag>) : float<mag> = magnitude + abOffset band

    /// Vega magnitude equivalent to an AB magnitude in a band.
    let vegaOfAb (band: PhotometricBand) (magnitude: float<mag>) : float<mag> = magnitude - abOffset band

    /// Colour index, the magnitude in the bluer band minus the magnitude in the
    /// redder one. The subtraction is trivial; the name is here so that call
    /// sites say which way round it goes.
    let colourIndex (blue: float<mag>) (red: float<mag>) : float<mag> = blue - red

    // -----------------------------------------------------------------------------------------
    // Bolometric magnitudes, on the IAU 2015 Resolution B2 scale. That
    // resolution fixed the two zero points below exactly, which decoupled the
    // bolometric scale from the adopted solar luminosity: the Sun is now 4.74
    // mag absolute because its luminosity is what it is, not by definition.
    // -----------------------------------------------------------------------------------------

    /// Luminosity of a source of zero absolute bolometric magnitude. Exact, IAU
    /// 2015 B2.
    let bolometricLuminosityZeroPoint: float<W> = 3.0128e28<W>

    /// Irradiance from a source of zero apparent bolometric magnitude. Exact,
    /// IAU 2015 B2, and the luminosity zero point placed at 10 pc.
    let bolometricFluxZeroPoint: float<W / m^2> = 2.518021002e-8<W / m^2>

    /// Absolute bolometric magnitude of a luminosity. The Sun's 3.828e26 W is
    /// 4.74 mag.
    let absoluteBolometric (luminosity: float<W>) : float<mag> =
        Magnitude.ofFluxRatio luminosity bolometricLuminosityZeroPoint

    /// Luminosity of a source of the given absolute bolometric magnitude.
    let luminosityOfAbsoluteBolometric (magnitude: float<mag>) : float<W> =
        bolometricLuminosityZeroPoint * Magnitude.fluxRatio magnitude

    /// Apparent bolometric magnitude of an irradiance.
    let apparentBolometric (flux: float<W / m^2>) : float<mag> =
        Magnitude.ofFluxRatio flux bolometricFluxZeroPoint

    /// Irradiance from a source of the given apparent bolometric magnitude.
    let fluxOfApparentBolometric (magnitude: float<mag>) : float<W / m^2> =
        bolometricFluxZeroPoint * Magnitude.fluxRatio magnitude

    /// Bolometric correction in a band, `m_bol - m`. Add it to a magnitude in
    /// that band to get the bolometric one. Negative for every band of a star
    /// whose flux peaks outside it.
    let bolometricCorrection (bolometric: float<mag>) (magnitude: float<mag>) : float<mag> = bolometric - magnitude

    /// Absolute magnitude from an apparent magnitude, a distance and an
    /// extinction in the same band. `Magnitude.absolute` is the same thing with
    /// no extinction, and `Extinction` is where the extinction itself comes
    /// from.
    let absolute (apparent: float<mag>) (distance: float<pc>) (extinction: float<mag>) : float<mag> =
        apparent - Magnitude.distanceModulus distance - extinction

    /// Apparent magnitude from an absolute magnitude, a distance and an
    /// extinction in the same band.
    let apparent (absolute: float<mag>) (distance: float<pc>) (extinction: float<mag>) : float<mag> =
        absolute + Magnitude.distanceModulus distance + extinction

    /// The standard broad bands, with Vega zero points from the literature.
    ///
    /// `U` to `I` are Johnson-Cousins as calibrated by Bessell, Castelli and
    /// Plez 1998, Table A2. `J`, `H` and `Ks` are 2MASS as calibrated by Cohen,
    /// Wheaton and Megeath 2003, Table 2. `u` to `z` are SDSS, whose zero
    /// points are derived from Vega's synthetic AB magnitudes rather than
    /// measured directly, because SDSS is an AB system and has no native Vega
    /// calibration; the magnitudes are those of Takanashi et al. 2017, Table 2
    /// (arXiv:1610.06396), and `abOffset` reproduces them by construction.
    ///
    /// Zero points for the same nominal band differ by a few percent between
    /// calibrations, so a magnitude is only as good as the band it was measured
    /// in; these are the ones most catalogues are on.
    [<RequireQualifiedAccess>]
    module Bands =

        let private band name (wavelength: float<um>) (zeroPoint: float<Jy>) = {
            Name = name
            EffectiveWavelength = wavelength * Length.metersPerMicrometer
            VegaZeroPoint = zeroPoint
        }

        /// Johnson U, the near ultraviolet band cut off from below by the
        /// atmosphere.
        let U = band "U" 0.366<um> 1790.0<Jy>

        /// Johnson B.
        let B = band "B" 0.438<um> 4063.0<Jy>

        /// Johnson V, the band Vega's magnitude scale is anchored in and the
        /// one a bare "magnitude" usually means.
        let V = band "V" 0.545<um> 3636.0<Jy>

        /// Cousins R.
        let R = band "R" 0.641<um> 3064.0<Jy>

        /// Cousins I.
        let I = band "I" 0.798<um> 2416.0<Jy>

        /// 2MASS J.
        let J = band "J" 1.235<um> 1594.0<Jy>

        /// 2MASS H.
        let H = band "H" 1.662<um> 1024.0<Jy>

        /// 2MASS Ks, the short K band 2MASS used to keep the thermal background
        /// down.
        let Ks = band "Ks" 2.159<um> 666.7<Jy>

        /// SDSS bands, from Vega's synthetic AB magnitudes in each: u 0.951, g
        /// -0.080, r 0.169, i 0.389, z 0.556. Takanashi et al. 2017, Table 2
        /// (arXiv:1610.06396), computed from the STSDAS synphot Vega spectrum
        /// with V = +0.03. A band is built from the offset so that `abOffset`
        /// returns it back.
        let private sdss name (wavelength: float<um>) (abMinusVega: float<mag>) =
            band name wavelength (abZeroPoint * Magnitude.fluxRatio abMinusVega)

        /// SDSS u.
        let u = sdss "u" 0.355<um> 0.951<mag>

        /// SDSS g.
        let g = sdss "g" 0.467<um> -0.080<mag>

        /// SDSS r.
        let r = sdss "r" 0.616<um> 0.169<mag>

        /// SDSS i.
        let i = sdss "i" 0.747<um> 0.389<mag>

        /// SDSS z.
        let z = sdss "z" 0.892<um> 0.556<mag>

        /// The Johnson-Cousins and 2MASS bands, in order of effective
        /// wavelength.
        let all = [ U; B; V; R; I; J; H; Ks ]

        /// The SDSS bands, in order of effective wavelength.
        let sdssAll = [ u; g; r; i; z ]
