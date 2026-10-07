module PlanckTests

open System
open Xunit
open FsCheck.FSharp
open FSharp.Astro.Units
open Helpers

/// 1.4 GHz, the frequency of the HI line and the deep Rayleigh-Jeans regime for anything warm.
let private radio = 1.4<GHz> * Frequency.hertzPerGigahertz

[<Fact>]
let ``the Planck function reduces to Rayleigh-Jeans where h nu is small`` () =
    let t = 100.0<K>
    // h nu / kB T here is 7e-4, so the two should agree to about half of that.
    close 1e-3 (Planck.rayleighJeansNu t radio) (Planck.bNu t radio)

[<Fact>]
let ``the Planck function falls below Rayleigh-Jeans where h nu is not small`` () =
    let t = 30.0<K>
    let frequency = 230.0<GHz> * Frequency.hertzPerGigahertz
    let exact = Planck.bNu t frequency
    let approximate = Planck.rayleighJeansNu t frequency
    // h nu / kB T is 0.37 here; the Rayleigh-Jeans form overshoots by about a sixth.
    Assert.True(exact < approximate)
    Assert.InRange(float (exact / approximate), 0.80, 0.85)

[<Fact>]
let ``the Planck function reduces to Wien where h nu is large`` () =
    let t = 5772.0<K>
    let frequency =
        Spectral.frequencyOfWavelength (200.0<nm> * Length.metersPerNanometer)
    close 1e-5 (Planck.wienNu t frequency) (Planck.bNu t frequency)

[<Fact>]
let ``the two forms of the Planck function describe the same spectrum`` () =
    check (
        forAll
            (Gen.choose (1, 100_000) |> Gen.map (fun n -> float n * 1e-8<m>))
            (fun wavelength ->
                let t = 5772.0<K>
                let frequency = Spectral.frequencyOfWavelength wavelength
                // B_lambda = B_nu c / lambda^2, the same change of variable as for a flux density.
                let expected = Planck.bNu t frequency * Constants.c / (wavelength * wavelength)
                within 1e-12 expected (Planck.bLambda t wavelength)
            )
    )

[<Fact>]
let ``brightness temperature inverts the Planck function exactly`` () =
    check (
        forAll
            // From 50 mK up. Below about 7 mK at this frequency the Planck function underflows to
            // zero and the temperature cannot be recovered from it; see the test below.
            (Gen.choose (50, 1_000_000) |> Gen.map (fun n -> float n * 1e-3<K>))
            (fun t ->
                let frequency = 100.0<GHz> * Frequency.hertzPerGigahertz
                within 1e-9 t (Planck.brightnessTemperature frequency (Planck.bNu t frequency))
            )
    )

[<Fact>]
let ``the Planck function underflows below a few millikelvin and takes the inversion with it`` () =
    let frequency = 100.0<GHz> * Frequency.hertzPerGigahertz

    // h nu / kB T passes 709 here and the exponential overflows, so the intensity is zero.
    let floor = Constants.h * frequency / (709.78 * Constants.kB)
    close 1e-3 6.7616e-3<K> floor

    Assert.True(Planck.bNu (floor / 2.0) frequency = 0.0<_>)
    Assert.Equal(0.0<K>, Planck.brightnessTemperature frequency (Planck.bNu (floor / 2.0) frequency))

    // Just above the floor the inversion is still exact.
    let warm = floor * 2.0
    close 1e-12 warm (Planck.brightnessTemperature frequency (Planck.bNu warm frequency))

    // The Rayleigh-Jeans form is linear and has no floor at all.
    close 1e-12 1e-6<K> (Planck.rayleighJeansTemperature frequency (Planck.rayleighJeansNu 1e-6<K> frequency))

[<Fact>]
let ``Rayleigh-Jeans temperature inverts the Rayleigh-Jeans form exactly`` () =
    check (
        forAll
            (Gen.choose (1, 100_000) |> Gen.map (fun n -> float n * 1e-3<K>))
            (fun t -> within 1e-12 t (Planck.rayleighJeansTemperature radio (Planck.rayleighJeansNu t radio)))
    )

[<Fact>]
let ``the two brightness temperatures agree in the Rayleigh-Jeans regime`` () =
    let t = 100.0<K>
    let intensity = Planck.bNu t radio
    close 1e-3 (Planck.brightnessTemperature radio intensity) (Planck.rayleighJeansTemperature radio intensity)

[<Fact>]
let ``jansky per beam converts to kelvin`` () =
    let fwhm = 10.0<arcsec> * Angle.radiansPerArcsecond

    let beam = Planck.gaussianBeamSolidAngle fwhm
    let flux = 1.0<Jy> * FluxDensity.siPerJansky
    let temperature = Planck.rayleighJeansTemperatureOfFluxDensity beam radio flux

    // The standard radio relation, T = 1.222e3 S/(nu^2 theta^2) with S in mJy, nu in GHz and
    // theta the beam FWHM in arcsec.
    let expected = 1.222e3 * 1000.0 / (1.4 * 1.4 * 10.0 * 10.0) * 1.0<K>
    close 2e-3 expected temperature

[<Fact>]
let ``flux density is the intensity times the solid angle`` () =
    let beam = Planck.gaussianBeamSolidAngle (1.0<arcsec> * Angle.radiansPerArcsecond)
    let t = 50.0<K>
    close 1e-15 (Planck.bNu t radio * beam) (Planck.fluxDensity beam t radio)

[<Fact>]
let ``a Gaussian beam of unit width subtends pi over four ln two steradians`` () =
    close 1e-15 (Math.PI / (4.0 * log 2.0) * 1.0<sr>) (Planck.gaussianBeamSolidAngle 1.0<rad>)

[<Fact>]
let ``the peak wavelength of the Sun is five hundred nanometres`` () =
    close 1e-3 (502.0<nm> * Length.metersPerNanometer) (Planck.peakWavelength Constants.Tsun)

[<Fact>]
let ``the microwave background peaks at 160 GHz`` () =
    close 1e-3 (160.2<GHz> * Frequency.hertzPerGigahertz) (Planck.peakFrequency 2.7255<K>)

[<Fact>]
let ``the peak frequency really is where the spectrum per unit frequency peaks`` () =
    let t = 5772.0<K>
    let peak = Planck.peakFrequency t
    let here = Planck.bNu t peak
    Assert.True(Planck.bNu t (peak * 0.99) < here)
    Assert.True(Planck.bNu t (peak * 1.01) < here)

[<Fact>]
let ``the peak wavelength really is where the spectrum per unit wavelength peaks`` () =
    let t = 5772.0<K>
    let peak = Planck.peakWavelength t
    let here = Planck.bLambda t peak
    Assert.True(Planck.bLambda t (peak * 0.99) < here)
    Assert.True(Planck.bLambda t (peak * 1.01) < here)

[<Fact>]
let ``the peaks in frequency and wavelength are not the same spectral point`` () =
    let t = 5772.0<K>
    let fromFrequency = Spectral.wavelengthOfFrequency (Planck.peakFrequency t)
    close 1e-6 1.7597805 (fromFrequency / Planck.peakWavelength t)

[<Fact>]
let ``integrating the Planck function over frequency gives Stefan-Boltzmann`` () =
    let t = 5772.0<K>

    // Trapezoid in log frequency over the twelve decades that carry the flux.
    let steps = 200_000
    let lo = log 1e9
    let hi = log 1e18
    let step = (hi - lo) / float steps

    let integrand i =
        let nu = exp (lo + float i * step) * 1.0<Hz>
        Planck.bNu t nu * nu

    let mutable total = (integrand 0 + integrand steps) / 2.0

    for i in 1 .. steps - 1 do
        total <- total + integrand i

    let integral = total * step
    close 1e-6 (Planck.integratedIntensity t) integral

[<Fact>]
let ``pi times the integrated intensity is the Stefan-Boltzmann flux`` () =
    let t = Constants.Tsun
    let squared = t * t
    close 1e-15 (Constants.sigmaSB * squared * squared) (Math.PI * Planck.integratedIntensity t * 1.0<sr>)

[<Fact>]
let ``a black body sphere of the Sun's size and temperature has the Sun's luminosity`` () =
    let area = 4.0 * Math.PI * Constants.Rsun * Constants.Rsun
    let emitted = Math.PI * Planck.integratedIntensity Constants.Tsun * 1.0<sr> * area
    close 1e-12 (Luminosity.blackBody Constants.Rsun Constants.Tsun) emitted
