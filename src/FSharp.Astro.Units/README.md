# FSharp.Astro.Units

Units of measure for astronomy in F# on .NET 10. No dependencies beyond FSharp.Core.

F# checks [units of measure](https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/units-of-measure)
at compile time and erases them at runtime, so a `float<pc>` is a plain `float` in the compiled
assembly and costs nothing. This library declares the units astronomers use alongside the SI units
FSharp.Core already ships, and gives you the exactly defined or IAU-nominal factors that relate them.
Adding a `float<pc>` to a `float<ly>` is a compile error. Multiplying by a factor converts.

```fsharp
open FSharp.Astro.Units

let distance = 1.3<kpc>
let meters = distance * Length.metersPerKiloparsec                                   // float<m>
let lightYears = Length.convert Length.metersPerKiloparsec Length.metersPerLightYear distance  // float<ly>

let proxima = Angle.convert Angle.radiansPerMilliarcsecond Angle.radiansPerArcsecond 768.5<mas>
Length.ofParallax proxima                                                            // 1.301<pc>

let hubble = 70.0<km/(s Mpc)>
let hubbleTime: float<Gyr> =
    1.0 / hubble * Length.metersPerMegaparsec / Length.metersPerKilometer / Time.secondsPerGigayear
```

## How it works

A unit in F# carries no scale. `pc` and `ly` are unrelated types until a conversion factor relates
them, so every quantity module holds factors to its SI base unit, named for what they are:
`Length.metersPerParsec` has type `float<m/pc>`. Multiply a `float<pc>` by it and the compiler
cancels the `pc` and leaves `float<m>`. Divide a `float<m>` by it and you get `float<pc>`.

Each module also has a `convert` that goes between any two of its units through the base:

```fsharp
Time.convert Time.secondsPerDay Time.secondsPerYear 365.25<d>   // 1.0<yr>
Mass.convert Mass.kilogramsPerSolarMass Mass.kilogramsPerJupiterMass 1.0<Msun>   // 1047.6<Mjup>
```

Compound units need no new declarations. `float<km/s>` is a velocity, `float<mas/yr>` a proper
motion, `float<erg/(s cm^2 AA)>` a flux density per wavelength, and `float<Msun/yr>` a star
formation rate. Factors chain through them the same way:

```fsharp
let cgs: float<erg/(s cm^2 Hz)> =
    1.0<Jy> * FluxDensity.siPerJansky / Luminosity.wattsPerErgPerSecond
    * Length.metersPerCentimeter * Length.metersPerCentimeter    // 1e-23
```

SI prefixes are separate units because the compiler cannot scale a type, so `kpc`, `Mpc` and `Gpc`
each exist with their own factor. Only the prefixes astronomy actually writes are declared.

To tag a plain number, multiply by a unit literal: `x * 1.0<Jy>`. To strip a unit, call `float`.
Both are free at runtime. This is how values from a FITS column or a catalogue acquire units.

## Units

The SI symbols `m`, `kg`, `s`, `K`, `Hz`, `N`, `Pa`, `J` and `W` are re-exported from FSharp.Core,
so `open FSharp.Astro.Units` is the only open you need and values flow to any other F# code using
the standard SI units. `Hz` is `1/s` and `W` is `J/s`, so `Constants.h * frequency` is already a
`float<J>`.

| Quantity | Units | Module |
| --- | --- | --- |
| Angle | `rad` `deg` `arcmin` `arcsec` `mas` `uas` `hourangle` `sr` | `Angle` |
| Length | `km` `cm` `mm` `um` `nm` `AA` `au` `ly` `pc` `kpc` `Mpc` `Gpc` `Rsun` `Rearth` `Rjup` | `Length` |
| Time | `ms` `min` `h` `d` `yr` `kyr` `Myr` `Gyr` | `Time` |
| Mass | `g` `Msun` `Mearth` `Mjup` | `Mass` |
| Energy | `erg` `eV` `keV` `MeV` `GeV` | `Energy` |
| Power | `Lsun` | `Luminosity` |
| Frequency | `kHz` `MHz` `GHz` `THz` | `Frequency` |
| Flux density | `Jy` `mJy` `uJy` | `FluxDensity` |
| Magnitude | `mag` | `Magnitude` `Photometry` `Extinction` |
| Instant | `jd` `mjd` `jyear` `byear` | `Epoch` |

`jd`, `mjd`, `jyear` and `byear` are instants rather than durations, so they relate to each other
by offsets instead of ratios and have no `convert`; see `Epoch` below. The year is the Julian year
of exactly 365.25 days. `Rsun`, `Rearth`, `Rjup` and `Lsun` are the
IAU 2015 nominal values. `Msun`, `Mearth` and `Mjup` are the IAU nominal GM divided by the CODATA
2018 G, matching astropy.

## Constants

`Constants` holds physical and astronomical constants with their units attached: `c`, `G`, `h`,
`hbar`, `kB`, `sigmaSB`, `bWien`, the particle masses, the nominal solar, terrestrial and jovian
parameters, and the derived masses. Values are the exact SI 2019 defining constants, CODATA 2018
for everything measured, and IAU 2015 Resolution B3 for the nominal values. Each has its source in
its doc comment.

## Beyond conversions

The quantity modules carry the relations that follow from their units.

```fsharp
Angle.wrap 360.0<deg> -30.0<deg>                 // 330.0<deg>
Angle.wrapSigned 24.0<hourangle> 13.0<hourangle> // -11.0<hourangle>
Angle.sin (90.0<deg> * Angle.radiansPerDegree)   // 1.0, trig takes radians and returns float

Spectral.wavelengthOfFrequency (1420.405751768<MHz> * Frequency.hertzPerMegahertz) // 0.2111<m>
Spectral.energyOfWavelength (1.0<AA> * Length.metersPerAngstrom) / Energy.joulesPerKiloelectronVolt // 12.4<keV>
Spectral.perWavelengthOfPerFrequency fnu wavelength   // f_lambda = f_nu c / lambda^2

Luminosity.blackBody Constants.Rsun Constants.Tsun    // 3.83e26<W>
Luminosity.flux Constants.Lsun (1.0<au> * Length.metersPerAu)  // 1361<W/m^2>

Magnitude.ab 3631.0<Jy>                   // 0.0<mag>
Magnitude.distanceModulus 100.0<pc>       // 5.0<mag>
Magnitude.absolute -26.74<mag> sunDistance  // 4.83<mag>
```

## Sexagesimal angles

`Sexagesimal` formats and parses the `hh:mm:ss.s` of a right ascension and the `dd:mm:ss.s` of a
declination. Formatting rounds the seconds to a fixed number of places and carries the overflow
upward, so a value that rounds to 60 seconds becomes the next minute rather than a sixtieth that
does not exist.

```fsharp
Sexagesimal.formatHours 3 5.919529166<hourangle>   // "05:55:10.305"
Sexagesimal.formatDegrees 2 7.407063888<deg>       // "+07:24:25.43"
Sexagesimal.parseHours "05h55m10.305s"             // 5.9195<hourangle>
Sexagesimal.parseDegrees "-00:30:00"               // -0.5<deg>, the sign is the angle's
```

Parsing takes colons, spaces and the `h m s`, `d m s` and `° ' "` markers, and fills missing
trailing fields with zero. It rejects minutes or seconds of 60 and up, and a fractional part on
anything but the last field written. `tryParseHours` and `tryParseDegrees` return an option.

## Instants

`Epoch` holds the points on a timeline that `Time` deliberately does not: Julian Dates, Modified
Julian Dates, Julian and Besselian epochs, and sidereal time. An instant is not a duration, so
these relate by offsets rather than ratios and there is no `convert`.

An instant is an `Instant`, a struct holding **two** doubles that sum to a Julian Date, rather than
one `float<jd>`. A Julian Date in a single double is around 2.45 million, where neighbouring
doubles are 40 microseconds apart, and that quantisation is fixed by the size of the number: no
amount of careful arithmetic improves it. Splitting the value so the fraction gets its own exponent
takes the resolution to 9.6 picoseconds, a factor of exactly 2^22. It is the same reason ERFA and
SOFA take Julian Dates as a pair.

```fsharp
Instant.ofJd 2451545.0<jd>                   // a plain Julian Date
Instant.ofParts 2451545.0<jd> 1e-11<jd>      // magnitude first, detail second
Epoch.ofDateTimeOffset DateTimeOffset.UtcNow // exact to the 100 ns tick
Epoch.ofMjdParts 51544.5<mjd> microsecond    // when one double is not enough

Instant.difference later Epoch.j2000         // float<d>, hand this to Time.convert
Instant.add 30.0<d> Epoch.j2000              // Instant
Epoch.toJulianEpoch Epoch.j2000              // 2000.0<jyear>
Epoch.greenwichMeanSiderealTime ut1          // 18.6974<hourangle>
Epoch.localApparentSiderealTime -155.47<deg> ut1
Epoch.hourAngle lst 5.919<hourangle>         // signed, negative is east of the meridian
```

The form is canonical, with `Day` an exact integer and `Fraction` in `[-0.5, 0.5)`, so two
instants are equal exactly when they name the same time and `<` is chronological. `difference`
cancels the two integer day counts before subtracting the fractions, which is where the precision
shows up: two instants a second apart in the year 2000 come back to picoseconds.

It is 16 bytes, never allocates, and `Instant.difference` measures the same as a raw `float`
subtraction. Normalising in `Instant.ofParts` costs about half a nanosecond, paid once when you
build one.

`Instant.toJd` and `Epoch.toMjd` return a single double and are lossy by construction, which is
fine for printing. The one thing the type cannot undo is a value already rounded into one double
before it arrives: `2451545.00000000001<jd>` has lost its last digits before `ofJd` is called.
That is what the `ofParts` and `ofMjdParts` overloads are for.

No time scale is modelled: an instant is whatever scale you put in. The sidereal functions want
UT1, and feeding them UTC costs at most the 0.9 s that DUT1 is kept below.

## Doppler shifts

`Doppler` relates a velocity to a spectral shift under the radio, optical and relativistic
conventions. The three disagree at order `(v/c)^2`, so a velocity means nothing without the
convention that produced it. Everything goes through the convention-free redshift.

```fsharp
let hi = 1420.405751768<MHz>
Doppler.velocityOfFrequency Radio hi 1420.0<MHz>      // float<m/s>
Doppler.frequencyOfVelocity Optical hi velocity       // float<MHz>
Doppler.convert Radio Relativistic velocity           // the same shift, read the other way
Doppler.betaOfRedshift Relativistic 1.0               // 0.6
```

## Black bodies

`Planck` has the Planck function per unit frequency and per unit wavelength, its Rayleigh-Jeans
and Wien limits, the Wien displacement law in both variables, and brightness temperature.
Intensity carries a steradian, which is what distinguishes it from the flux density a point source
delivers; multiply by a solid angle to get back.

```fsharp
Planck.bNu 5772.0<K> nu                                  // float<W/(m^2 Hz sr)>
Planck.brightnessTemperature nu intensity                // inverts the Planck function exactly
Planck.rayleighJeansTemperature nu intensity             // the linear one radio work means
Planck.gaussianBeamSolidAngle fwhm                       // float<sr>
Planck.rayleighJeansTemperatureOfFluxDensity beam nu s   // jansky per beam to kelvin
Planck.peakWavelength Constants.Tsun                     // 5.02e-7<m>
```

## The sphere

`Spherical` is geometry on the celestial sphere: separations, position angles, and the point a
given offset away from another. Everything is in radians, and the first coordinate is a longitude
measured eastward. Nothing here knows which frame it is in or transforms between frames.

```fsharp
Spherical.separation ra1 dec1 ra2 dec2      // float<rad>, accurate at an arcsecond and at 180 deg
Spherical.positionAngle ra1 dec1 ra2 dec2   // east of north, in [0, 2 pi)
Spherical.offset ra dec pa distance         // the point that far away in that direction
Spherical.capSolidAngle radius              // float<sr>, a circular field of view
```

`separation` uses the Vincenty formula rather than the spherical law of cosines. The law of
cosines loses half its digits exactly where astronomy needs them: at an arcsecond, `cos d` differs
from one by 1e-11 and a double keeps about five figures of the answer.

`offset` is the right way to move a position: it stays correct at the pole, where adding to the
longitude does not.

## Proper motion

`ProperMotion` carries a source across the sky over an epoch gap. The longitude component is the
catalogue one, `mu_alpha* = mu_alpha cos delta`, already multiplied by the cosine of the latitude.

```fsharp
ProperMotion.total muRaStar muDec                        // float<rad/yr>
ProperMotion.atEpoch 1991.25<jyear> 2016.0<jyear> ra dec muRaStar muDec
ProperMotion.tangentialVelocity 5.0<pc> mu               // float<m/s>, the 4.74 falls out
```

Positions are transported along the great circle the motion points down, not by adding to each
coordinate, so the result stays correct near the pole. The components are tied to the local north,
which rotates as the source moves, so transporting forward and back with the same pair does not
return the source exactly: the gap is the convergence of the meridians, half an arcsecond for the
fastest star over a century. Radial velocity is not modelled, so there is no perspective
acceleration.

## Photometric systems

`Photometry` adds the ST and Vega systems to the AB one in `Magnitude`, with bolometric magnitudes
on the IAU 2015 scale and extinction. AB and ST each fix a flat spectrum and so need only one
number; Vega is defined by the spectrum of a star and so needs a zero point per band, which is why
`vega` takes a `PhotometricBand` and `ab` does not.

```fsharp
Photometry.st fLambda                                  // float<mag>, HST's system
Photometry.vega Photometry.Bands.Ks fluxDensity        // float<mag>
Photometry.abOffset Photometry.Bands.Ks                // 1.84<mag>, m_AB - m_Vega
Photometry.absoluteBolometric Constants.Lsun           // 4.74<mag>
Photometry.absolute 14.2<mag> 1500.0<pc> 2.4<mag>      // with the dust taken out
```

`Photometry.Bands` carries Johnson-Cousins `U` to `I` from Bessell, Castelli and Plez 1998, 2MASS
`J`, `H` and `Ks` from Cohen, Wheaton and Megeath 2003, and SDSS `u` to `z` in `sdssAll`. Build a
`PhotometricBand` by hand for a filter not listed.

## Extinction

`Extinction` is dust: how much it dims a source, and how much more it dims the blue end than the
red. The scalar helpers work with any reddening and any `R_V`; an `ExtinctionLaw` turns the
extinction in one band into the extinction at every wavelength.

```fsharp
Extinction.ofColourExcess Extinction.rvDiffuse 0.3<mag>  // 0.93<mag> in V
Extinction.transmission 1.0<mag>                         // 0.398 of the flux survives
Extinction.relativeToV Cardelli1989 3.1 wavelength       // A(lambda)/A(V), one in V
Extinction.inBand Cardelli1989 3.1 av Photometry.Bands.B // 1.33 av
Extinction.bandColourExcess Cardelli1989 3.1 av bBand vBand
```

`Cardelli1989` is the mean Galactic curve, valid from 0.1 to 3.3 micrometres and parameterised by
`R_V` alone. `tryRelativeToV` returns an option outside that range; `relativeToV` raises.

## Building

This package lives in the [fsharp-astro](https://github.com/leonitousconforti/fsharp-astro)
repository alongside `FSharp.Astro.Fits`. The repository README covers building and testing.
