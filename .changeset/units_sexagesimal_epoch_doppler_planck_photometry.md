---
default: minor
---

# Sexagesimal angles, instants, Doppler conventions, black bodies and photometric systems

Five additions to `FSharp.Astro.Units`.

`Sexagesimal` formats and parses `hh:mm:ss.s` and `dd:mm:ss.s`. Formatting rounds the seconds to a
fixed number of places and carries the overflow upward, so a value that rounds to 60 seconds
becomes the next minute. Parsing takes colons, spaces and the `h m s`, `d m s` and `° ' "` markers.

`Epoch` adds the `jd`, `mjd`, `jyear` and `byear` measures and the instants they name: Julian
Dates, Modified Julian Dates, Julian and Besselian epochs, conversion to and from
`DateTimeOffset`, Greenwich and local mean sidereal time, the Earth rotation angle and hour
angles. Instants relate by offsets rather than ratios, so there is no `convert`; `difference`
turns two of them back into a `float<d>`.

`Doppler` relates a velocity to a spectral shift under the `Radio`, `Optical` and `Relativistic`
conventions, in frequency or wavelength, with conversion between conventions at a fixed shift.

`Planck` has the Planck function per unit frequency and per unit wavelength, its Rayleigh-Jeans and
Wien limits, both forms of the Wien displacement law, exact and Rayleigh-Jeans brightness
temperature, Gaussian beam solid angles and the jansky per beam to kelvin conversion.

`Photometry` adds the ST and Vega systems alongside AB, with a `PhotometricBand` record and zero
points for Johnson-Cousins `UBVRI` and 2MASS `JHKs`, AB offsets derived from them, bolometric
magnitudes on the IAU 2015 scale, and extinction from a column, a reddening or an optical depth.
