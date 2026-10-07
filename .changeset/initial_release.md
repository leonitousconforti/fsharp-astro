---
default: major
---

# Initial release

`FSharp.Astro.Units`: units of measure for astronomy. Angle, length, time, mass, energy, power,
frequency, flux density and magnitude units alongside the SI ones FSharp.Core ships, with exactly
defined and IAU-nominal factors and one `convert` between any two units of a quantity; constants
from the 2019 SI, CODATA 2018 and IAU 2015; spectral equivalencies; Pogson magnitudes with the AB,
ST and Vega systems, Johnson-Cousins, 2MASS and SDSS zero points, and bolometric magnitudes on the
IAU 2015 scale; sexagesimal formatting and parsing; two-part `Instant` Julian Dates that resolve
picoseconds, with Modified Julian Dates, Julian and Besselian epochs, civil calendar conversion,
the Earth rotation angle and IAU 2006 mean and apparent sidereal time; the radio, optical and
relativistic Doppler conventions; the Planck function, its limits and brightness temperature;
spherical separations, position angles and offsets; proper motion; and extinction with the
Cardelli, Clayton and Mathis 1989 curve. Built for .NET 10 and .NET Standard 2.1.

`FSharp.Astro.Fits`: a FITS reader and writer. Strict by default and lazy, with headers that
round-trip byte for byte; images, binary tables, ASCII tables and random groups; cutout reads;
checksums; tile compressed images with RICE_1, GZIP_1, GZIP_2 and NOCOMPRESS; TDIMn cell shapes and
the TZEROn offset conventions; decoders and record mapping; the TUNIT and BUNIT unit grammar, with
conversion between units and units of measure on columns and pixels; and an erased type provider,
`FitsProvider`, shipped inside the package. Depends on `FSharp.Astro.Units`.
