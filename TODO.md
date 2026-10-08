# Roadmap

## Repository

- API docs with fsdocs in `docs/`, one site for both packages.
- Round-trip the other way: write a `float<Jy>[]` column and have the writer emit TUNIT from the
  measure. Needs a measure-to-symbol map, which F# cannot derive from an erased measure, so it
  would have to go through a `UnitTag`.

## FSharp.Astro.Units

Done: units for angle, length, time, mass, energy, power, frequency, flux density and magnitude;
conversion factors and `convert` per quantity; constants from SI 2019, CODATA 2018 and IAU 2015;
spectral equivalencies; Pogson magnitudes, AB zero point and distance modulus; sexagesimal
formatting and parsing; JD, MJD, Julian and Besselian epochs with mean and apparent sidereal time;
the three Doppler conventions; the Planck function and brightness temperature; ST and Vega
photometry with Johnson-Cousins, 2MASS and SDSS bands; bolometric magnitudes; extinction with the
Cardelli 1989 curve; spherical separations, position angles and offsets; proper motion; two-part
Julian Dates resolving picoseconds. Nothing further is planned.

## FSharp.Astro.Fits

Done: cards, headers, HDU enumeration, images, binary tables, ASCII tables, random groups,
checksums, writing, cutout reads, decoders, record mapping, erased type provider; the TUNIT and
BUNIT grammar, conversion between units, and units of measure on provided columns and pixels; the
unsigned and signed byte column conventions; TDIMn cell shapes; both kinds of table in the type
provider; reading tile compressed images with RICE_1, GZIP_1, GZIP_2 and NOCOMPRESS.

### Next

- In-place header update that succeeds only when the new header fits in the same number of blocks.
- TDIMn that splits a character column into several fixed-width strings, as in TFORM '60A' with
  TDIM '(10,6)'. The shape is reported but the decoder still gives one string per cell, because the
  split has to happen before the trailing blanks are trimmed.
- Units on more of the provided surface: a measure on an integer column, and on `Data` when BSCALE
  and BZERO are the identity. Both need confidence that F# accepts a measure on every numeric type
  the provider emits.
- A violation for a TUNITn that is not a unit string at all, reported under strict handling rather
  than being ignored the way it is now.
- Type provider: watch the sample file and invalidate on change, nested types for HIERARCH
  prefixes, and a netstandard2.0 build of the core so editors on older runtimes can load it.

### Later

- The quantized float form of a compressed image: ZSCALE and ZZERO per tile turn the stored
  integers back into floats, with ZQUANTIZ and ZDITHER0 saying whether a dither was subtracted
  first. `Compress.tile` refuses such a tile today rather than guessing. This is how fpack stores
  any floating point image, so it is the gap that matters most.
- Writing a compressed image. `Rice.encode` already writes the bytes fpack writes; what is missing
  is generating the ZIMAGE header and cutting an image into tiles.
- HCompress and PLIO.
- Async and HTTP range request sources. IFitsSource is already offset-based so this is additive.
- More fixtures from real archives. `test/FSharp.Astro.Fits.Test/fixtures/archive` has six, which
  cover tile compression, random groups, ASCII tables and TDIMn. Nothing yet covers long strings,
  HIERARCH or a file from an instrument pipeline rather than another library's test suite.
- Benchmarks with BenchmarkDotNet in `benchmark/`.
- WCS as a separate library.
