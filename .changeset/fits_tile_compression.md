---
default: minor
---

# Tile compressed images read, and the suite reads files it did not write

`Fits.readImage` now sees through the tiled compression convention of section 10 of the standard,
which is how fpack stores an image. A compressed image is a binary table, but nothing above
`readImage` has to know that. `Fits.readCompressedImage` gives the convention itself: the tile grid,
the algorithm and its parameters, one tile at a time through `Compress.tile`, and the header with
the Z machinery taken off through `Compress.imageHeader`.

RICE_1, GZIP_1, GZIP_2 and NOCOMPRESS are read. `Rice` is the coder on its own, with an encoder as
well as a decoder.

Not yet: the quantized form fpack stores a floating point image in, where ZSCALE and ZZERO turn the
stored integers back into floats, and HCompress and PLIO. A tile this library cannot read raises
rather than returning pixels it guessed at.

`test/FSharp.Astro.Fits.Test/fixtures/archive` holds six files from the astropy test suite, under
BSD-3-Clause and recorded in a README there. Until now nothing in the suite read a byte stream this
library had not written, so the reader and the writer only ever proved each other. These check the
Rice coder against fpack in both directions, and they exercise random groups, ASCII tables and
TDIMn against files from elsewhere.
