# Files this library did not write

Everything else the suite reads, it also wrote, which proves only that the reader and the writer
agree with each other. These files come from somewhere else, so reading them is the only check that
either agrees with the rest of the world.

Taken on 2026-10-06 from the test data of [astropy](https://github.com/astropy/astropy), at
`astropy/io/fits/tests/data`, which is BSD-3-Clause:

| File | What it is worth reading for |
| --- | --- |
| `compressed_image.fits` | fpack RICE_1, BYTEPIX 2, a 10 x 10 image of 0 to 99 in ten tiles. Small enough that a wrong decoder is obvious. |
| `comp.fits` | fpack RICE_1, BYTEPIX 2, a real 440 x 300 image in 300 tiles. |
| `compressed_float_bzero.fits` | fpack RICE_1 over three pixels, with the unsigned convention on the table. |
| `random_groups.fits` | A real AIPS UV data set: UU, VV, WW, BASELINE and DATE over a 3 x 1 x 128 x 1 x 1 array. |
| `ascii.fits` | An ASCII table with an E and an I field and an undefined row. |
| `tdim.fits` | TDIMn on a binary table column. |

The FITS standard is a format, not a protocol with a reference server, so agreement with cfitsio is
what interoperability means in practice. `ArchiveTests.fs` is where that agreement is checked.
