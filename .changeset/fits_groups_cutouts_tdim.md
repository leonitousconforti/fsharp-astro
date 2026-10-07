---
default: minor
---

# Random groups, cutout reads and TDIMn cell shapes

`Fits.readGroups` reads the random groups layout of section 6 of the standard, which until now had
its size computed but no reader. Each group's parameters come back scaled by PSCALn and PZEROn, and
its array comes back as an `Image` carrying BZERO, BSCALE and BLANK, so the image functions apply to
it unchanged. Parameters that share a PTYPEn are summed, which is how the layout carries a value too
wide for the stored type. `Fits.randomGroups` writes one.

`Fits.readCutout` takes a rectangular sub-region of an image without reading the rest of the data
unit: one read per line along the fastest axis, which is the only part of the region that is
contiguous on disk. `Image.regionRuns` exposes the arithmetic on its own.

`Table.cellShape` and `Table.cellImage` give a column's cells the shape TDIMn declares, and a record
field of rank two or more is now filled from it, so `TDIM1 = '(3,2)'` over a repeat of six fills a
`float32[,]`. A record field typed `uint16`, `uint32`, `uint64` or `sbyte` now reads a column written
with the matching TZEROn offset convention.

A TDIMn whose dimensions do not multiply out to the repeat count is refused on write and reported as
a violation on read, where before it was taken at its word.
