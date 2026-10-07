---
default: minor
---

# ASCII tables, unsigned columns, and a commentary card that could not be read back

`XTENSION = 'TABLE'` is now read and written. `Fits.readAsciiTable` gives an `AsciiTable`, `Ascii`
parses the Fortran A, I, F, E and D edit descriptors and lays fields out from TBCOLn, and
`Fits.asciiTable` writes one. Decoding a field gives back the same `Column` a binary table does, so
`Table.toFloat64`, record mapping and `Quantity.column` work on either kind of table, and
`Fits.readRecords` now picks the right reader from the HDU itself.

`Table.ofUInt16s`, `toUInt16`, `isUInt16` and the matching `Int8`, `UInt32` and `UInt64` functions
cover the TZEROn offset conventions for columns, the way the image functions already did.

Fixed: writing a commentary card whose keyword is not COMMENT, HISTORY or blank and whose text
begins with the value indicator produced a card that read back as a value card with no value. Such
a card is now refused with `Violation.AmbiguousCommentary`, since the format cannot represent it.
