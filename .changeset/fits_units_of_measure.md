---
default: minor
---

# FITS unit strings become units of measure

`FSharp.Astro.Fits` now reads the unit grammar of section 4.3 of the standard, which TUNITn and
BUNIT are written in. `UnitExpr` parses it, `Measure` says what the symbols are worth and converts
between units of the same quantity, and `Quantity` reads a column or an image in a unit the
compiler checks.

The type provider gives a floating point column whose TUNITn names a measure that
`FSharp.Astro.Units` declares that measure: a column with `TUNIT3 = 'Jy'` is now `float32<Jy>[]`
rather than `float32[]`, and `Pixels` takes its measure from BUNIT. Code that used such a column as
a bare number needs the unit added or stripped.

`FSharp.Astro.Fits` now depends on `FSharp.Astro.Units`. The two already version and publish
together.
