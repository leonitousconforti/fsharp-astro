---
default: major
---

# Two-part Julian Dates

Instants are now an `Instant`, a struct holding two doubles that sum to a Julian Date, rather than
a single `float<jd>`.

A Julian Date in one double is about 2.45 million, where neighbouring doubles are 40 microseconds
apart. That quantisation follows from the magnitude of the number and no arithmetic improves it,
so every instant this century was pinned to 40 microseconds. Splitting the value so the fraction
carries its own exponent takes the resolution to 9.6 picoseconds, a factor of exactly 2^22. ERFA
and SOFA take Julian Dates as a pair for the same reason.

The representation is canonical, with `Day` an exact integer and `Fraction` in `[-0.5, 0.5)`, so
equality means "the same time" and comparison is chronological. `Instant.difference` cancels the
two integer day counts before subtracting the fractions, so instants a second apart come back to
picoseconds rather than to 40 microseconds.

It is 16 bytes and never allocates. `Instant.difference` measures the same as a raw `float`
subtraction; normalising in `Instant.ofParts` costs about half a nanosecond per construction.

`Epoch.ofDateTimeOffset` and `Epoch.toDateTimeOffset` are now exact to the 100 nanosecond tick
rather than to the millisecond. `Epoch.ofMjdParts` takes a Modified Julian Date in two parts, for
when one double is not enough: a single `float<mjd>` resolves 630 nanoseconds and so cannot carry
a microsecond.

## Breaking

Every `Epoch` function that took a `float<jd>` now takes an `Instant`. Use `Instant.ofJd` to pass
a plain Julian Date and `Instant.toJd` to get one back, remembering that both are as lossy as the
single double they go through.

Renamed, so that the direction reads off the name and the Julian Date stops being spelled out:

| Was | Now |
| --- | --- |
| `Epoch.mjdOfJd` | `Epoch.toMjd` |
| `Epoch.jdOfMjd` | `Epoch.ofMjd` |
| `Epoch.julianEpochOfJd` | `Epoch.toJulianEpoch` |
| `Epoch.jdOfJulianEpoch` | `Epoch.ofJulianEpoch` |
| `Epoch.besselianEpochOfJd` | `Epoch.toBesselianEpoch` |
| `Epoch.jdOfBesselianEpoch` | `Epoch.ofBesselianEpoch` |
| `Epoch.unixSeconds` | `Epoch.toUnixSeconds` |
| `Epoch.add` | `Instant.add` |
| `Epoch.difference` | `Instant.difference` |

`Epoch.j2000`, `Epoch.b1950`, `Epoch.mjdOrigin` and `Epoch.unixOrigin` are `Instant` rather than
`float<jd>`.
