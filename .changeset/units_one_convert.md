---
default: major
---

# One `convert` instead of one per quantity

`Length.convert`, `Angle.convert`, `Time.convert`, `Mass.convert`, `Energy.convert`,
`Luminosity.convert`, `Frequency.convert` and `FluxDensity.convert` were eight copies of the same
line, differing only in the base unit. They are replaced by a single `convert` that takes the base
as a measure parameter alongside the two units, so one function serves every quantity, including any
added later.

```fsharp
convert Length.metersPerParsec Length.metersPerLightYear 1.0<pc>   // was Length.convert ...
convert Time.secondsPerDay Time.secondsPerYear 365.25<d>           // was Time.convert ...
```

It is auto-opened, so `open FSharp.Astro.Units` is still the only open needed. Dropping the module
prefix is the whole migration.

The eight signatures looked as though they kept the factors of one quantity together, but they did
not: `Length.convert Length.metersPerParsec Angle.radiansPerDegree 1.0<pc>` compiled before this
change, because F# unified the base with whatever it was given. Nothing is lost by collapsing them.

`Doppler.convert` is untouched. It converts between velocity conventions rather than between units,
and has nothing to do with the other eight.
