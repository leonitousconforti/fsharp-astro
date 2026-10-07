# fsharp-astro

Astronomy libraries for F# on .NET 10. One repository, two NuGet packages, no dependencies beyond
FSharp.Core. `FSharp.Astro.Fits` depends on `FSharp.Astro.Units`; nothing depends on the FITS
library. `FSharp.Astro.Units` is also built for .NET Standard 2.1.

| Package | What it does | Source |
| --- | --- | --- |
| `FSharp.Astro.Units` | Units of measure for astronomy: parsecs, arcseconds, janskys, solar masses and magnitudes alongside the SI units, with exactly defined and IAU-nominal conversion factors and constants. | [`src/FSharp.Astro.Units`](src/FSharp.Astro.Units/README.md) |
| `FSharp.Astro.Fits` | A FITS reader and writer. Strict by default, lazy, headers that round-trip byte for byte, images, binary tables, checksums, decoders, record mapping, units of measure from TUNIT and BUNIT, and an erased type provider. | [`src/FSharp.Astro.Fits`](src/FSharp.Astro.Fits/README.md) |

Each package folder has its own README, which is also the README shown on NuGet. The type provider
ships inside `FSharp.Astro.Fits`, so one package reference gives both the API and `FitsProvider`.

```fsharp
open FSharp.Astro.Units

let proxima = convert Angle.radiansPerMilliarcsecond Angle.radiansPerArcsecond 768.5<mas>
Length.ofParallax proxima                           // 1.301<pc>
Magnitude.distanceModulus 100.0<pc>                 // 5.0<mag>
let hubbleTime = 1.0 / 70.0<km/(s Mpc)> * Length.metersPerMegaparsec / Length.metersPerKilometer
```

```fsharp
open FSharp.Astro.Fits

use file = Fits.openFile "image.fits"
let image = Fits.readImage file 0
let pixels = Image.toFloat64 image
```

The two meet at the unit strings FITS carries in `TUNIT` and `BUNIT`. The FITS library parses that
grammar, converts between units, and the type provider gives a column the measure its unit names.

```fsharp
open FSharp.Astro.Fits
open FSharp.Astro.Fits.Typed
open FSharp.Astro.Units

Measure.tryFactor "mJy" "Jy"                 // Some 0.001

type Obs = FitsProvider<"catalog.fits">
use file = Obs.Load "catalog.fits"
file.CAT.FLUX                                // float32<Jy>[], because TUNIT3 is 'Jy'
```

## Layout

```
src/FSharp.Astro.Units               the units library
src/FSharp.Astro.Fits                the FITS library
src/FSharp.Astro.Fits.TypeProvider   the types provided properties erase to, shipped in lib/
src/FSharp.Astro.Fits.TypeProvider.DesignTime
                                     FitsProvider itself, shipped in typeproviders/fsharp41/
src/FSharp.Astro.Fits.UI             command line HDU lister
test/FSharp.Astro.Units.Test         xunit.v3 and FsCheck
test/FSharp.Astro.Fits.Test          xunit.v3 and FsCheck, with FITS fixtures
docs/                                the FITS 4.0 standard
```

`Directory.Build.props` holds the target framework, the FSharp.Core pin and the NuGet metadata
shared by both packages, which version in lockstep. Knope maintains the version there; `knope.toml`
and `.changeset/` describe the release process.

## Releasing

Releases are driven by change files, the way changesets works for JavaScript, using
[Knope](https://knope.tech). Both packages share one version and publish together.

1. A pull request that changes a package adds a Markdown file to `.changeset/` saying whether the
   change is `major`, `minor` or `patch`, with a short description. `knope document-change` writes
   one interactively, and `.changeset/README.md` shows the format.
2. When that lands on `main`, the prepare-release workflow computes the next version, writes
   `CHANGELOG.md`, bumps `Directory.Build.props`, and opens or updates a pull request from the
   `release` branch. That pull request previews the release and stays current as more change files
   land.
3. Merging the release pull request runs the release workflow: Release build, tests, pack, push to
   nuget.org with a short-lived key from trusted publishing, then a tag and GitHub release with the
   changelog entry as notes and the packages attached.

No version is edited by hand, and commit messages do not influence versions. Only change files do.

One-time setup before the first release:

- A fine-grained personal access token for this repository with read and write access to Contents
  and Pull requests, stored as the Actions secret `RELEASE_PAT`. The prepare-release workflow uses
  it instead of the built-in token because pull requests opened with the built-in token do not run
  CI.
- On nuget.org, open your profile and choose Trusted Publishing. Add a policy for repository owner
  `leonitousconforti`, repository `fsharp-astro`, workflow file `release.yml` and environment
  `release`, with scopes that allow pushing new packages and new versions matching `FSharp.Astro.*`.
- In the GitHub repository settings, add an Actions secret `NUGET_USER` holding your nuget.org
  profile name, not your email address.

Both packages are licensed under the GPL-3.0-only. The text is in `LICENSE`.

## Building

The flake in this repository provides the .NET SDK. Run `direnv allow` once to pick it up.

```
dotnet build
dotnet test
dotnet fantomas src test
dotnet pack src/FSharp.Astro.Units src/FSharp.Astro.Fits
```
