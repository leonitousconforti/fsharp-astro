---
default: minor
---

# The type provider ships, inside the FSharp.Astro.Fits package

`FitsProvider` was never packed. `FSharp.Astro.Fits.TypeProvider` had no `IsPackable`, so it
inherited `false` from the repository defaults, `dotnet pack` produced only the two library packages,
and the type provider the README documents could not be obtained from NuGet at all.

It now ships inside `FSharp.Astro.Fits`, so one package reference gives both the API and
`FitsProvider`. To do that it is split the way the compiler expects. `FSharp.Astro.Fits.TypeProvider`
keeps the handles every provided property erases to and is packed into `lib/`, because a compiled
program needs them. The new `FSharp.Astro.Fits.TypeProvider.DesignTime` holds the provider itself and
is packed into `typeproviders/fsharp41/`, where the compiler looks after reading the
`TypeProviderAssembly` attribute off the runtime half. A program that uses `FitsProvider` no longer
has `FSharp.TypeProviders.SDK` anywhere in its runtime closure.

There is no new package and nothing for a consumer to change.
