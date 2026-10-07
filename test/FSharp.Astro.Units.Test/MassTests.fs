module MassTests

open Xunit
open FSharp.Astro.Units
open Helpers

[<Fact>]
let ``the Sun is about a thousand Jupiters`` () =
    close 1e-6 1047.5655<Mjup> (convert Mass.kilogramsPerSolarMass Mass.kilogramsPerJupiterMass 1.0<Msun>)

[<Fact>]
let ``the Sun is about 333 thousand Earths`` () =
    close 1e-6 332946.08<Mearth> (convert Mass.kilogramsPerSolarMass Mass.kilogramsPerEarthMass 1.0<Msun>)
