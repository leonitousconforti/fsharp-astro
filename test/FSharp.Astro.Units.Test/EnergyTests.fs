module EnergyTests

open Xunit
open FSharp.Astro.Units
open Helpers

[<Fact>]
let ``ten million kelvin is a bit under a kiloelectron volt`` () =
    let kT = Energy.ofTemperature 1e7<K> / Energy.joulesPerKiloelectronVolt
    close 1e-5 0.861733<keV> kT
