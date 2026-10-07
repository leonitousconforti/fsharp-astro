module TimeTests

open Xunit
open FSharp.Astro.Units

[<Fact>]
let ``a Julian year is 365.25 days`` () =
    Assert.Equal(365.25<d>, convert Time.secondsPerYear Time.secondsPerDay 1.0<yr>)
