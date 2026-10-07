module TimeTests

open Xunit
open FSharp.Astro.Units
open Helpers

[<Fact>]
let ``a day is 86400 seconds`` () =
    Assert.Equal(86400.0<s>, 1.0<d> * Time.secondsPerDay)

[<Fact>]
let ``a Julian year is 365.25 days`` () =
    Assert.Equal(365.25<d>, Time.convert Time.secondsPerYear Time.secondsPerDay 1.0<yr>)

[<Fact>]
let ``a gigayear is 3.15576e16 seconds`` () =
    Assert.Equal(3.15576e16<s>, 1.0<Gyr> * Time.secondsPerGigayear)

[<Fact>]
let ``an hour is sixty minutes`` () =
    Assert.Equal(60.0<min>, Time.convert Time.secondsPerHour Time.secondsPerMinute 1.0<h>)

[<Fact>]
let ``converting there and back is the identity`` () =
    check (
        forAll
            positive
            (fun x ->
                let t = x * 1.0<Myr>

                Time.convert Time.secondsPerMegayear Time.secondsPerDay t
                |> Time.convert Time.secondsPerDay Time.secondsPerMegayear
                |> within 1e-12 t
            )
    )
