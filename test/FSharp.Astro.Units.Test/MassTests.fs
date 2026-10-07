module MassTests

open Xunit
open FSharp.Astro.Units
open Helpers

[<Fact>]
let ``a gram is a thousandth of a kilogram`` () =
    Assert.Equal(1e-3<kg>, 1.0<g> * Mass.kilogramsPerGram)

[<Fact>]
let ``a solar mass in kilograms`` () =
    close 1e-9 1.988409870698051e30<kg> (1.0<Msun> * Mass.kilogramsPerSolarMass)

[<Fact>]
let ``the Sun is about a thousand Jupiters`` () =
    close 1e-6 1047.5655<Mjup> (convert Mass.kilogramsPerSolarMass Mass.kilogramsPerJupiterMass 1.0<Msun>)

[<Fact>]
let ``the Sun is about 333 thousand Earths`` () =
    close 1e-6 332946.08<Mearth> (convert Mass.kilogramsPerSolarMass Mass.kilogramsPerEarthMass 1.0<Msun>)

[<Fact>]
let ``Jupiter is about 318 Earths`` () =
    close 1e-6 317.8284<Mearth> (convert Mass.kilogramsPerJupiterMass Mass.kilogramsPerEarthMass 1.0<Mjup>)

[<Fact>]
let ``mass ratios do not depend on G`` () =
    let ratio =
        Mass.kilogramsPerSolarMass / Mass.kilogramsPerJupiterMass * 1.0<Msun / Mjup>
    close 1e-15 (Constants.GMsun / Constants.GMjup) ratio

[<Fact>]
let ``converting there and back is the identity`` () =
    check (
        forAll
            positive
            (fun x ->
                let mass = x * 1.0<Msun>

                convert Mass.kilogramsPerSolarMass Mass.kilogramsPerGram mass
                |> convert Mass.kilogramsPerGram Mass.kilogramsPerSolarMass
                |> within 1e-12 mass
            )
    )
