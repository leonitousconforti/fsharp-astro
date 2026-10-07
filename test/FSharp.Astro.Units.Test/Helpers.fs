/// Generators and assertions shared by the test files.
module Helpers

open FsCheck
open FsCheck.FSharp
open Xunit

let check (property: Property) =
    Check.One(Config.QuickThrowOnFailure.WithMaxTest 300, property)

let forAll (gen: Gen<'T>) (test: 'T -> bool) : Property = Prop.forAll (Arb.fromGen gen) test

/// Relative error between two values of the same unit, scaled by the larger magnitude.
let relativeError (expected: float<'u>) (actual: float<'u>) : float =
    let e = float expected
    let a = float actual
    let scale = max (abs e) (abs a)
    if scale = 0.0 then 0.0 else abs (a - e) / scale

/// True when two values agree to within a relative tolerance.
let within (tolerance: float) (expected: float<'u>) (actual: float<'u>) : bool =
    relativeError expected actual <= tolerance

/// Asserts that two values agree to within a relative tolerance.
let close (tolerance: float) (expected: float<'u>) (actual: float<'u>) : unit =
    let error = relativeError expected actual

    Assert.True(
        error <= tolerance,
        $"expected {float expected} but got {float actual}, relative error {error:g3} exceeds {tolerance:g3}"
    )

/// Finite positive floats spanning the twenty-four decades astronomy routinely uses.
let positive: Gen<float> = gen {
    let! mantissa = Gen.choose (100, 999)
    let! exponent = Gen.choose (-12, 12)
    return float mantissa / 100.0 * 10.0 ** float exponent
}

/// Finite floats of either sign in a modest range, for angles and wrapping.
let signed: Gen<float> =
    Gen.choose (-10_000_000, 10_000_000) |> Gen.map (fun n -> float n / 97.0)

/// Floats in [-1, 1], the domain of the inverse sine and cosine.
let unitInterval: Gen<float> =
    Gen.choose (-1000, 1000) |> Gen.map (fun n -> float n / 1000.0)
