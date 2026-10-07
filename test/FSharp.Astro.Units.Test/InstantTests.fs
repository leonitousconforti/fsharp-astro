module InstantTests

open System
open Xunit
open FsCheck
open FsCheck.FSharp
open FSharp.Astro.Units
open Helpers

/// Spacing between a double and the next one up, for stating what a representation can resolve.
let private ulp (x: float) : float =
    BitConverter.Int64BitsToDouble(BitConverter.DoubleToInt64Bits(abs x) + 1L)
    - abs x

[<Fact>]
let ``the normal form is a whole day and a fraction under a half`` () =
    check (
        forAll
            (Gen.zip (Gen.choose (0, 5_000_000)) (Gen.choose (-2_000_000, 2_000_000)))
            (fun (d, f) ->
                let instant = Instant.ofParts (float d * 1.0<jd>) (float f / 1_000_000.0 * 1.0<jd>)
                let day = float instant.Day
                day = Math.Floor day
                && instant.Fraction >= -0.5<jd>
                && instant.Fraction < 0.5<jd>
            )
    )

[<Fact>]
let ``the two parts always sum back to the Julian Date`` () =
    check (
        forAll
            (Gen.zip (Gen.choose (0, 5_000_000)) (Gen.choose (-2_000_000, 2_000_000)))
            (fun (d, f) ->
                let day = float d * 1.0<jd>
                let fraction = float f / 1_000_000.0 * 1.0<jd>
                let instant = Instant.ofParts day fraction
                within 1e-15 (day + fraction) instant.JulianDate
            )
    )

[<Fact>]
let ``the normal form is canonical, so the same instant written two ways is equal`` () =
    let a = Instant.ofParts 2451545.0<jd> 0.25<jd>
    let b = Instant.ofParts 2451544.0<jd> 1.25<jd>
    let c = Instant.ofParts 2451545.25<jd> 0.0<jd>
    Assert.Equal(a, b)
    Assert.Equal(a, c)
    Assert.Equal(a.GetHashCode(), b.GetHashCode())

[<Fact>]
let ``instants compare chronologically`` () =
    let earlier = Instant.ofParts 2451545.0<jd> -0.4<jd>
    let middle = Instant.ofParts 2451545.0<jd> 0.1<jd>
    let later = Instant.ofParts 2451546.0<jd> -0.4<jd>
    Assert.True(earlier < middle)
    Assert.True(middle < later)
    Assert.True(earlier < later)
    Assert.Equal(earlier, Instant.earlier middle earlier)
    Assert.Equal(later, Instant.later middle later)

[<Fact>]
let ``comparison agrees with the difference`` () =
    check (
        forAll
            (Gen.zip (Gen.choose (-1_000_000, 1_000_000)) (Gen.choose (-1_000_000, 1_000_000)))
            (fun (a, b) ->
                let x = Instant.ofParts 2451545.0<jd> (float a / 100_000.0 * 1.0<jd>)
                let y = Instant.ofParts 2451545.0<jd> (float b / 100_000.0 * 1.0<jd>)
                let gap = Instant.difference x y
                (x < y) = (gap < 0.0<d>) && (x = y) = (gap = 0.0<d>)
            )
    )

[<Fact>]
let ``the second part keeps precision a single Julian Date cannot`` () =
    // A single double at JD 2451545 is quantised at 40 microseconds, so adding 864 nanoseconds to
    // it does nothing at all.
    let detail = 1e-11<jd> // 864 nanoseconds
    let asOneDouble = (2451545.0<jd> + detail) - 2451545.0<jd>
    Assert.Equal(0.0<jd>, asOneDouble)

    // The two-part form keeps it, and difference gives it back.
    let instant = Instant.ofParts 2451545.0<jd> detail
    let gap = Instant.difference instant Epoch.j2000
    close 1e-12 (1e-11<d>) gap

[<Fact>]
let ``a single double resolves 40 microseconds and the pair resolves ten picoseconds`` () =
    let singleDouble = ulp 2451545.0 * 86400.0
    close 1e-3 40.2e-6 singleDouble

    // The pair is limited by the spacing of doubles near a half day, not near 2.45 million.
    let pair = ulp 0.5 * 86400.0
    close 1e-3 9.59e-12 pair

    // Exactly 2^22, the difference in exponent between a half and two and a half million.
    close 1e-12 4194304.0 (singleDouble / pair)

[<Fact>]
let ``difference is exact for two instants in the same day`` () =
    // One second apart, two and a half million days from the origin. A single-double Julian Date
    // could not represent either endpoint to better than 40 microseconds.
    let second = 1.0 / 86400.0
    let a = Instant.ofParts 2451545.0<jd> 0.25<jd>
    let b = Instant.ofParts 2451545.0<jd> (0.25<jd> + second * 1.0<jd>)
    let gap = Instant.difference b a * Time.secondsPerDay
    // Within five picoseconds, which is the spacing of doubles at a quarter of a day: the limit
    // is representing the endpoint, not subtracting the two.
    Assert.True(abs (gap - 1.0<s>) < 1e-11<s>, $"gap was {float gap}")

[<Fact>]
let ``add and difference invert each other`` () =
    check (
        forAll
            (Gen.choose (-100_000_000, 100_000_000)
             |> Gen.map (fun n -> float n / 1000.0 * 1.0<d>))
            (fun duration ->
                let moved = Instant.add duration Epoch.j2000
                abs (Instant.difference moved Epoch.j2000 - duration) < 1e-9<d>
            )
    )

[<Fact>]
let ``adding a long duration does not swamp a short one`` () =
    // Ten thousand days and then one second. The second survives, which it would not if the two
    // were summed into a single Julian Date first.
    let instant = Instant.add 10_000.0<d> Epoch.j2000
    let later = Instant.add (1.0<s> / Time.secondsPerDay) instant
    let gap = Instant.difference later instant * Time.secondsPerDay
    Assert.True(abs (gap - 1.0<s>) < 1e-9<s>, $"gap was {float gap}")

[<Fact>]
let ``ofJd and toJd round trip a plain Julian Date`` () =
    check (
        forAll
            (Gen.choose (0, 5_000_000) |> Gen.map (fun n -> float n / 2.0 * 1.0<jd>))
            (fun julianDate -> Instant.toJd (Instant.ofJd julianDate) = julianDate)
    )

[<Fact>]
let ``a non finite Julian Date is refused`` () =
    Assert.Throws<ArgumentException>(fun () -> Instant.ofParts (nan * 1.0<jd>) 0.0<jd> |> ignore)
    |> ignore
    Assert.Throws<ArgumentException>(fun () -> Instant.ofParts (infinity * 1.0<jd>) 0.0<jd> |> ignore)
    |> ignore

[<Fact>]
let ``an instant prints as its Julian Date`` () =
    Assert.Equal("JD 2451545.000000000", string Epoch.j2000)
