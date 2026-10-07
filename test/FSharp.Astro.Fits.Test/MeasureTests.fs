module MeasureTests

open Xunit
open FsCheck
open FsCheck.FSharp
open FSharp.Astro.Fits
open FSharp.Astro.Units

let private terms (unit: string) =
    UnitExpr.tryTerms unit
    |> Option.map (fun t -> t.Factor, t.Terms |> List.map (fun (s, p) -> s, string p))

[<Fact>]
let ``parses the shapes of unit the standard writes`` () =
    Assert.Equal(Some UnitExpr.Unity, UnitExpr.tryParse "")
    Assert.Equal(Some UnitExpr.Unity, UnitExpr.tryParse "1")
    Assert.Equal(Some(UnitExpr.Symbol "Jy"), UnitExpr.tryParse "Jy")
    Assert.Equal(Some(UnitExpr.Symbol "Jy"), UnitExpr.tryParse "  [Jy] ")

    Assert.Equal(Some(UnitExpr.Quotient(UnitExpr.Symbol "km", UnitExpr.Symbol "s")), UnitExpr.tryParse "km/s")

    Assert.Equal(Some(UnitExpr.Product(UnitExpr.Symbol "m", UnitExpr.Symbol "s")), UnitExpr.tryParse "m s")

    Assert.Equal(Some(UnitExpr.Product(UnitExpr.Symbol "m", UnitExpr.Symbol "s")), UnitExpr.tryParse "m*s")

    Assert.Equal(Some(UnitExpr.Raised(UnitExpr.Symbol "m", Power.create 3 2)), UnitExpr.tryParse "m**(3/2)")

    Assert.Equal(Some(UnitExpr.Raised(UnitExpr.Symbol "s", Power.ofInt -1)), UnitExpr.tryParse "s**-1")
    Assert.Equal(Some(UnitExpr.Raised(UnitExpr.Symbol "s", Power.ofInt -1)), UnitExpr.tryParse "s^-1")
    Assert.Equal(Some(UnitExpr.Apply("log", UnitExpr.Symbol "Hz")), UnitExpr.tryParse "log(Hz)")
    Assert.Equal(None, UnitExpr.tryParse "m**")
    Assert.Equal(None, UnitExpr.tryParse "(m")
    Assert.Equal(None, UnitExpr.tryParse "/s")

[<Fact>]
let ``the solidus divides by the one component that follows it`` () =
    // 10**(-20) J/(s m**2 Hz) is the standard's own example.
    Assert.Equal(
        Some(1e-20, [ "J", "1"; "Hz", "-1"; "m", "-2"; "s", "-1" ] |> List.sortBy fst),
        terms "10**(-20) J/(s m**2 Hz)"
    )

    // Without the parentheses the solidus reaches only the second.
    Assert.Equal(Some(1.0, [ "J", "1"; "m", "2"; "s", "-1" ]), terms "J/s m**2")

[<Fact>]
let ``the IAU style of writing exponents parses too`` () =
    Assert.Equal(terms "erg/(s cm**2 Angstrom)", terms "erg s-1 cm-2 Angstrom-1")
    Assert.Equal(Some(1.0, [ "cm", "2" ]), terms "cm2")

[<Fact>]
let ``sqrt is an exponent of one half and log is not a product of powers`` () =
    Assert.Equal(Some(1.0, [ "Hz", "1/2" ]), terms "sqrt(Hz)")
    Assert.Equal(None, terms "log(Hz)")
    Assert.Equal(None, terms "mag/ln(s)")

[<Fact>]
let ``formatting a unit produces a string that parses back the same`` () =
    let roundTrips (text: string) =
        match UnitExpr.tryParse text with
        | Some expr -> Assert.Equal(terms text, terms (UnitExpr.format expr))
        | None -> failwith $"'{text}' did not parse"

    for text in
        [
            "Jy"
            "km/s"
            "10**(-20) J/(s m**2 Hz)"
            "erg s-1 cm-2 Angstrom-1"
            "m**(3/2)"
            "sqrt(Hz)"
            "count/(s pixel)"
            "1"
        ] do
        roundTrips text

[<Fact>]
let ``a unit is a product of powers however it is written`` () =
    Assert.Equal(terms "Jy/beam", terms "Jy beam**-1")
    Assert.Equal(terms "m/s", terms "m s**-1")
    Assert.Equal(terms "m", terms "m**2/m")

[<Fact>]
let ``reduction takes prefixes off and an exact spelling wins`` () =
    // A millijansky is 1e-3 of 1e-26 W/(m^2 Hz), and that is kg/s^2 in base units.
    let reduced = Measure.tryReduce "mJy" |> Option.get
    Assert.Equal<(string * string) list>(
        [ "kg", "1"; "s", "-2" ],
        reduced.Terms |> List.map (fun (s, p) -> s, string p)
    )
    Assert.Equal(1.0, reduced.Factor / 1e-29, 12)
    Assert.True(Measure.isKnown "GHz")
    Assert.True(Measure.isKnown "kpc")
    // Pa is the pascal, not a peta-year, and min is the minute, not a milli-anything.
    Assert.Equal(Some 1.0, Measure.tryFactor "Pa" "N/m**2")
    Assert.Equal(Some 60.0, Measure.tryFactor "min" "s")
    Assert.False(Measure.isKnown "wombat")

[<Fact>]
let ``converting between units of the same quantity`` () =
    Assert.Equal(Some 1e-3, Measure.tryFactor "mJy" "Jy")
    Assert.Equal(Some 1e-3, Measure.tryFactor "10**(-3) Jy" "Jy")
    Assert.Equal(Some 1e3, Measure.tryFactor "Jy" "mJy")
    Assert.Equal(Some 3600.0, Measure.tryFactor "deg" "arcsec")
    Assert.Equal(None, Measure.tryFactor "Jy" "K")
    Assert.Equal(None, Measure.tryFactor "Jy" "nonsense(")

    // A symbol nobody knows stands for itself, so a beam cancels a beam.
    Assert.Equal(Some 1e-3, Measure.tryFactor "mJy/beam" "Jy/beam")
    Assert.Equal(None, Measure.tryFactor "mJy/beam" "Jy")

[<Fact>]
let ``cgs flux converts to SI`` () =
    let factor = Measure.tryFactor "erg s-1 cm-2" "W/m**2" |> Option.get
    Assert.Equal(1e-3, factor, 12)

[<Fact>]
let ``units that are the same need no conversion`` () =
    Assert.True(Measure.equivalent "Jy/beam" "Jy beam-1")
    Assert.True(Measure.equivalent "J" "kg m**2/s**2")
    Assert.False(Measure.equivalent "mJy" "Jy")
    Assert.True(Measure.compatible "mJy" "Jy")

[<Fact>]
let ``a column is read in the unit asked for`` () =
    let flux = Table.ofFloat32s "FLUX" [| 1500.0f; 2500.0f |] |> Table.withUnit "mJy"

    Assert.Equal<float<Jy>[]>([| 1.5<Jy>; 2.5<Jy> |], Quantity.column Tag.Jy flux)
    Assert.Equal<float<mJy>[]>([| 1500.0<mJy>; 2500.0<mJy> |], Quantity.column Tag.mJy flux)
    Assert.Equal(None, Quantity.tryColumn Tag.K flux)
    Assert.Throws<System.InvalidOperationException>(fun () -> Quantity.column Tag.K flux |> ignore)
    |> ignore

[<Fact>]
let ``a column with no unit cannot be read in one`` () =
    let plain = Table.ofFloat64s "X" [| 1.0 |]
    Assert.Equal(None, Quantity.tryColumn Tag.Jy plain)
    Assert.Throws<System.InvalidOperationException>(fun () -> Quantity.column Tag.Jy plain |> ignore)
    |> ignore

[<Fact>]
let ``reading a column applies TSCAL and TZERO before the unit conversion`` () =
    let counts =
        Table.ofInt16s "C" [| 1s; 2s |]
        |> Table.withUnit "mJy"
        |> Table.withScaling 100.0 10.0

    Assert.Equal<float<Jy>[]>([| 0.11<Jy>; 0.12<Jy> |], Quantity.column Tag.Jy counts)

[<Fact>]
let ``an image is read in the unit of BUNIT`` () =
    let image =
        Image.create [| 2 |] (ImageData.Int16 [| 10s; 20s |])
        |> fun i -> { i with BScale = 0.5 }

    let header = Header.set "BUNIT" (CardValue.String "mJy") Header.empty
    Assert.Equal<float<Jy>[]>([| 0.005<Jy>; 0.01<Jy> |], Quantity.image Tag.Jy header image)
    Assert.Equal(None, Quantity.tryImage Tag.deg header image)
    Assert.Equal(None, Quantity.tryImage Tag.Jy Header.empty image)

[<Fact>]
let ``every tag names a unit this library can reduce`` () =
    let symbols = [
        Tag.m.Symbol
        Tag.km.Symbol
        Tag.cm.Symbol
        Tag.mm.Symbol
        Tag.um.Symbol
        Tag.nm.Symbol
        Tag.angstrom.Symbol
        Tag.au.Symbol
        Tag.ly.Symbol
        Tag.pc.Symbol
        Tag.kpc.Symbol
        Tag.Mpc.Symbol
        Tag.Gpc.Symbol
        Tag.Rsun.Symbol
        Tag.s.Symbol
        Tag.ms.Symbol
        Tag.min.Symbol
        Tag.h.Symbol
        Tag.d.Symbol
        Tag.yr.Symbol
        Tag.kyr.Symbol
        Tag.Myr.Symbol
        Tag.Gyr.Symbol
        Tag.kg.Symbol
        Tag.g.Symbol
        Tag.Msun.Symbol
        Tag.K.Symbol
        Tag.Hz.Symbol
        Tag.kHz.Symbol
        Tag.MHz.Symbol
        Tag.GHz.Symbol
        Tag.THz.Symbol
        Tag.J.Symbol
        Tag.erg.Symbol
        Tag.eV.Symbol
        Tag.keV.Symbol
        Tag.MeV.Symbol
        Tag.GeV.Symbol
        Tag.W.Symbol
        Tag.Lsun.Symbol
        Tag.Jy.Symbol
        Tag.mJy.Symbol
        Tag.uJy.Symbol
        Tag.rad.Symbol
        Tag.deg.Symbol
        Tag.arcmin.Symbol
        Tag.arcsec.Symbol
        Tag.mas.Symbol
        Tag.uas.Symbol
        Tag.mag.Symbol
    ]

    for symbol in symbols do
        Assert.True(Measure.isKnown symbol, $"'{symbol}' is not a unit this library knows")
        Assert.True((Measure.tryMeasureName symbol).IsSome, $"'{symbol}' has no measure")

[<Fact>]
let ``the tags agree with the conversion factors of the units library`` () =
    let close (expected: float) (actual: float) = Assert.Equal(expected, actual, 9)

    close (float Length.metersPerParsec) (Measure.tryFactor "pc" "m" |> Option.get)
    close (float Length.metersPerAu) (Measure.tryFactor "AU" "m" |> Option.get)
    close (float Mass.kilogramsPerSolarMass) (Measure.tryFactor "solMass" "kg" |> Option.get)
    close (float Time.secondsPerYear) (Measure.tryFactor "yr" "s" |> Option.get)
    close (float Energy.joulesPerElectronVolt) (Measure.tryFactor "eV" "J" |> Option.get)
    close (float Angle.radiansPerArcsecond) (Measure.tryFactor "arcsec" "rad" |> Option.get)
    close 1e-26 (Measure.tryFactor "Jy" "W/(m**2 Hz)" |> Option.get)

[<Fact>]
let ``exponents reduce and add`` () =
    Assert.Equal(Power.create 1 2, Power.create 2 4)
    Assert.Equal(Power.create -1 2, Power.create 1 -2)
    Assert.Equal(Power.one, Power.add Power.half Power.half)
    Assert.Equal(Power.zero, Power.add Power.one (Power.negate Power.one))
    Assert.Equal(Some 3, (Power.ofInt 3).Whole)
    Assert.Equal(None, (Power.create 3 2).Whole)
    Assert.Equal(1.5, (Power.create 3 2).Value)
    Assert.Equal("3/2", string (Power.create 3 2))
    Assert.Throws<System.ArgumentException>(fun () -> Power.create 1 0 |> ignore)
    |> ignore

[<Fact>]
let ``any unit that parses reduces and converts to itself`` () =
    let symbols = [ "m"; "s"; "Jy"; "erg"; "deg"; "pc"; "count"; "beam"; "mag"; "K" ]

    let unit = gen {
        let! a = Gen.elements symbols
        let! b = Gen.elements symbols
        let! n = Gen.choose (1, 3)
        return $"{a}/({b}**{n})"
    }

    Prop.forAll
        (Arb.fromGen unit)
        (fun text ->
            match Measure.tryFactor text text with
            | Some factor -> factor = 1.0
            | None -> false
        )
    |> Check.QuickThrowOnFailure
