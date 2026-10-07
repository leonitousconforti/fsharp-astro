namespace FSharp.Astro.Fits

open System
open FSharp.Astro.Units

// What the symbols in a FITS unit string are worth. UnitExpr parses the grammar; this is the
// dictionary. Every symbol the standard names in its Tables 7 to 10 reduces to a factor and a
// product of base units, so two unit strings can be compared and a value converted between them.
//
// A symbol this library has never heard of stands for itself, which is what makes 'Jy/beam' and
// 'mJy/beam' convertible even though nothing here knows what a beam is.

/// The meaning of FITS unit strings, and the bridge to the units of measure FSharp.Astro.Units
/// declares.
[<RequireQualifiedAccess>]
module Measure =

    /// Each known symbol as a factor and a product of base units. The bases are the SI base units
    /// plus the radian and the countable tokens of Table 9, which are dimensionless but distinct.
    let private known: Map<string, float * (string * int) list> =
        let joulesPerErg = float Energy.joulesPerErg
        let joulesPerElectronVolt = float Energy.joulesPerElectronVolt
        let secondsPerYear = float Time.secondsPerYear
        let radiansPerDegree = float Angle.radiansPerDegree

        Map.ofList [
            // SI base units and the radian.
            "m", (1.0, [ "m", 1 ])
            "kg", (1.0, [ "kg", 1 ])
            "g", (1e-3, [ "kg", 1 ])
            "s", (1.0, [ "s", 1 ])
            "A", (1.0, [ "A", 1 ])
            "K", (1.0, [ "K", 1 ])
            "mol", (1.0, [ "mol", 1 ])
            "cd", (1.0, [ "cd", 1 ])
            "rad", (1.0, [ "rad", 1 ])
            "sr", (1.0, [ "rad", 2 ])

            // Derived SI units.
            "Hz", (1.0, [ "s", -1 ])
            "N", (1.0, [ "kg", 1; "m", 1; "s", -2 ])
            "Pa", (1.0, [ "kg", 1; "m", -1; "s", -2 ])
            "J", (1.0, [ "kg", 1; "m", 2; "s", -2 ])
            "W", (1.0, [ "kg", 1; "m", 2; "s", -3 ])
            "C", (1.0, [ "A", 1; "s", 1 ])
            "V", (1.0, [ "kg", 1; "m", 2; "s", -3; "A", -1 ])
            "Ohm", (1.0, [ "kg", 1; "m", 2; "s", -3; "A", -2 ])
            "S", (1.0, [ "kg", -1; "m", -2; "s", 3; "A", 2 ])
            "F", (1.0, [ "kg", -1; "m", -2; "s", 4; "A", 2 ])
            "Wb", (1.0, [ "kg", 1; "m", 2; "s", -2; "A", -1 ])
            "T", (1.0, [ "kg", 1; "s", -2; "A", -1 ])
            "H", (1.0, [ "kg", 1; "m", 2; "s", -2; "A", -2 ])
            "lm", (1.0, [ "cd", 1; "rad", 2 ])
            "lx", (1.0, [ "cd", 1; "rad", 2; "m", -2 ])

            // Angle.
            "deg", (radiansPerDegree, [ "rad", 1 ])
            "arcmin", (float Angle.radiansPerArcminute, [ "rad", 1 ])
            "arcsec", (float Angle.radiansPerArcsecond, [ "rad", 1 ])
            "mas", (float Angle.radiansPerMilliarcsecond, [ "rad", 1 ])
            "uas", (float Angle.radiansPerMicroarcsecond, [ "rad", 1 ])

            // Time. The standard writes the Julian year as 'a'; files more often write 'yr'.
            "min", (60.0, [ "s", 1 ])
            "h", (3600.0, [ "s", 1 ])
            "d", (86400.0, [ "s", 1 ])
            "a", (secondsPerYear, [ "s", 1 ])
            "yr", (secondsPerYear, [ "s", 1 ])

            // Length.
            "AU", (float Length.metersPerAu, [ "m", 1 ])
            "au", (float Length.metersPerAu, [ "m", 1 ])
            "pc", (float Length.metersPerParsec, [ "m", 1 ])
            "lyr", (float Length.metersPerLightYear, [ "m", 1 ])
            "ly", (float Length.metersPerLightYear, [ "m", 1 ])
            "Angstrom", (1e-10, [ "m", 1 ])
            "angstrom", (1e-10, [ "m", 1 ])
            "AA", (1e-10, [ "m", 1 ])
            "solRad", (float Length.metersPerSolarRadius, [ "m", 1 ])
            "Rsun", (float Length.metersPerSolarRadius, [ "m", 1 ])

            // Mass.
            "solMass", (float Mass.kilogramsPerSolarMass, [ "kg", 1 ])
            "Msun", (float Mass.kilogramsPerSolarMass, [ "kg", 1 ])
            "u", (float Constants.atomicMassUnit, [ "kg", 1 ])

            // Energy and power.
            "erg", (joulesPerErg, [ "kg", 1; "m", 2; "s", -2 ])
            "eV", (joulesPerElectronVolt, [ "kg", 1; "m", 2; "s", -2 ])
            "Ry", (2.1798723611035e-18, [ "kg", 1; "m", 2; "s", -2 ])
            "solLum", (float Luminosity.wattsPerSolarLuminosity, [ "kg", 1; "m", 2; "s", -3 ])
            "Lsun", (float Luminosity.wattsPerSolarLuminosity, [ "kg", 1; "m", 2; "s", -3 ])

            // Everything else the standard allows. A jansky is 1e-26 W/(m^2 Hz), which is kg/s^2.
            "Jy", (float FluxDensity.siPerJansky, [ "kg", 1; "s", -2 ])
            "barn", (1e-28, [ "m", 2 ])
            "G", (1e-4, [ "kg", 1; "s", -2; "A", -1 ])
            "D", (1e-21 / float Constants.c, [ "A", 1; "s", 1; "m", 1 ])
            "R", (1e10 / (4.0 * Math.PI), [ "photon", 1; "m", -2; "s", -1; "rad", -2 ])
            "%", (1e-2, [])

            // Dimensionless tokens that are still worth keeping apart.
            "mag", (1.0, [ "mag", 1 ])
            "count", (1.0, [ "count", 1 ])
            "ct", (1.0, [ "count", 1 ])
            "photon", (1.0, [ "photon", 1 ])
            "ph", (1.0, [ "photon", 1 ])
            "pixel", (1.0, [ "pixel", 1 ])
            "pix", (1.0, [ "pixel", 1 ])
            "voxel", (1.0, [ "voxel", 1 ])
            "bin", (1.0, [ "bin", 1 ])
            "chan", (1.0, [ "chan", 1 ])
            "byte", (1.0, [ "byte", 1 ])
            "bit", (0.125, [ "byte", 1 ])
            "adu", (1.0, [ "adu", 1 ])
            "beam", (1.0, [ "beam", 1 ])
            "Sun", (1.0, [ "Sun", 1 ])
        ]

    /// Symbols an SI prefix may be attached to. The others, such as the degree and the parsec,
    /// take a prefix only in the spellings already listed above.
    let private prefixable =
        set [
            "m"
            "g"
            "s"
            "A"
            "K"
            "mol"
            "cd"
            "rad"
            "sr"
            "Hz"
            "N"
            "Pa"
            "J"
            "W"
            "C"
            "V"
            "Ohm"
            "S"
            "F"
            "Wb"
            "T"
            "H"
            "lm"
            "lx"
            "eV"
            "erg"
            "Jy"
            "pc"
            "yr"
            "a"
            "barn"
            "count"
            "photon"
            "byte"
            "bit"
            "adu"
        ]

    /// The SI prefixes of the standard's Table 10, longest first so 'da' wins over 'd'.
    let private prefixes = [
        "da", 1e1
        "Y", 1e24
        "Z", 1e21
        "E", 1e18
        "P", 1e15
        "T", 1e12
        "G", 1e9
        "M", 1e6
        "k", 1e3
        "h", 1e2
        "d", 1e-1
        "c", 1e-2
        "m", 1e-3
        "u", 1e-6
        "n", 1e-9
        "p", 1e-12
        "f", 1e-15
        "a", 1e-18
        "z", 1e-21
        "y", 1e-24
    ]

    /// What one symbol is worth, taking an SI prefix off it if it carries one. An exact spelling
    /// always wins, so 'Pa' is the pascal and not a peta-year.
    let private lookup (symbol: string) : (float * (string * int) list) option =
        match Map.tryFind symbol known with
        | Some entry -> Some entry
        | None ->
            prefixes
            |> List.tryPick (fun (prefix, factor) ->
                if
                    symbol.Length > prefix.Length
                    && symbol.StartsWith(prefix, StringComparison.Ordinal)
                then
                    let rest = symbol.Substring prefix.Length

                    if Set.contains rest prefixable then
                        Map.tryFind rest known |> Option.map (fun (f, bases) -> factor * f, bases)
                    else
                        None
                else
                    None
            )

    /// True when this library knows the symbol, with or without an SI prefix.
    let isKnown (symbol: string) : bool = (lookup symbol).IsSome

    /// Reduces a unit string to a factor and powers of base units, which is the form two units
    /// have to be in to be compared. A symbol this library does not know stands for itself.
    /// Returns None when the string is not a unit, or when it applies log, ln or exp.
    let tryReduce (unit: string) : UnitTerms option =
        UnitExpr.tryTerms unit
        |> Option.map (fun parsed ->
            let mutable factor = parsed.Factor
            let bases = ResizeArray<string * Power>()

            for symbol, power in parsed.Terms do
                match lookup symbol with
                | Some(value, reduced) ->
                    factor <- factor * (value ** power.Value)

                    for name, exponent in reduced do
                        bases.Add(name, Power.multiply power (Power.ofInt exponent))
                | None -> bases.Add(symbol, power)

            {
                Factor = factor
                Terms = UnitExpr.collect (List.ofSeq bases)
            }
        )

    /// The number that converts a value in one unit to a value in another, when both measure the
    /// same thing. `Measure.tryFactor "mJy" "Jy"` is `Some 0.001`.
    let tryFactor (from: string) (into: string) : float option =
        match tryReduce from, tryReduce into with
        | Some a, Some b when a.Terms = b.Terms -> Some(a.Factor / b.Factor)
        | _ -> None

    /// True when two unit strings measure the same thing, whatever the scale.
    let compatible (a: string) (b: string) : bool = (tryFactor a b).IsSome

    /// True when two unit strings are the same unit, so a value in one needs no conversion.
    let equivalent (a: string) (b: string) : bool =
        match tryFactor a b with
        | Some factor -> factor = 1.0
        | None -> false

    /// The measures FSharp.Astro.Units declares, by the FITS symbol that names them. The type
    /// provider uses this to give a column whose TUNIT is 'Jy' the type float&lt;Jy&gt;.
    let private measures: Map<string, string> =
        Map.ofList [
            "m", "m"
            "km", "km"
            "cm", "cm"
            "mm", "mm"
            "um", "um"
            "nm", "nm"
            "Angstrom", "angstrom"
            "angstrom", "angstrom"
            "AA", "angstrom"
            "AU", "au"
            "au", "au"
            "lyr", "ly"
            "ly", "ly"
            "pc", "pc"
            "kpc", "kpc"
            "Mpc", "Mpc"
            "Gpc", "Gpc"
            "solRad", "Rsun"
            "Rsun", "Rsun"
            "s", "s"
            "ms", "ms"
            "min", "min"
            "h", "h"
            "d", "d"
            "a", "yr"
            "yr", "yr"
            "kyr", "kyr"
            "Myr", "Myr"
            "Gyr", "Gyr"
            "kg", "kg"
            "g", "g"
            "solMass", "Msun"
            "Msun", "Msun"
            "K", "K"
            "Hz", "Hz"
            "kHz", "kHz"
            "MHz", "MHz"
            "GHz", "GHz"
            "THz", "THz"
            "J", "J"
            "erg", "erg"
            "eV", "eV"
            "keV", "keV"
            "MeV", "MeV"
            "GeV", "GeV"
            "W", "W"
            "solLum", "Lsun"
            "Lsun", "Lsun"
            "N", "N"
            "Pa", "Pa"
            "Jy", "Jy"
            "mJy", "mJy"
            "uJy", "uJy"
            "rad", "rad"
            "deg", "deg"
            "arcmin", "arcmin"
            "arcsec", "arcsec"
            "mas", "mas"
            "uas", "uas"
            "mag", "mag"
        ]

    /// The name of the FSharp.Astro.Units measure a FITS unit symbol denotes, when the library
    /// declares one. The symbol has to be exactly a measure: 'mJy' is `mJy`, and '10**(-3) Jy',
    /// which is the same unit, is not.
    let tryMeasureName (symbol: string) : string option = Map.tryFind symbol measures

/// An F# unit of measure paired with the FITS unit string that names it. A tag is what lets a
/// column or an image be read in a unit the compiler checks: the string says what to convert
/// from, the measure says what the result is.
type UnitTag<[<Measure>] 'u> = {
    /// The unit as TUNITn or BUNIT would spell it.
    Symbol: string
    /// One of the unit, which turns a plain number into a tagged one.
    One: float<'u>
}

/// Tags for the units FSharp.Astro.Units declares. `Tag.create` makes one for any other measure.
[<RequireQualifiedAccess>]
module Tag =
    /// A tag of your own: `Tag.create "Jy/beam" 1.0<Jy/beam>`, given a `beam` measure.
    let create (symbol: string) (one: float<'u>) : UnitTag<'u> = { Symbol = symbol; One = one }

    let m: UnitTag<m> = create "m" 1.0<m>
    let km: UnitTag<km> = create "km" 1.0<km>
    let cm: UnitTag<cm> = create "cm" 1.0<cm>
    let mm: UnitTag<mm> = create "mm" 1.0<mm>
    let um: UnitTag<um> = create "um" 1.0<um>
    let nm: UnitTag<nm> = create "nm" 1.0<nm>
    let angstrom: UnitTag<angstrom> = create "Angstrom" 1.0<angstrom>
    let au: UnitTag<au> = create "AU" 1.0<au>
    let ly: UnitTag<ly> = create "lyr" 1.0<ly>
    let pc: UnitTag<pc> = create "pc" 1.0<pc>
    let kpc: UnitTag<kpc> = create "kpc" 1.0<kpc>
    let Mpc: UnitTag<Mpc> = create "Mpc" 1.0<Mpc>
    let Gpc: UnitTag<Gpc> = create "Gpc" 1.0<Gpc>
    let Rsun: UnitTag<Rsun> = create "solRad" 1.0<Rsun>

    let s: UnitTag<s> = create "s" 1.0<s>
    let ms: UnitTag<ms> = create "ms" 1.0<ms>
    let min: UnitTag<min> = create "min" 1.0<min>
    let h: UnitTag<h> = create "h" 1.0<h>
    let d: UnitTag<d> = create "d" 1.0<d>
    let yr: UnitTag<yr> = create "yr" 1.0<yr>
    let kyr: UnitTag<kyr> = create "kyr" 1.0<kyr>
    let Myr: UnitTag<Myr> = create "Myr" 1.0<Myr>
    let Gyr: UnitTag<Gyr> = create "Gyr" 1.0<Gyr>

    let kg: UnitTag<kg> = create "kg" 1.0<kg>
    let g: UnitTag<g> = create "g" 1.0<g>
    let Msun: UnitTag<Msun> = create "solMass" 1.0<Msun>

    let K: UnitTag<K> = create "K" 1.0<K>
    let Hz: UnitTag<Hz> = create "Hz" 1.0<Hz>
    let kHz: UnitTag<kHz> = create "kHz" 1.0<kHz>
    let MHz: UnitTag<MHz> = create "MHz" 1.0<MHz>
    let GHz: UnitTag<GHz> = create "GHz" 1.0<GHz>
    let THz: UnitTag<THz> = create "THz" 1.0<THz>

    let J: UnitTag<J> = create "J" 1.0<J>
    let erg: UnitTag<erg> = create "erg" 1.0<erg>
    let eV: UnitTag<eV> = create "eV" 1.0<eV>
    let keV: UnitTag<keV> = create "keV" 1.0<keV>
    let MeV: UnitTag<MeV> = create "MeV" 1.0<MeV>
    let GeV: UnitTag<GeV> = create "GeV" 1.0<GeV>
    let W: UnitTag<W> = create "W" 1.0<W>
    let Lsun: UnitTag<Lsun> = create "solLum" 1.0<Lsun>

    let Jy: UnitTag<Jy> = create "Jy" 1.0<Jy>
    let mJy: UnitTag<mJy> = create "mJy" 1.0<mJy>
    let uJy: UnitTag<uJy> = create "uJy" 1.0<uJy>

    let rad: UnitTag<rad> = create "rad" 1.0<rad>
    let deg: UnitTag<deg> = create "deg" 1.0<deg>
    let arcmin: UnitTag<arcmin> = create "arcmin" 1.0<arcmin>
    let arcsec: UnitTag<arcsec> = create "arcsec" 1.0<arcsec>
    let mas: UnitTag<mas> = create "mas" 1.0<mas>
    let uas: UnitTag<uas> = create "uas" 1.0<uas>
    let mag: UnitTag<mag> = create "mag" 1.0<mag>

/// Reading a column or an image in a unit of measure, converting from TUNITn or BUNIT.
[<RequireQualifiedAccess>]
module Quantity =

    /// The factor from a declared unit into the tag's unit, or None when the two do not measure
    /// the same thing.
    let private tryFactorFrom (tag: UnitTag<'u>) (unit: string option) : float option =
        unit |> Option.bind (fun u -> Measure.tryFactor u tag.Symbol)

    let private factorFrom (tag: UnitTag<'u>) (unit: string option) (what: string) : float =
        match unit with
        | None -> invalidOp $"{what} declares no unit, so it cannot be read as '{tag.Symbol}'"
        | Some u ->
            match Measure.tryFactor u tag.Symbol with
            | Some factor -> factor
            | None -> invalidOp $"{what} is in '{u}', which does not measure the same thing as '{tag.Symbol}'"

    /// Physical values of a numeric column in the tag's unit: TZERO + TSCAL * stored, converted
    /// from TUNITn. Fails when the column declares no unit or one of another quantity.
    let column (tag: UnitTag<'u>) (column: Column) : float<'u>[] =
        let factor = factorFrom tag column.Info.Unit $"column '{column.Info.Name}'"
        Table.toFloat64 column |> Array.map (fun v -> v * factor * tag.One)

    /// As column, but None rather than a failure when the units do not match.
    let tryColumn (tag: UnitTag<'u>) (column: Column) : float<'u>[] option =
        tryFactorFrom tag column.Info.Unit
        |> Option.map (fun factor -> Table.toFloat64 column |> Array.map (fun v -> v * factor * tag.One))

    /// Physical pixel values in the tag's unit: BZERO + BSCALE * stored, converted from the BUNIT
    /// of the header the image was read with. Fails when BUNIT is absent or of another quantity.
    let image (tag: UnitTag<'u>) (header: Header) (image: Image) : float<'u>[] =
        let factor = factorFrom tag (Header.tryString "BUNIT" header) "image"
        Image.toFloat64 image |> Array.map (fun v -> v * factor * tag.One)

    /// As image, but None rather than a failure when the units do not match.
    let tryImage (tag: UnitTag<'u>) (header: Header) (image: Image) : float<'u>[] option =
        tryFactorFrom tag (Header.tryString "BUNIT" header)
        |> Option.map (fun factor -> Image.toFloat64 image |> Array.map (fun v -> v * factor * tag.One))
