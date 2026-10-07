namespace FSharp.Astro.Fits.TypeProvider

open System
open System.Reflection
open ProviderImplementation.ProvidedTypes
open FSharp.Astro.Fits
open FSharp.Astro.Units

/// Turns a FITS unit string into an F# unit of measure, so a column whose TUNIT is 'Jy' is
/// provided as float<Jy> rather than as a bare float. The measure is erased along with everything
/// else the provider emits, so this costs nothing at runtime; it only makes the compiler refuse to
/// add a flux to a frequency.
module Measures =

    /// The assembly that declares the measures. A unit of measure cannot be named with typeof, so
    /// the assembly comes from a function that lives beside the measures in it.
    let private unitsAssembly: Assembly =
        match <@ Length.ofParallax 1.0<arcsec> @> with
        | Quotations.Patterns.Call(_, method, _) -> method.DeclaringType.Assembly
        | _ -> typeof<Strictness>.Assembly

    /// The measure type of a name. An F# measure abbreviation, which is how this library spells
    /// the SI units it re-exports from FSharp.Core, emits no type of its own, so a name that is
    /// not a declaration of FSharp.Astro.Units is looked up among the SI units instead.
    let private measureType (name: string) : Type option =
        unitsAssembly.GetType("FSharp.Astro.Units." + name)
        |> Option.ofObj
        |> Option.orElseWith (fun () -> ProvidedMeasureBuilder.SI name |> Option.ofObj)

    let private product (parts: Type list) : Type =
        match parts with
        | [] -> ProvidedMeasureBuilder.One
        | first :: rest ->
            rest
            |> List.fold (fun acc part -> ProvidedMeasureBuilder.Product(acc, part)) first

    /// The measure a FITS unit string denotes, when every symbol in it is one FSharp.Astro.Units
    /// declares, every exponent is whole and there is no numeric factor. '10**(-26) W/(m**2 Hz)'
    /// is a jansky, but it is not `Jy`, so it gets no measure.
    let tryOfUnit (unit: string) : Type option =
        match UnitExpr.tryTerms unit with
        | Some terms when terms.Factor = 1.0 && not terms.Terms.IsEmpty ->
            let resolved =
                terms.Terms
                |> List.map (fun (symbol, power) ->
                    match Measure.tryMeasureName symbol, power.Whole with
                    | Some name, Some exponent -> measureType name |> Option.map (fun t -> t, exponent)
                    | _ -> None
                )

            if resolved |> List.forall Option.isSome then
                let parts = resolved |> List.map Option.get

                let repeat sign = [
                    for t, exponent in parts do
                        if sign * exponent > 0 then
                            yield! List.replicate (abs exponent) t
                ]

                match repeat 1, repeat -1 with
                | [], [] -> None
                | numerator, [] -> Some(product numerator)
                | numerator, denominator -> Some(ProvidedMeasureBuilder.Ratio(product numerator, product denominator))
            else
                None
        | _ -> None

    /// Annotates a numeric type with the measure of a unit string, leaving it alone when the unit
    /// is absent or is not a measure this library declares.
    let annotate (basic: Type) (unit: string option) : Type =
        match unit |> Option.bind tryOfUnit with
        | Some measure -> ProvidedMeasureBuilder.AnnotateType(basic, [ measure ])
        | None -> basic
