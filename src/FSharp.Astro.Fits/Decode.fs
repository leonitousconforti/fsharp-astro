namespace FSharp.Astro.Fits

open System
open System.Numerics

/// Reads a typed value out of a header. A decoder reports every problem it finds, so combining
/// decoders yields one error listing each missing or mistyped keyword.
type Decoder<'T> = Header -> Result<'T, Issue list>

/// A type that knows how to decode itself from a header.
type IFitsRecord<'T when 'T :> IFitsRecord<'T>> =
    static abstract Decoder: Decoder<'T>

/// Primitive decoders and the combinators that join them.
[<RequireQualifiedAccess>]
module Decode =
    /// Runs a decoder against a header.
    let run (decoder: Decoder<'T>) (header: Header) : Result<'T, Issue list> = decoder header

    /// A decoder that always yields the value.
    let succeed (value: 'T) : Decoder<'T> = fun _ -> Ok value

    /// A decoder that always fails with the issues.
    let fail (issues: Issue list) : Decoder<'T> = fun _ -> Error issues

    /// The header itself, for escaping to the untyped API.
    let header: Decoder<Header> = fun h -> Ok h

    let private describeType (value: CardValue) : string =
        match value with
        | CardValue.Logical _ -> "a logical"
        | CardValue.Integer _ -> "an integer"
        | CardValue.Real _ -> "a real"
        | CardValue.ComplexInt _
        | CardValue.ComplexReal _ -> "a complex"
        | CardValue.String _ -> "a string"
        | CardValue.Undefined -> "undefined"

    /// The raw value of a keyword.
    let value (keyword: string) : Decoder<CardValue> =
        fun h ->
            match Header.tryValue keyword h with
            | Some v -> Ok v
            | None -> Error [ Issue.create (Violation.MissingKeyword keyword) ]

    let private typed (expected: string) (pick: CardValue -> 'T option) (keyword: string) : Decoder<'T> =
        fun h ->
            match Header.tryValue keyword h with
            | None -> Error [ Issue.create (Violation.MissingKeyword keyword) ]
            | Some v ->
                match pick v with
                | Some x -> Ok x
                | None -> Error [ Issue.create (Violation.TypeMismatch(keyword, expected, describeType v)) ]

    /// A string keyword.
    let string (keyword: string) : Decoder<string> =
        typed
            "a string"
            (fun v ->
                match v with
                | CardValue.String s -> Some s
                | _ -> None
            )
            keyword

    /// A real keyword. Integers are widened.
    let float (keyword: string) : Decoder<float> =
        typed
            "a real"
            (fun v ->
                match v with
                | CardValue.Real r -> Some r
                | CardValue.Integer i -> Some(Operators.float i)
                | _ -> None
            )
            keyword

    /// An integer keyword.
    let int64 (keyword: string) : Decoder<int64> =
        typed
            "an integer"
            (fun v ->
                match v with
                | CardValue.Integer i -> Some i
                | _ -> None
            )
            keyword

    /// An integer keyword that fits in 32 bits.
    let int (keyword: string) : Decoder<int> =
        typed
            "a 32-bit integer"
            (fun v ->
                match v with
                | CardValue.Integer i when i >= Operators.int64 Int32.MinValue && i <= Operators.int64 Int32.MaxValue ->
                    Some(Operators.int i)
                | _ -> None
            )
            keyword

    /// A logical keyword.
    let bool (keyword: string) : Decoder<bool> =
        typed
            "a logical"
            (function
            | CardValue.Logical b -> Some b
            | _ -> None)
            keyword

    /// A complex keyword.
    let complex (keyword: string) : Decoder<Complex> =
        typed
            "a complex"
            (fun v ->
                match v with
                | CardValue.ComplexInt(re, im) -> Some(Complex(Operators.float re, Operators.float im))
                | CardValue.ComplexReal(re, im) -> Some(Complex(re, im))
                | _ -> None
            )
            keyword

    /// The comment of a keyword, which may be absent even when the keyword is present.
    let comment (keyword: string) : Decoder<string option> =
        fun h ->
            match Header.tryFind keyword h with
            | Some(Card.Value(_, _, comment)) -> Ok comment
            | _ -> Error [ Issue.create (Violation.MissingKeyword keyword) ]

    /// Every COMMENT card.
    let comments: Decoder<string list> = fun h -> Ok(Header.comments h)

    /// Every HISTORY card.
    let history: Decoder<string list> = fun h -> Ok(Header.history h)

    /// Applies a function to the decoded value.
    let map (f: 'a -> 'b) (decoder: Decoder<'a>) : Decoder<'b> = fun h -> decoder h |> Result.map f

    /// Runs two decoders and pairs their results. When both fail, the issues of both are reported.
    let zip (a: Decoder<'a>) (b: Decoder<'b>) : Decoder<'a * 'b> =
        fun h ->
            match a h, b h with
            | Ok x, Ok y -> Ok(x, y)
            | Error ea, Error eb -> Error(ea @ eb)
            | Error e, _
            | _, Error e -> Error e

    /// Combines two decoders.
    let map2 (f: 'a -> 'b -> 'c) (a: Decoder<'a>) (b: Decoder<'b>) : Decoder<'c> = zip a b |> map (fun (x, y) -> f x y)

    /// Combines three decoders.
    let map3 (f: 'a -> 'b -> 'c -> 'd) (a: Decoder<'a>) (b: Decoder<'b>) (c: Decoder<'c>) : Decoder<'d> =
        zip a (zip b c) |> map (fun (x, (y, z)) -> f x y z)

    /// Applies a decoded function to a decoded value.
    let apply (f: Decoder<'a -> 'b>) (a: Decoder<'a>) : Decoder<'b> = map2 (fun f x -> f x) f a

    /// Chooses the next decoder from a decoded value. Unlike zip, a failure here stops the chain.
    let andThen (f: 'a -> Decoder<'b>) (decoder: Decoder<'a>) : Decoder<'b> =
        fun h ->
            match decoder h with
            | Ok v -> f v h
            | Error e -> Error e

    /// Runs every decoder and collects the results, reporting the issues of all that fail.
    let all (decoders: Decoder<'T> list) : Decoder<'T list> =
        List.foldBack (fun d acc -> map2 (fun x xs -> x :: xs) d acc) decoders (succeed [])

    /// Turns a missing keyword into None. A keyword of the wrong type is still an error. For a
    /// decoder of several keywords, the result is None only when every one of them is missing.
    let optional (decoder: Decoder<'T>) : Decoder<'T option> =
        fun h ->
            match decoder h with
            | Ok v -> Ok(Some v)
            | Error issues when
                issues
                |> List.forall (fun i ->
                    match i.Violation with
                    | Violation.MissingKeyword _ -> true
                    | _ -> false
                )
                ->
                Ok None
            | Error issues -> Error issues

    /// Replaces None with a fallback.
    let withDefault (fallback: 'T) (decoder: Decoder<'T option>) : Decoder<'T> =
        map (Option.defaultValue fallback) decoder

    /// Fails with the reason when the decoded value does not satisfy the predicate.
    let check (keyword: string) (reason: string) (predicate: 'T -> bool) (decoder: Decoder<'T>) : Decoder<'T> =
        fun h ->
            match decoder h with
            | Ok v when predicate v -> Ok v
            | Ok _ -> Error [ Issue.create (Violation.InvalidKeywordValue(keyword, reason)) ]
            | Error e -> Error e

    /// The decoder a record type declares for itself.
    let record<'T when 'T :> IFitsRecord<'T>> () : Decoder<'T> = 'T.Decoder

/// Builds decoders with computation expression syntax. `let! a = ... and! b = ...` runs the
/// decoders independently and reports the issues of every one that fails. A sequence of `let!`
/// runs them in order and stops at the first failure.
type DecodeBuilder() =
    member _.Return(value: 'T) : Decoder<'T> = Decode.succeed value
    member _.ReturnFrom(decoder: Decoder<'T>) : Decoder<'T> = decoder
    member _.Bind(decoder: Decoder<'a>, f: 'a -> Decoder<'b>) : Decoder<'b> = Decode.andThen f decoder
    member _.BindReturn(decoder: Decoder<'a>, f: 'a -> 'b) : Decoder<'b> = Decode.map f decoder
    member _.MergeSources(a: Decoder<'a>, b: Decoder<'b>) : Decoder<'a * 'b> = Decode.zip a b
    member _.Zero() : Decoder<unit> = Decode.succeed ()

/// The `decode` computation expression.
[<AutoOpen>]
module DecodeBuilder =
    /// Builds a decoder. See DecodeBuilder.
    let decode = DecodeBuilder()
