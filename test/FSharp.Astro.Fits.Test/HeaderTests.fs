module HeaderTests

open System
open System.Text
open Xunit
open FsCheck
open FsCheck.FSharp
open FSharp.Astro.Fits
open Generators

let parseText (strictness: Strictness) (lines: string list) : Result<ParsedHeader, Issue list> =
    let text = lines |> List.map line |> String.concat ""
    let padded = text.PadRight(int (Block.padded (int64 text.Length)))
    Header.parse strictness 0 0L (ReadOnlySpan(Encoding.Latin1.GetBytes padded))

let minimal = [
    "SIMPLE  =                    T"
    "BITPIX  =                    8"
    "NAXIS   =                    0"
]

[<Fact>]
let ``parses a minimal header`` () =
    let parsed = parseText Strict (minimal @ [ "END" ]) |> expectOk
    Assert.Equal(2880, parsed.Length)
    Assert.Empty parsed.Warnings
    Assert.Equal<string list>([ "SIMPLE"; "BITPIX"; "NAXIS" ], Header.keywords parsed.Header)
    Assert.Equal(Some true, Header.tryBool "SIMPLE" parsed.Header)
    Assert.Equal(Some 8L, Header.tryInt "bitpix" parsed.Header)

[<Fact>]
let ``merges long strings`` () =
    let parsed =
        parseText
            Strict
            (minimal
             @ [
                 "LONG    = 'abc&'"
                 "CONTINUE  'def&' / first"
                 "CONTINUE  '' / second"
                 "END"
             ])
        |> expectOk

    Assert.Equal(
        Some(Card.Value("LONG", CardValue.String "abcdef", Some "firstsecond")),
        Header.tryFind "LONG" parsed.Header
    )
    Assert.Equal(4, parsed.Header.Count)

[<Fact>]
let ``keeps an ampersand that is not continued`` () =
    let parsed =
        parseText Strict (minimal @ [ "AMP     = 'abc&'"; "NEXT    = 1"; "END" ])
        |> expectOk
    Assert.Equal(Some "abc&", Header.tryString "AMP" parsed.Header)

[<Fact>]
let ``fails without END`` () =
    let issues = parseText Strict minimal |> expectError
    Assert.Equal<Violation list>([ Violation.MissingEnd ], issues |> List.map (fun i -> i.Violation))

[<Fact>]
let ``strict fails on a bad card and locates it`` () =
    let issues = parseText Strict (minimal @ [ "bad     = 1"; "END" ]) |> expectError
    let issue = Assert.Single issues
    Assert.Equal(Violation.InvalidKeyword "bad", issue.Violation)
    Assert.Equal(Some 3, issue.Card)
    Assert.Equal(Some 240L, issue.Offset)
    Assert.Equal(Some 0, issue.Hdu)

[<Fact>]
let ``lenient warns on a bad card and keeps it`` () =
    let parsed = parseText Lenient (minimal @ [ "bad     = 1"; "END" ]) |> expectOk
    Assert.Equal(Some 1L, Header.tryInt "bad" parsed.Header)
    Assert.Equal<Violation list>([ Violation.InvalidKeyword "bad" ], parsed.Warnings |> List.map (fun i -> i.Violation))

[<Fact>]
let ``lenient keeps an unparseable card as raw`` () =
    let parsed = parseText Lenient (minimal @ [ "X       = @@@"; "END" ]) |> expectOk
    Assert.Contains(Card.Raw(line "X       = @@@"), parsed.Header.Cards)

[<Fact>]
let ``reports garbage after END`` () =
    let issues = parseText Strict (minimal @ [ "END"; "junk" ]) |> expectError
    Assert.Equal<Violation list>([ Violation.GarbageAfterEnd ], issues |> List.map (fun i -> i.Violation))

[<Fact>]
let ``reports a dangling CONTINUE`` () =
    let issues = parseText Strict (minimal @ [ "CONTINUE  'x'"; "END" ]) |> expectError
    Assert.Equal<Violation list>([ Violation.DanglingContinue ], issues |> List.map (fun i -> i.Violation))

[<Fact>]
let ``strict collects every violation before failing`` () =
    let issues =
        parseText Strict (minimal @ [ "bad     = 1"; "X       = 1.0e3"; "END" ])
        |> expectError
    Assert.Equal(2, issues.Length)

[<Fact>]
let ``serializes to whole blocks with END`` () =
    let header = Header.ofCards [ Card.Value("SIMPLE", CardValue.Logical true, None) ]
    let bytes = Header.toBytes header
    Assert.Equal(2880, bytes.Length)
    Assert.Equal(line "SIMPLE  =                    T", Encoding.ASCII.GetString(bytes, 0, 80))
    Assert.Equal(line "END", Encoding.ASCII.GetString(bytes, 80, 80))
    Assert.True(bytes |> Array.skip 160 |> Array.forall (fun b -> b = 0x20uy))

[<Fact>]
let ``unchanged cards are written back byte for byte`` () =
    let odd = line "ODD     =   1.0D+00  / weird spacing"
    let parsed = parseText Strict (minimal @ [ odd; "END" ]) |> expectOk
    let bytes = Header.toBytes parsed.Header
    Assert.Equal(odd, Encoding.ASCII.GetString(bytes, 240, 80))

[<Fact>]
let ``modified cards are reformatted`` () =
    let parsed =
        parseText Strict (minimal @ [ "ODD     =   1.0D+00  / weird spacing"; "END" ])
        |> expectOk
    let bytes = Header.toBytes (Header.set "ODD" (CardValue.Real 2.0) parsed.Header)
    Assert.Equal(line "ODD     =                  2.0 / weird spacing", Encoding.ASCII.GetString(bytes, 240, 80))

[<Fact>]
let ``set replaces in place and appends when absent`` () =
    let header =
        Header.ofCards [
            Card.Value("A", CardValue.Integer 1L, Some "a")
            Card.Value("B", CardValue.Integer 2L, None)
        ]
    let updated =
        header
        |> Header.set "a" (CardValue.Integer 10L)
        |> Header.set "C" (CardValue.Integer 3L)

    Assert.Equal<Card list>(
        [
            Card.Value("a", CardValue.Integer 10L, Some "a")
            Card.Value("B", CardValue.Integer 2L, None)
            Card.Value("C", CardValue.Integer 3L, None)
        ],
        updated.Cards
    )

[<Fact>]
let ``remove drops every card with the keyword`` () =
    let header =
        Header.ofCards [
            Card.Value("A", CardValue.Integer 1L, None)
            Card.Commentary("COMMENT", "x")
            Card.Value("A", CardValue.Integer 2L, None)
        ]
    Assert.Equal<Card list>([ Card.Commentary("COMMENT", "x") ], (Header.remove "A" header).Cards)

[<Fact>]
let ``serialized headers are whole blocks of restricted ASCII`` () =
    check (
        forAll
            cards
            (fun cards ->
                match Header.tryToBytes (Header.ofCards cards) with
                | Ok bytes ->
                    bytes.Length % Block.Size = 0
                    && bytes |> Array.forall (fun b -> b >= 0x20uy && b <= 0x7Euy)
                | Error issues when issues |> List.forall (fun i -> unrepresentable i.Violation) -> true
                | Error issues -> failwith $"unexpected %A{issues}"
            )
    )

[<Fact>]
let ``parsing a serialized header gives the cards back`` () =
    check (
        forAll
            cards
            (fun cards ->
                let header = Header.ofCards cards

                match Header.tryToBytes header with
                | Error issues when issues |> List.forall (fun i -> unrepresentable i.Violation) -> true
                | Error issues -> failwith $"unexpected %A{issues}"
                | Ok bytes ->
                    match Header.parse Strict 0 0L (ReadOnlySpan bytes) with
                    | Ok parsed ->
                        if parsed.Header <> header then
                            failwith (firstMismatch header.Cards parsed.Header.Cards)

                        parsed.Length = bytes.Length && List.isEmpty parsed.Warnings
                    | Error issues -> failwith $"parse failed: %A{issues}"
            )
    )

[<Fact>]
let ``long strings with long comments survive a round trip`` () =
    let gen = gen {
        let! value = asciiString 300 |> Gen.map (fun s -> s.TrimEnd ' ')
        let! comment =
            asciiString 200
            |> Gen.map (fun s -> s.Trim ' ')
            |> Gen.filter (fun s -> s <> "")
        return Card.Value("LONG", CardValue.String value, Some comment)
    }

    check (
        forAll
            gen
            (fun card ->
                let header = Header.ofCards [ card ]
                let bytes = Header.toBytes header

                match Header.parse Strict 0 0L (ReadOnlySpan bytes) with
                | Ok parsed ->
                    if parsed.Header.Cards <> [ card ] then
                        failwith (firstMismatch [ card ] parsed.Header.Cards)

                    true
                | Error issues -> failwith $"parse failed: %A{issues}"
            )
    )
