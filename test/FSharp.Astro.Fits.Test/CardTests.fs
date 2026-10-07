module CardTests

open System
open Xunit
open FSharp.Astro.Fits
open Generators

let parseOk (text: string) : Card =
    let card, violations = Card.parse (line text)
    Assert.Empty violations
    card

[<Fact>]
let ``parses a logical`` () =
    Assert.Equal(
        Card.Value("SIMPLE", CardValue.Logical true, Some "file does conform"),
        parseOk "SIMPLE  =                    T / file does conform"
    )

[<Fact>]
let ``parses an integer`` () =
    Assert.Equal(Card.Value("BITPIX", CardValue.Integer -32L, None), parseOk "BITPIX  =                  -32")

[<Fact>]
let ``parses a real with a D exponent`` () =
    Assert.Equal(
        Card.Value("BSCALE", CardValue.Real 1.5e-3, Some "scale"),
        parseOk "BSCALE  =             1.5D-03 / scale"
    )

[<Fact>]
let ``parses a real with no leading digit`` () =
    Assert.Equal(Card.Value("X", CardValue.Real 0.5, None), parseOk "X       = .5")

[<Fact>]
let ``parses free format values`` () =
    Assert.Equal(Card.Value("NAXIS", CardValue.Integer 2L, Some "axes"), parseOk "NAXIS   = 2 / axes")

[<Fact>]
let ``parses a string with doubled quotes and trailing spaces trimmed`` () =
    Assert.Equal(Card.Value("OBJECT", CardValue.String "O'Neil  x", None), parseOk "OBJECT  = 'O''Neil  x   '")

[<Fact>]
let ``parses a string with a slash inside and a comment after`` () =
    Assert.Equal(Card.Value("PATH", CardValue.String "a/b", Some "c/d"), parseOk "PATH    = 'a/b     ' / c/d")

[<Fact>]
let ``parses complex values`` () =
    Assert.Equal(Card.Value("CI", CardValue.ComplexInt(1L, -2L), None), parseOk "CI      = (1, -2)")
    Assert.Equal(Card.Value("CR", CardValue.ComplexReal(1.5, -2.0), Some "c"), parseOk "CR      = (1.5, -2.0) / c")

[<Fact>]
let ``parses an undefined value`` () =
    Assert.Equal(Card.Value("UNDEF", CardValue.Undefined, None), parseOk "UNDEF   =")
    Assert.Equal(Card.Value("UNDEF", CardValue.Undefined, Some "no value"), parseOk "UNDEF   = / no value")

[<Fact>]
let ``parses commentary cards`` () =
    Assert.Equal(Card.Commentary("COMMENT", "hello = world"), parseOk "COMMENT hello = world")
    Assert.Equal(Card.Commentary("HISTORY", "step 1"), parseOk "HISTORY step 1")
    Assert.Equal(Card.Commentary("", "blank"), parseOk "        blank")
    Assert.Equal(Card.Commentary("FOO", "no value indicator"), parseOk "FOO     no value indicator")

[<Fact>]
let ``parses END`` () = Assert.Equal(Card.End, parseOk "END")

[<Fact>]
let ``parses HIERARCH cards`` () =
    Assert.Equal(
        Card.Value("ESO DET DIT", CardValue.Real 0.5, Some "exposure"),
        parseOk "HIERARCH ESO DET DIT = 0.5 / exposure"
    )
    Assert.Equal(Card.Value("ESO INS NAME", CardValue.String "x", None), parseOk "HIERARCH ESO INS NAME = 'x'")

[<Fact>]
let ``parses CONTINUE cards`` () =
    Assert.Equal(Card.Continue("more text", Some "c"), parseOk "CONTINUE  'more text' / c")

[<Fact>]
let ``reports a lowercase keyword`` () =
    let card, violations = Card.parse (line "exptime = 1.0")
    Assert.Equal(Card.Value("exptime", CardValue.Real 1.0, None), card)
    Assert.Equal<Violation list>([ Violation.InvalidKeyword "exptime" ], violations)

[<Fact>]
let ``reports a non-ASCII byte`` () =
    let _, violations = Card.parse (line "COMMENT café")
    Assert.Equal<Violation list>([ Violation.NonAsciiCharacter 12 ], violations)

[<Fact>]
let ``reports an unterminated string`` () =
    let card, violations = Card.parse (line "OBJECT  = 'open")
    Assert.Equal(Card.Value("OBJECT", CardValue.String "open", None), card)
    Assert.Equal<Violation list>([ Violation.UnterminatedString ], violations)

[<Fact>]
let ``reports a lowercase exponent but still parses it`` () =
    let card, violations = Card.parse (line "X       = 1.0e3")
    Assert.Equal(Card.Value("X", CardValue.Real 1000.0, None), card)
    Assert.Equal<Violation list>([ Violation.NonStandardNumber "1.0e3" ], violations)

[<Fact>]
let ``keeps an unparseable value as raw`` () =
    let text = line "X       = @@@"
    let card, violations = Card.parse text
    Assert.Equal(Card.Raw text, card)
    Assert.Equal<Violation list>([ Violation.InvalidValue "@@@" ], violations)

[<Fact>]
let ``reports garbage after a value`` () =
    let card, violations = Card.parse (line "X       = 1 junk")
    Assert.Equal(Card.Value("X", CardValue.Integer 1L, Some "junk"), card)
    Assert.Equal<Violation list>([ Violation.InvalidValue "junk" ], violations)

[<Fact>]
let ``reports a malformed END`` () =
    let card, violations = Card.parse (line "END     junk")
    Assert.Equal(Card.End, card)
    Assert.Equal<Violation list>([ Violation.MalformedEnd ], violations)

[<Fact>]
let ``formats fixed format values`` () =
    Assert.Equal(
        line "SIMPLE  =                    T / conforms",
        Card.format (Card.Value("SIMPLE", CardValue.Logical true, Some "conforms"))
    )
    Assert.Equal(
        line "BITPIX  =                  -32",
        Card.format (Card.Value("BITPIX", CardValue.Integer -32L, None))
    )
    Assert.Equal(line "OBJECT  = 'M31     '", Card.format (Card.Value("OBJECT", CardValue.String "M31", None)))
    Assert.Equal(line "BSCALE  =                  1.0", Card.format (Card.Value("BSCALE", CardValue.Real 1.0, None)))
    Assert.Equal(line "TINY    =              1.0E-05", Card.format (Card.Value("TINY", CardValue.Real 1e-5, None)))
    Assert.Equal(line "END", Card.format Card.End)

[<Fact>]
let ``formats HIERARCH keywords`` () =
    Assert.Equal(
        line "HIERARCH ESO DET DIT = 0.5 / exposure",
        Card.format (Card.Value("ESO DET DIT", CardValue.Real 0.5, Some "exposure"))
    )

[<Fact>]
let ``formats a long string onto CONTINUE cards`` () =
    let value = String('x', 150)
    let formatted =
        Card.format (Card.Value("LONG", CardValue.String value, Some "note"))
    Assert.Equal(240, formatted.Length)
    Assert.StartsWith("LONG    = '" + String('x', 67) + "&'", formatted)
    Assert.StartsWith("CONTINUE  '" + String('x', 67) + "&'", formatted.Substring 80)
    Assert.Equal(line ("CONTINUE  '" + String('x', 16) + "' / note"), formatted.Substring 160)

[<Fact>]
let ``formats a long commentary onto several cards`` () =
    let formatted = Card.format (Card.Commentary("COMMENT", String('a', 100)))
    Assert.Equal(160, formatted.Length)
    Assert.Equal("COMMENT " + String('a', 72), formatted.Substring(0, 80))
    Assert.Equal(line ("COMMENT " + String('a', 28)), formatted.Substring 80)

[<Fact>]
let ``refuses what the standard cannot represent`` () =
    Assert.Equal(Error Violation.NonFiniteReal, Card.tryFormat (Card.Value("X", CardValue.Real nan, None)))
    Assert.Equal(
        Error(Violation.InvalidKeyword "BAD=KEY"),
        Card.tryFormat (Card.Value("BAD=KEY", CardValue.Integer 1L, None))
    )
    Assert.Equal(
        Error(Violation.CommentTooLong "X"),
        Card.tryFormat (Card.Value("X", CardValue.Integer 1L, Some(String('c', 60))))
    )
    Assert.Equal(Error(Violation.NonAsciiCharacter 11), Card.tryFormat (Card.Value("X", CardValue.String "café", None)))

[<Fact>]
let ``formatted single cards are exactly 80 restricted ASCII characters`` () =
    check (
        forAll
            card
            (fun card ->
                match Card.tryFormat card with
                | Ok formatted ->
                    formatted.Length % 80 = 0
                    && formatted.Length > 0
                    && formatted |> String.forall (fun c -> c >= ' ' && c <= '~')
                | Error e when unrepresentable e -> true
                | Error e -> failwith $"unexpected %A{e}"
            )
    )

[<Fact>]
let ``parsing a formatted single card gives the card back`` () =
    check (
        forAll
            card
            (fun card ->
                match Card.tryFormat card with
                | Ok formatted when formatted.Length = 80 ->
                    let parsed, violations = Card.parse formatted

                    if parsed <> card || not (List.isEmpty violations) then
                        failwith $"parsed %A{parsed} with %A{violations} from \"{formatted}\""

                    true
                | Ok _ -> true
                | Error e when unrepresentable e -> true
                | Error e -> failwith $"unexpected %A{e}"
            )
    )

[<Fact>]
let ``a commentary card that would read back as a value is refused`` () =
    // Under a keyword that is not COMMENT, HISTORY or blank, '= ' in bytes nine and ten makes a
    // value card. The padding supplies the space, so text of exactly "=" is ambiguous too.
    Assert.Equal(Error(Violation.AmbiguousCommentary "NOTE"), Card.tryFormat (Card.Commentary("NOTE", "=")))

    Assert.Equal(Error(Violation.AmbiguousCommentary "NOTE"), Card.tryFormat (Card.Commentary("NOTE", "= something")))

    // A later chunk of a wrapped card lands in the same columns, so it is checked too.
    let wrapped = String('x', 72) + "= tail"
    Assert.Equal(Error(Violation.AmbiguousCommentary "NOTE"), Card.tryFormat (Card.Commentary("NOTE", wrapped)))

    // The reserved commentary keywords are read as commentary whatever follows them.
    for keyword in [ ""; "COMMENT"; "HISTORY" ] do
        let card = Card.Commentary(keyword, "= still a comment")
        let image = Card.tryFormat card |> expectOk
        Assert.Equal(card, fst (Card.parse image))

    // Anything that is not the value indicator is still fine.
    let fine = Card.Commentary("NOTE", "=x")
    Assert.Equal(fine, fst (Card.parse (Card.tryFormat fine |> expectOk)))
