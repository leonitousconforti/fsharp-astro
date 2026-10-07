module SexagesimalTests

open System
open Xunit
open FSharp.Astro.Units
open Helpers

[<Fact>]
let ``formats an hour angle`` () =
    Assert.Equal("05:30:00.00", Sexagesimal.formatHours 2 5.5<hourangle>)
    Assert.Equal("00:00:00", Sexagesimal.formatHours 0 0.0<hourangle>)

[<Fact>]
let ``formats a declination with an explicit sign`` () =
    Assert.Equal("+41:16:09.0", Sexagesimal.formatDegrees 1 (41.0<deg> + 16.0<deg> / 60.0 + 9.0<deg> / 3600.0))
    Assert.Equal("-00:00:01.0", Sexagesimal.formatDegrees 1 (-1.0<deg> / 3600.0))

[<Fact>]
let ``a small negative angle keeps its sign through the zero fields`` () =
    Assert.Equal("-00:00:00", Sexagesimal.formatDegrees 0 (-0.1<deg> / 3600.0))

[<Fact>]
let ``rounding the seconds carries upward instead of writing a sixtieth`` () =
    // 59.96 s to one place is 60.0 s, which does not exist: the minute and then the hour carry.
    let value = 12.0<hourangle> + 59.0<hourangle> / 60.0 + 59.96<hourangle> / 3600.0
    Assert.Equal("13:00:00.0", Sexagesimal.formatHours 1 value)
    Assert.Equal("12:59:59.96", Sexagesimal.formatHours 2 value)

[<Fact>]
let ``the carry runs past the top field rather than wrapping`` () =
    let value = 23.0<hourangle> + 59.0<hourangle> / 60.0 + 59.99<hourangle> / 3600.0
    Assert.Equal("24:00:00.0", Sexagesimal.formatHours 1 value)

[<Fact>]
let ``parses a right ascension`` () =
    // Betelgeuse, 05 55 10.305 +07 24 25.43.
    close 1e-12 5.919529166666667<hourangle> (Sexagesimal.parseHours "05:55:10.305")
    close 1e-12 7.4070638888888885<deg> (Sexagesimal.parseDegrees "+07:24:25.43")

[<Fact>]
let ``parses the separators catalogues actually use`` () =
    let expected = Sexagesimal.parseHours "05:55:10.305"

    for text in [ "05 55 10.305"; "05h55m10.305s"; "5h 55m 10.305s"; "05:55:10.305" ] do
        close 1e-12 expected (Sexagesimal.parseHours text)

    let dec = Sexagesimal.parseDegrees "+07:24:25.43"

    for text in [ "07 24 25.43"; "7d24m25.43s"; "+07°24'25.43\"" ] do
        close 1e-12 dec (Sexagesimal.parseDegrees text)

[<Fact>]
let ``a missing field is zero`` () =
    Assert.Equal(12.0<hourangle>, Sexagesimal.parseHours "12")
    Assert.Equal(12.5<hourangle>, Sexagesimal.parseHours "12:30")

[<Fact>]
let ``the sign applies to the whole angle, not the first field`` () =
    close 1e-12 -0.5<deg> (Sexagesimal.parseDegrees "-00:30:00")
    close 1e-12 -30.5<deg> (Sexagesimal.parseDegrees "-30:30:00")

[<Fact>]
let ``rejects what is not a sexagesimal angle`` () =
    let bad = [
        ""
        "   "
        "abc"
        "12:60:00" // sixtieth of an hour that does not exist
        "12:30:60"
        "12:30.5:00" // only the last field may have a fraction
        "1:2:3:4"
        "12:-30:00"
        "12:30:1e3"
    ]

    for text in bad do
        Assert.True(Option.isNone (Sexagesimal.tryParseDegrees text), $"'{text}' should not parse")

    Assert.Throws<FormatException>(fun () -> Sexagesimal.parseHours "nope" |> ignore)
    |> ignore

[<Fact>]
let ``parts agree with the formatted fields`` () =
    let value = 12.0<hourangle> + 59.0<hourangle> / 60.0 + 59.96<hourangle> / 3600.0
    let parts = Sexagesimal.parts 1 (float value)
    Assert.Equal(1, parts.Sign)
    Assert.Equal(13, parts.Units)
    Assert.Equal(0, parts.Minutes)
    Assert.Equal(0.0, parts.Seconds, 12)

[<Fact>]
let ``formatting then parsing recovers the angle to the written precision`` () =
    check (
        forAll
            signed
            (fun x ->
                let angle = Angle.wrap 24.0<hourangle> (x * 1.0<hourangle>)
                let text = Sexagesimal.formatHours 4 angle

                match Sexagesimal.tryParseHours text with
                // Half of 1e-4 s of time, expressed in hours, is the most the rounding can cost.
                | Some back -> abs (back - angle) <= 0.5e-4<hourangle> / 3600.0 + 1e-12<hourangle>
                | None -> false
            )
    )

[<Fact>]
let ``parsing then formatting reproduces the string`` () =
    check (
        forAll
            signed
            (fun x ->
                let angle = Angle.wrapSigned 360.0<deg> (x * 1.0<deg>)
                let text = Sexagesimal.formatDegrees 3 angle
                Sexagesimal.formatDegrees 3 (Sexagesimal.parseDegrees text) = text
            )
    )
