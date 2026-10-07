module AngleTests

open System
open Xunit
open FSharp.Astro.Units
open Helpers

[<Fact>]
let ``a degree is 3600 arcseconds`` () =
    close 1e-15 3600.0<arcsec> (Angle.convert Angle.radiansPerDegree Angle.radiansPerArcsecond 1.0<deg>)

[<Fact>]
let ``an arcsecond in radians`` () =
    close 1e-15 4.84813681109536e-6<rad> (1.0<arcsec> * Angle.radiansPerArcsecond)

[<Fact>]
let ``a milliarcsecond is a thousandth of an arcsecond`` () =
    close 1e-15 1e-3<arcsec> (Angle.convert Angle.radiansPerMilliarcsecond Angle.radiansPerArcsecond 1.0<mas>)

[<Fact>]
let ``a microarcsecond is a millionth of an arcsecond`` () =
    close 1e-15 1e-6<arcsec> (Angle.convert Angle.radiansPerMicroarcsecond Angle.radiansPerArcsecond 1.0<uas>)

[<Fact>]
let ``an hour of right ascension is fifteen degrees`` () =
    close 1e-15 15.0<deg> (Angle.convert Angle.radiansPerHourAngle Angle.radiansPerDegree 1.0<hourangle>)

[<Fact>]
let ``a full circle is two pi radians`` () =
    close 1e-15 (2.0 * Math.PI * 1.0<rad>) (360.0<deg> * Angle.radiansPerDegree)

[<Fact>]
let ``a square degree in steradians`` () =
    let side = 1.0<deg> * Angle.radiansPerDegree
    let solidAngle: float<sr> = side * side
    close 1e-15 3.0461741978670857e-4<sr> solidAngle

[<Fact>]
let ``wrap brings angles into the principal range`` () =
    Assert.Equal(330.0<deg>, Angle.wrap 360.0<deg> -30.0<deg>)
    Assert.Equal(0.0<deg>, Angle.wrap 360.0<deg> 720.0<deg>)
    Assert.Equal(1.0<hourangle>, Angle.wrap 24.0<hourangle> 25.0<hourangle>)

[<Fact>]
let ``wrapSigned centres the range on zero`` () =
    Assert.Equal(-90.0<deg>, Angle.wrapSigned 360.0<deg> 270.0<deg>)
    Assert.Equal(-180.0<deg>, Angle.wrapSigned 360.0<deg> 180.0<deg>)
    Assert.Equal(90.0<deg>, Angle.wrapSigned 360.0<deg> 90.0<deg>)

[<Fact>]
let ``wrap lands in [0, period) and is idempotent`` () =
    check (
        forAll
            signed
            (fun x ->
                let angle = x * 1.0<deg>
                let wrapped = Angle.wrap 360.0<deg> angle
                wrapped >= 0.0<deg>
                && wrapped < 360.0<deg>
                && Angle.wrap 360.0<deg> wrapped = wrapped
            )
    )

[<Fact>]
let ``wrap changes an angle by a whole number of turns`` () =
    check (
        forAll
            signed
            (fun x ->
                let angle = x * 1.0<deg>
                let turns = (angle - Angle.wrap 360.0<deg> angle) / 360.0<deg>
                abs (turns - Math.Round turns) < 1e-9
            )
    )

[<Fact>]
let ``wrapSigned lands in [-period/2, period/2)`` () =
    check (
        forAll
            signed
            (fun x ->
                let wrapped = Angle.wrapSigned (2.0 * Math.PI * 1.0<rad>) (x * 1.0<rad>)
                wrapped >= -Math.PI * 1.0<rad> && wrapped < Math.PI * 1.0<rad>
            )
    )

[<Fact>]
let ``trigonometry takes radians`` () =
    close 1e-15 1.0 (Angle.sin (90.0<deg> * Angle.radiansPerDegree))
    close 1e-15 45.0<deg> (Angle.atan2 1.0<pc> 1.0<pc> / Angle.radiansPerDegree)

[<Fact>]
let ``inverse trigonometry round trips`` () =
    check (
        forAll
            unitInterval
            (fun x ->
                // Absolute tolerance: cos (acos 0.0) is 6e-17, not 0, so a relative check fails at zero.
                abs (Angle.sin (Angle.asin x) - x) < 1e-12
                && abs (Angle.cos (Angle.acos x) - x) < 1e-12
                && abs (Angle.tan (Angle.atan x) - x) < 1e-12
            )
    )
