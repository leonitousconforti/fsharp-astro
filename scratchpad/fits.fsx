// Scratchpad over the FITS library. Loads the sources straight from src/, so
// there is nothing to build first, opens the file named on the command line
// leniently and prints what is in it. Replace the bottom half with whatever the
// day's question is.
//
//   dotnet fsi --nowarn:3535,3536,3886 scratchpad/fits.fsx <file.fits>
//
// The flag quiets diagnostics from the loaded sources that their project files
// already silence; a #nowarn here would only cover this file.

// Keep in compile order with src/FSharp.Astro.Units/Units.fsproj
#load "../src/FSharp.Astro.Units/Units.fs"
#load "../src/FSharp.Astro.Units/Convert.fs"
#load "../src/FSharp.Astro.Units/Polynomial.fs"
#load "../src/FSharp.Astro.Units/Constants.fs"
#load "../src/FSharp.Astro.Units/Time.fs"
#load "../src/FSharp.Astro.Units/Angle.fs"
#load "../src/FSharp.Astro.Units/Sexagesimal.fs"
#load "../src/FSharp.Astro.Units/Spherical.fs"
#load "../src/FSharp.Astro.Units/Instant.fs"
#load "../src/FSharp.Astro.Units/Epoch.fs"
#load "../src/FSharp.Astro.Units/Length.fs"
#load "../src/FSharp.Astro.Units/Mass.fs"
#load "../src/FSharp.Astro.Units/Energy.fs"
#load "../src/FSharp.Astro.Units/Luminosity.fs"
#load "../src/FSharp.Astro.Units/Frequency.fs"
#load "../src/FSharp.Astro.Units/FluxDensity.fs"
#load "../src/FSharp.Astro.Units/Spectral.fs"
#load "../src/FSharp.Astro.Units/Doppler.fs"
#load "../src/FSharp.Astro.Units/Planck.fs"
#load "../src/FSharp.Astro.Units/Magnitude.fs"
#load "../src/FSharp.Astro.Units/Photometry.fs"
#load "../src/FSharp.Astro.Units/Extinction.fs"
#load "../src/FSharp.Astro.Units/ProperMotion.fs"

// Keep in compile order with src/FSharp.Astro.Fits/Fits.fsproj
#load "../src/FSharp.Astro.Fits/Block.fs"
#load "../src/FSharp.Astro.Fits/Errors.fs"
#load "../src/FSharp.Astro.Fits/Unit.fs"
#load "../src/FSharp.Astro.Fits/Card.fs"
#load "../src/FSharp.Astro.Fits/Header.fs"
#load "../src/FSharp.Astro.Fits/Hdu.fs"
#load "../src/FSharp.Astro.Fits/Image.fs"
#load "../src/FSharp.Astro.Fits/Group.fs"
#load "../src/FSharp.Astro.Fits/Table.fs"
#load "../src/FSharp.Astro.Fits/Ascii.fs"
#load "../src/FSharp.Astro.Fits/Rice.fs"
#load "../src/FSharp.Astro.Fits/Compress.fs"
#load "../src/FSharp.Astro.Fits/Decode.fs"
#load "../src/FSharp.Astro.Fits/Record.fs"
#load "../src/FSharp.Astro.Fits/Measure.fs"
#load "../src/FSharp.Astro.Fits/Checksum.fs"
#load "../src/FSharp.Astro.Fits/Source.fs"
#load "../src/FSharp.Astro.Fits/Fits.fs"

open FSharp.Astro.Fits

match fsi.CommandLineArgs |> Array.tryItem 1 with
| None -> eprintfn "usage: dotnet fsi scratchpad/fits.fsx <file.fits>"
| Some path ->
    match Fits.tryOpenFileWith FitsOptions.Lenient path with
    | Error error -> eprintfn "%s" (FitsError.describe error)
    | Ok file ->
        use file = file

        for hdu in file.Hdus do
            let name = hdu.Name |> Option.map (fun n -> $" '{n}'") |> Option.defaultValue ""

            printfn
                "HDU %d: %A%s BITPIX=%d axes=%A data=%d bytes"
                hdu.Index
                hdu.Kind
                name
                hdu.BitPix.Code
                hdu.Axes
                hdu.DataLength

        for warning in file.Warnings do
            printfn "warning: %s" (Issue.describe warning)
