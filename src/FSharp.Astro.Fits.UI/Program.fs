open System
open FSharp.Astro.Fits

match Environment.GetCommandLineArgs() |> Array.tryItem 1 with
| None -> printfn "usage: FSharp.Astro.Fits.UI <file.fits>"
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
