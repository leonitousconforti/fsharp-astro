# FSharp.Astro.Fits

A FITS (Flexible Image Transport System) library for F# on .NET 10. Depends on `FSharp.Astro.Units`,
which the two use to give a column its unit of measure, and on nothing else beyond FSharp.Core.

FSharp.Astro.Fits is strict by default: a file that violates the FITS 4.0 standard fails to open, and the
error says which HDU, card and byte offset is at fault. Lenient handling is an explicit opt-in for
files from the wild.

## What works today

- Header cards: every value type, fixed and free format, long strings via CONTINUE, HIERARCH keywords.
- Headers round-trip byte for byte. A card read from disk is written back unchanged unless you modify it.
- Images: all six BITPIX types, BZERO and BSCALE scaling, BLANK, unsigned 16 and 32-bit conventions.
- Binary tables: every TFORM type including bits, complex values and variable-length arrays in the heap.
- ASCII tables: TBCOLn and the Fortran A, I, F, E and D edit descriptors, reading and writing.
- Random groups: the parameters and array of every group, with PSCALn and PZEROn scaling.
- Cutout reads: a sub-region of an image without reading the rest of the data unit.
- Tile compressed images (ZIMAGE): reading RICE_1, GZIP_1, GZIP_2 and NOCOMPRESS.
- CHECKSUM and DATASUM, both writing and verification.
- Reading is lazy. Opening a file parses every header and reads no data until asked.
- Decoders and record mapping for headers and table rows, and an erased type provider.
- TUNITn and BUNIT parsed, converted between, and turned into F# units of measure.

Not yet: writing a compressed image, the quantized float form of one, HCompress and PLIO, in-place
header update. See `TODO.md`.

## Reading

```fsharp
open FSharp.Astro.Fits

use file = Fits.openFile "image.fits"

for hdu in file.Hdus do
    printfn "%d %A %A" hdu.Index hdu.Kind hdu.Axes

let image = Fits.readImage file 0
let pixels = Image.toFloat64 image   // BZERO + BSCALE * stored, BLANK as NaN

match image.Data with
| ImageData.Int16 raw -> printfn "stored as int16, %d values" raw.Length
| _ -> ()

let table = Fits.readTable file 1
let flux = Table.tryColumn "FLUX" table |> Option.get

match flux.Data with
| ColumnData.Float32s values -> printfn "%A" values
| _ -> ()
```

An ASCII table, `XTENSION = 'TABLE'`, reads through `Fits.readAsciiTable` and `Ascii.tryColumn`.
Decoding one gives back the same `Column` type a binary table does, so `Table.toFloat64`, record
mapping and `Quantity.column` all work on either kind.

```fsharp
let catalog = Fits.readAsciiTable file 2
let ra = Ascii.tryColumn "RA" catalog |> Option.get
Table.toFloat64 ra                       // TZERO + TSCAL * value, blanks as NaN
```

An `Iw` field decodes to `int64` and `Fw.d`, `Ew.d` and `Dw.d` to `float`, whatever width the field
has. A field that is all blanks, or that holds the TNULLn string, is undefined: `NaN` for a real
and `Ascii.NullInteger` for a whole number, which the decoded column reports as its TNULL so that
the usual scaling and option mapping treat it as missing. The reader accepts the number forms
Fortran writes, including a `D` exponent, an exponent with the letter left out as in `1.5+02`, and
a value with no decimal point at all, where the last `d` digits are the fraction.

Shapes are reported in C order. The last entry of `image.Shape` is NAXIS1, which varies fastest on
disk, so a flat index into `image.Data` is row-major over the shape.

`Fits.readCutout` takes a sub-region without reading the rest of the data unit. `start` and the
shape are in C order like `Image.Shape`, and the result is an image carrying the scaling keywords
of the HDU, so the usual functions apply to it.

```fsharp
let stamp = Fits.readCutout file 0 [| 2040; 2040 |] [| 16; 16 |]
Image.toFloat64 stamp                    // 256 pixels, 256 pixels read
```

One line along the fastest axis is contiguous on disk, so a cutout costs one read per line of the
region. `Image.regionRuns` is the arithmetic behind it, should you want the offsets yourself.

An image that fpack compressed is a binary table under the tiled compression convention, and
`Fits.readImage` sees through it. Nothing above has to know which kind of HDU it was.

```fsharp
let image = Fits.readImage file 1       // the image, whether or not it arrived compressed
Compress.isCompressedImage file[1]      // true when it did

let compressed = Fits.readCompressedImage file 1
compressed.Compression                  // TileCompression.Rice (32, 2)
compressed.TileCount                    // tiles, each a row of the table
Compress.tile 0 compressed              // one tile, decompressed
Compress.imageHeader compressed         // the header with the Z machinery taken off
```

RICE_1, GZIP_1, GZIP_2 and NOCOMPRESS are read. The Rice coder is checked against files fpack
wrote, both ways: it recovers their pixels and it writes back the bytes they hold. HCompress and
PLIO are not implemented, and neither is the quantized form fpack stores a floating point image in,
where ZSCALE and ZZERO turn integers back into floats per tile. A file this library cannot read
says so rather than handing back pixels it guessed at.

The legacy random groups layout of a primary HDU reads through `Fits.readGroups`. Each group's
array comes back as an `Image`, so nothing new has to be learned to use one.

```fsharp
let groups = Fits.readGroups file 0
groups.Count                             // GCOUNT
Groups.parameters 0 groups               // PZEROn + PSCALn * stored, one per PTYPEn
Groups.image 0 groups                    // that group's array, with BZERO and BSCALE
Groups.tryParameterValues "DATE" groups  // every group, summing parameters that share the name
```

Every reading function has a `try` variant that returns `Result<_, FitsError>` instead of throwing
`FitsException`. Pass `FitsOptions.Lenient` to accept recoverable violations and collect them in
`file.Warnings`.

## Writing

```fsharp
let image = Image.create [| 2; 3 |] (ImageData.Float32 [| 1.f; 2.f; 3.f; 4.f; 5.f; 6.f |])
let header = Header.empty |> Header.setWithComment "OBJECT" (CardValue.String "M31") "target"

let catalog =
    Fits.binaryTable
        (Header.set "EXTNAME" (CardValue.String "CAT") Header.empty)
        [ Table.ofInt32s "ID" [| 1; 2 |]
          Table.ofStrings "NAME" 8 [| "alpha"; "beta" |]
          Table.ofFloat32s "FLUX" [| 1.5f; 2.5f |] |> Table.withUnit "Jy" ]

Fits.writeFile "out.fits" [ Fits.withChecksum (Fits.primary header image); catalog ]
```

Mandatory keywords are generated from the data and placed first. Any copies in the header you pass
are replaced. The writer validates every card and refuses anything the standard cannot represent.

An ASCII table is written the same way. Fields are laid out left to right with one blank between,
and TBCOLn is written from that layout.

```fsharp
let sources =
    Fits.asciiTable (Header.set "EXTNAME" (CardValue.String "CAT") Header.empty)
        [ Ascii.ofStrings "NAME" 8 [| "alpha"; "beta" |]
          Ascii.ofInt64s "ID" 4 [| 1L; 2L |]
          Ascii.ofFloat64s "RA" 8 3 [| 10.5; 266.417 |] |> Ascii.withUnit "deg"
          Ascii.ofExponentials "FLUX" 11 4 [| 1.5e-3; 2.0 |] |> Ascii.withUnit "Jy" ]
```

The standard has no unsigned integer or signed byte column. Table 18 of the standard instead
offsets the value and says so with TZEROn, which `Table.ofUInt16s` and friends write and
`Table.toUInt16` and friends read back, the same way the image functions do.

```fsharp
Table.ofUInt16s "COUNTS" [| 0us; 65535us |]   // stored as int16 with TZERO = 32768
Table.isUInt16 column                         // true for a column written that way
Table.toUInt16 column                         // uint16[]
```

`ofInt8s`, `ofUInt32s` and `ofUInt64s` cover the other three conventions.

`Fits.randomGroups` writes the legacy layout. It asks for groups already in stored form, since
nothing new should be written this way and there is no point dressing it up.

```fsharp
let uv =
    Fits.randomGroups Header.empty [| 2; 3 |]
        [ Groups.parameterInfo "UU"; Groups.parameterInfo "VV" ]
        [ { Parameters = ImageData.Float32 [| 0.f; 1.f |]
            Data = ImageData.Float32 (Array.zeroCreate 6) } ]
```

## Decoding headers into records

Decoders read typed values and report every missing or mistyped keyword at once. `and!` runs the
decoders independently; a chain of `let!` stops at the first failure.

```fsharp
type Observation = { Target: string; ExpTime: float; Filter: string option }

let observation =
    decode {
        let! target = Decode.string "OBJECT"
        and! expTime = Decode.float "EXPTIME"
        and! filter = Decode.optional (Decode.string "FILTER")
        return { Target = target; ExpTime = expTime; Filter = filter }
    }

match Decode.run observation file.Primary.Header with
| Ok obs -> printfn "%s for %gs" obs.Target obs.ExpTime
| Error issues -> issues |> List.iter (Issue.describe >> eprintfn "%s")
```

A record can declare its own decoder through `IFitsRecord` and be read with `Fits.readAs<Observation> file 0`.

For scripts, let the record shape do the mapping. Fields map to the keyword of the same name in
upper case, `[<Keyword>]` overrides that, and `option` marks a keyword that may be absent.

```fsharp
type Observation = { [<Keyword "OBJECT">] Target: string; ExpTime: float; Filter: string option }
type Star = { Id: int; Name: string; Flux: float32; Mag: float option; Spec: float32[] }

let obs = Record.ofHeader<Observation> file.Primary.Header
let stars = Fits.readRecords<Star> file 1
```

Table fields map to columns by name, with `[<Column>]` to override. A float field on an integer
column gets physical values with TSCAL and TZERO applied, an option field turns TNULL into None,
and array fields take multi-element and variable-length cells. Mismatches are reported for every
field together, before any row is built.

A field of rank two or more is filled from TDIMn, which declares a cell's dimensions fastest axis
first, so `TDIM1 = '(3,2)'` over a repeat of six gives a `float32[,]` of two rows of three. A field
typed `uint16`, `uint32`, `uint64` or `sbyte` reads a column written with the matching TZEROn
offset. `Table.cellShape` gives the shape on its own, and `Table.cellImage` hands one cell back as
an image.

## Type provider

`FitsProvider` is an erased type provider that reads a sample at compile time and gives every HDU,
keyword and column a typed property, with the keyword comment as documentation. It ships inside
this package, so referencing `FSharp.Astro.Fits` is all it takes; there is no separate package and
nothing to add to a project file.

```fsharp
open FSharp.Astro.Fits.Typed

[<Literal>]
let Sample = __SOURCE_DIRECTORY__ + "/fixtures/nircam.fits"

type Obs = FitsProvider<Sample>

use file = Obs.Load "jw01234_cal.fits"
file.Primary.EXPTIME          // float
file.SCI.Data                 // float32[], from BITPIX
for star in file.CAT.Rows do
    printfn "%s %f" star.NAME star.FLUX
file.CAT.FLUX                 // the whole column, float32<Jy>[] when TUNIT3 is 'Jy'
```

Both kinds of table are typed. An ASCII table gets the same `Rows`, `Row` and per-column members a
binary one does; an `Iw` field comes through as `int64` and the `F`, `E` and `D` forms as `float`,
whatever width they have.

HDUs are found by EXTNAME at runtime, so a reordered file still works. `Load` takes a path, an
http or https URL, or a stream; `AsyncLoad` downloads without blocking; `Parse` takes bytes; and
`GetSample()` opens the sample the type came from, when that sample is a FITS file rather than a
text dump. Otherwise the sample is never needed at runtime. Static parameters:

- `Sample`: a FITS file or a text header dump, or several separated by semicolons. Keywords absent
  from some samples become `option`, and an integer in one and a real in another widens to `float`.
- `Schema`: additions or overrides as `"HDU.KEYWORD: type [option]"`, for example
  `"Primary.GAIN: float option"`.
- `Lenient`: open non-conforming samples and files.
- `ResolutionFolder`: base for relative paths, defaulting to the project directory.

A floating point column whose TUNITn names a measure `FSharp.Astro.Units` declares is provided
with that measure: `'Jy'` gives `float32<Jy>[]`, `'erg s-1 cm-2 Angstrom-1'` gives
`float32<erg/(s cm^2 angstrom)>[]`. `Pixels` takes its measure from BUNIT the same way. A unit
nothing declares a measure for, such as `'Jy/beam'`, a unit with a numeric factor, such as
`'10**(-26) W/(m**2 Hz)'`, and a column that TSCALn and TZEROn still have to be applied to are all
left as bare numbers. Measures are erased, so none of this costs anything at runtime.

The provider is split in two, the way the compiler expects. The half a compiled program needs, the
handles every provided property erases to, sits in `lib/` beside the core assembly. The half only
the compiler runs sits in `typeproviders/fsharp41/`, so nothing a program runs ever loads the type
provider machinery.

The provider targets .NET 10 and loads in the .NET 10 compiler. Editors whose F# language service
runs on an older runtime cannot load it, and would need a netstandard2.0 build of the core first.

## Headers

```fsharp
let header = file.Primary.Header
Header.tryFloat "EXPTIME" header      // Some 30.0
Header.getString "OBJECT" header      // throws when missing
header["NAXIS1"]                      // CardValue.Integer 1024L

header
|> Header.set "EXPTIME" (CardValue.Real 60.0)   // keeps the existing comment
|> Header.addHistory "doubled exposure"
|> Header.remove "OBSOLETE"
```

Headers are immutable. Keyword lookup ignores case.

## Units

TUNITn, BUNIT and the bracketed unit of a comment field are written in the grammar of section 4.3
of the standard. `UnitExpr` parses it, `Measure` says what the symbols are worth, and `Quantity`
reads a column or an image in a unit the compiler checks.

```fsharp
open FSharp.Astro.Fits
open FSharp.Astro.Units

UnitExpr.tryParse "10**(-20) J/(s m**2 Hz)"   // the standard's own example
Measure.tryFactor "mJy" "Jy"                  // Some 0.001
Measure.tryFactor "erg s-1 cm-2" "W/m**2"     // Some 0.001, the IAU style parses too
Measure.compatible "Jy" "K"                   // false

let flux: float<Jy>[] = Quantity.column Tag.Jy (Table.tryColumn "FLUX" table |> Option.get)
let pixels: float<Jy>[] = Quantity.image Tag.Jy hdu.Header image
```

`Quantity.column` applies TSCALn and TZEROn, converts from TUNITn, and tags the result, so a
column stored in `'mJy'` comes back as `float<Jy>` when that is what you asked for. It fails when
the column declares no unit or one of another quantity; `tryColumn` returns `None` instead.
`Tag.create` makes a tag for a measure of your own.

A symbol nothing here knows stands for itself, which is what makes `'Jy/beam'` and `'mJy/beam'`
convertible without anyone having to say what a beam is.

## Building

This package lives in the [fsharp-astro](https://github.com/leonitousconforti/fsharp-astro)
repository alongside `FSharp.Astro.Units`. The repository README covers building and testing.
