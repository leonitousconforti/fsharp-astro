/// Infers the shape of a FITS file from one or more samples at design time.
namespace FSharp.Astro.Fits.TypeProvider

open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open FSharp.Astro.Fits

/// The static type a keyword gets.
[<RequireQualifiedAccess>]
type ValueKind =
    | Logical
    | Integer
    | Real
    | String
    | Complex
    /// Different samples disagree, so the property exposes the raw CardValue.
    | Any

type KeywordSchema = {
    Keyword: string
    Kind: ValueKind
    /// Absent from at least one sample.
    Optional: bool
    Comment: string option
}

/// How a column declares itself, which depends on the kind of table it is in.
[<RequireQualifiedAccess>]
type ColumnForm =
    /// TFORMn of a BINTABLE.
    | Binary of TForm
    /// TFORMn of a TABLE, a Fortran edit descriptor.
    | Ascii of AsciiForm

    /// The TFORMn value, as the header writes it.
    member this.Text: string =
        match this with
        | Binary form -> TForm.format form
        | Ascii form -> AsciiForm.format form

type ColumnSchema = {
    Index: int
    Name: string
    Form: ColumnForm
    /// TUNITn.
    Unit: string option
    /// TSCALn.
    Scale: float
    /// TZEROn.
    Zero: float
}

type HduSchema = {
    Index: int
    /// EXTNAME when present, otherwise empty, used to find the HDU at runtime.
    ExtName: string
    /// The property name on the provided file type.
    PropertyName: string
    Kind: HduKind
    Keywords: KeywordSchema list
    BitPix: BitPix option
    /// BUNIT, the unit of the physical pixel values of an image.
    BUnit: string option
    Columns: ColumnSchema list
}

/// Turns keywords and names into identifiers.
module Naming =
    /// A valid identifier for a keyword or column name.
    let identifier (name: string) : string =
        let cleaned =
            name |> String.map (fun c -> if Char.IsLetterOrDigit c then c else '_')

        if cleaned = "" then "_"
        elif Char.IsDigit cleaned[0] then "_" + cleaned
        else cleaned

    /// Makes names unique by appending a counter to repeats.
    let unique (names: string list) : string list =
        let seen = Collections.Generic.Dictionary<string, int>()

        names
        |> List.map (fun name ->
            match seen.TryGetValue name with
            | true, n ->
                seen[name] <- n + 1
                $"{name}_{n + 1}"
            | _ ->
                seen[name] <- 1
                name
        )

module Schema =
    let private descriptorPattern =
        Regex(@"^T(FORM|TYPE|UNIT|NULL|SCAL|ZERO|DIM|DISP|BCOL)[0-9]+$", RegexOptions.CultureInvariant)

    let private kindOf (value: CardValue) : ValueKind option =
        match value with
        | CardValue.Logical _ -> Some ValueKind.Logical
        | CardValue.Integer _ -> Some ValueKind.Integer
        | CardValue.Real _ -> Some ValueKind.Real
        | CardValue.String _ -> Some ValueKind.String
        | CardValue.ComplexInt _
        | CardValue.ComplexReal _ -> Some ValueKind.Complex
        | CardValue.Undefined -> None

    /// The schema of one HDU from its header and layout.
    let ofHeader (index: int) (header: Header) (layout: HduLayout) : HduSchema =
        let keywords =
            header.Cards
            |> List.choose (fun card ->
                match card with
                | Card.Value(keyword, value, comment) when not (descriptorPattern.IsMatch keyword) ->
                    kindOf value
                    |> Option.map (fun kind -> {
                        Keyword = keyword
                        Kind = kind
                        Optional = false
                        Comment = comment
                    })
                | _ -> None
            )
            |> List.distinctBy (fun k -> k.Keyword)

        let columns =
            let fields () =
                Header.tryInt "TFIELDS" header |> Option.defaultValue 0L |> int

            let describe (n: int) (form: ColumnForm) = {
                Index = n - 1
                Name = Header.tryString $"TTYPE{n}" header |> Option.defaultValue $"COL{n}"
                Form = form
                Unit = Header.tryString $"TUNIT{n}" header
                Scale = Header.tryFloat $"TSCAL{n}" header |> Option.defaultValue 1.0
                Zero = Header.tryFloat $"TZERO{n}" header |> Option.defaultValue 0.0
            }

            match layout.Kind with
            | HduKind.BinaryTable -> [
                for n in 1 .. fields () do
                    match Header.tryString $"TFORM{n}" header |> Option.bind TForm.tryParse with
                    | Some form -> describe n (ColumnForm.Binary form)
                    | None -> ()
              ]
            | HduKind.AsciiTable -> [
                for n in 1 .. fields () do
                    match Header.tryString $"TFORM{n}" header |> Option.bind AsciiForm.tryParse with
                    | Some form -> describe n (ColumnForm.Ascii form)
                    | None -> ()
              ]
            | _ -> []

        let extName = Header.tryString "EXTNAME" header |> Option.defaultValue ""

        let propertyName =
            if index = 0 then "Primary"
            elif extName <> "" then Naming.identifier extName
            else $"Hdu{index}"

        {
            Index = index
            ExtName = extName
            PropertyName = propertyName
            Kind = layout.Kind
            Keywords = keywords
            BitPix =
                match layout.Kind with
                | HduKind.Primary
                | HduKind.Image -> Some layout.BitPix
                | _ -> None
            BUnit =
                match layout.Kind with
                | HduKind.Primary
                | HduKind.Image -> Header.tryString "BUNIT" header
                | _ -> None
            Columns = columns
        }

    let private strictness (lenient: bool) = if lenient then Lenient else Strict

    /// Schemas of every HDU in a FITS file.
    let ofFile (lenient: bool) (path: string) : HduSchema list =
        use file =
            Fits.openFileWith (if lenient then FitsOptions.Lenient else FitsOptions.Default) path

        file.Hdus |> List.map (fun hdu -> ofHeader hdu.Index hdu.Header hdu.Layout)

    /// Schemas from a text dump of headers: one card per line, each header ending with END.
    let ofHeaderText (lenient: bool) (text: string) : HduSchema list =
        let lines =
            text.Split([| '\n' |], StringSplitOptions.None)
            |> Array.map (fun l -> l.TrimEnd('\r'))
            |> Array.filter (fun l -> l.Trim() <> "")

        let headers = ResizeArray<string list>()
        let current = ResizeArray<string>()

        for line in lines do
            let card = line.PadRight(Block.CardSize).Substring(0, Block.CardSize)
            current.Add card

            if card.StartsWith "END     " then
                headers.Add(List.ofSeq current)
                current.Clear()

        if current.Count > 0 then
            headers.Add(List.ofSeq current @ [ "END".PadRight Block.CardSize ])

        headers
        |> Seq.mapi (fun index cards ->
            let text = String.concat "" cards
            let padded = text.PadRight(int (Block.padded (int64 text.Length)))
            let bytes = Encoding.Latin1.GetBytes padded

            let header =
                match Header.parse (strictness lenient) index 0L (ReadOnlySpan bytes) with
                | Ok parsed -> parsed.Header
                | Error issues -> raise (FitsException(Violations issues))

            let layout =
                match HduLayout.tryOfHeader (strictness lenient) index header with
                | Ok(layout, _) -> layout
                | Error issues -> raise (FitsException(Violations issues))

            ofHeader index header layout
        )
        |> List.ofSeq

    /// True when the file starts like a FITS file. A FITS file has no line breaks in its first
    /// block, while a text header dump has one after each card.
    let isFitsFile (path: string) : bool =
        let bytes = Array.zeroCreate<byte> Block.Size

        let read =
            use stream = File.OpenRead path
            stream.Read(bytes, 0, bytes.Length)

        let head = Array.sub bytes 0 read
        let isText = head |> Array.exists (fun b -> b = 0x0Auy || b = 0x0Duy)

        not isText
        && read = Block.Size
        && Encoding.ASCII.GetString(head, 0, 8) = "SIMPLE  "

    /// Schemas from a sample path: a FITS file, or a text header dump.
    let ofSample (lenient: bool) (path: string) : HduSchema list =
        if isFitsFile path then
            ofFile lenient path
        else
            ofHeaderText lenient (File.ReadAllText path)

    let private widen (a: ValueKind) (b: ValueKind) : ValueKind =
        match a, b with
        | x, y when x = y -> x
        | ValueKind.Integer, ValueKind.Real
        | ValueKind.Real, ValueKind.Integer -> ValueKind.Real
        | _ -> ValueKind.Any

    let private mergeKeywords (a: KeywordSchema list) (b: KeywordSchema list) : KeywordSchema list =
        let byName (ks: KeywordSchema list) =
            ks |> List.map (fun k -> k.Keyword, k) |> Map.ofList
        let left = byName a
        let right = byName b

        let merged = [
            for k in a do
                match Map.tryFind k.Keyword right with
                | Some other -> {
                    k with
                        Kind = widen k.Kind other.Kind
                        Optional = k.Optional || other.Optional
                  }
                | None -> { k with Optional = true }
            for k in b do
                if not (Map.containsKey k.Keyword left) then
                    { k with Optional = true }
        ]

        merged

    /// Merges the schemas of several samples. HDUs pair up by EXTNAME, or by index when unnamed.
    /// A keyword absent from any sample becomes optional, and conflicting types widen.
    let merge (samples: HduSchema list list) : HduSchema list =
        match samples with
        | [] -> []
        | first :: rest ->
            rest
            |> List.fold
                (fun (acc: HduSchema list) sample ->
                    let matches (a: HduSchema) (b: HduSchema) =
                        if a.ExtName <> "" || b.ExtName <> "" then
                            a.ExtName = b.ExtName
                        else
                            a.Index = b.Index

                    [
                        for hdu in acc do
                            match sample |> List.tryFind (matches hdu) with
                            | Some other -> {
                                hdu with
                                    Keywords = mergeKeywords hdu.Keywords other.Keywords
                              }
                            | None -> hdu
                        for other in sample do
                            if not (acc |> List.exists (matches other)) then
                                {
                                    other with
                                        Keywords = other.Keywords |> List.map (fun k -> { k with Optional = true })
                                }
                    ]
                )
                first

    let private overridePattern =
        Regex(
            @"^\s*(?:([A-Za-z0-9_]+)\.)?([^:]+?)\s*:\s*(string|float|int|bool|complex|value)(\s+option)?\s*$",
            RegexOptions.CultureInvariant
        )

    /// Applies overrides of the form "HDU.KEYWORD: type [option]" separated by commas or
    /// semicolons. The HDU defaults to Primary. Types are string, float, int, bool, complex, value.
    let applyOverrides (overrides: string) (schema: HduSchema list) : HduSchema list =
        let entries =
            overrides.Split([| ','; ';' |], StringSplitOptions.RemoveEmptyEntries)
            |> Array.map (fun entry ->
                let m = overridePattern.Match entry

                if not m.Success then
                    failwith $"schema entry '{entry.Trim()}' is not of the form [HDU.]KEYWORD: type [option]"

                let hdu = if m.Groups[1].Success then m.Groups[1].Value else "Primary"

                let kind =
                    match m.Groups[3].Value with
                    | "string" -> ValueKind.String
                    | "float" -> ValueKind.Real
                    | "int" -> ValueKind.Integer
                    | "bool" -> ValueKind.Logical
                    | "complex" -> ValueKind.Complex
                    | _ -> ValueKind.Any

                hdu,
                {
                    Keyword = m.Groups[2].Value.Trim().ToUpperInvariant()
                    Kind = kind
                    Optional = m.Groups[4].Success
                    Comment = None
                }
            )

        schema
        |> List.map (fun hdu ->
            let mine =
                entries
                |> Array.filter (fun (name, _) ->
                    String.Equals(name, hdu.PropertyName, StringComparison.OrdinalIgnoreCase)
                )
                |> Array.map snd
                |> List.ofArray

            let replaced =
                hdu.Keywords
                |> List.map (fun k ->
                    mine
                    |> List.tryFind (fun o -> o.Keyword = k.Keyword)
                    |> Option.map (fun o -> { o with Comment = k.Comment })
                    |> Option.defaultValue k
                )

            let added =
                mine
                |> List.filter (fun o -> not (hdu.Keywords |> List.exists (fun k -> k.Keyword = o.Keyword)))

            { hdu with Keywords = replaced @ added }
        )
