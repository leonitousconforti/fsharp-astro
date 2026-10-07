namespace FSharp.Astro.Fits

open System

/// The data type of array elements, as declared by BITPIX.
[<RequireQualifiedAccess>]
type BitPix =
    | UInt8
    | Int16
    | Int32
    | Int64
    | Float32
    | Float64

    /// The BITPIX keyword value.
    member this.Code: int =
        match this with
        | UInt8 -> 8
        | Int16 -> 16
        | Int32 -> 32
        | Int64 -> 64
        | Float32 -> -32
        | Float64 -> -64

    /// Size of one element in bytes.
    member this.Size: int = abs this.Code / 8

    /// The BitPix for a BITPIX keyword value.
    static member TryOfCode(code: int64) : BitPix option =
        match code with
        | 8L -> Some UInt8
        | 16L -> Some Int16
        | 32L -> Some Int32
        | 64L -> Some Int64
        | -32L -> Some Float32
        | -64L -> Some Float64
        | _ -> None

/// What an HDU holds.
[<RequireQualifiedAccess>]
type HduKind =
    /// The first HDU, holding an image or nothing.
    | Primary
    /// The first HDU in the legacy random groups layout.
    | RandomGroups
    /// An IMAGE extension.
    | Image
    /// A BINTABLE extension.
    | BinaryTable
    /// A TABLE (ASCII) extension.
    | AsciiTable
    /// A conforming extension of another type, by XTENSION name.
    | Extension of name: string

/// Sizes and shapes declared by the mandatory keywords of an HDU.
type HduLayout = {
    Kind: HduKind
    BitPix: BitPix
    /// NAXIS1..NAXISn in FITS order, so the first entry varies fastest on disk.
    Axes: int64[]
    PCount: int64
    GCount: int64
}

/// Where an HDU sits in a file and what it declares.
type HduInfo = {
    /// Zero-based index in the file.
    Index: int
    Kind: HduKind
    Header: Header
    BitPix: BitPix
    /// NAXIS1..NAXISn in FITS order, so the first entry varies fastest on disk.
    Axes: int64[]
    PCount: int64
    GCount: int64
    /// Byte offset of the first header card.
    HeaderOffset: int64
    /// Header size in bytes, a multiple of the block size.
    HeaderLength: int64
    /// Byte offset of the first data byte.
    DataOffset: int64
    /// Size of the data unit in bytes, before padding.
    DataLength: int64
} with

    /// Byte offset just past the padded data unit, where the next HDU begins.
    member this.DataEnd: int64 = this.DataOffset + Block.padded this.DataLength

    /// The EXTNAME value, when present.
    member this.Name: string option = Header.tryString "EXTNAME" this.Header

    /// The EXTVER value, when present.
    member this.Version: int64 option = Header.tryInt "EXTVER" this.Header

    member this.Layout: HduLayout = {
        Kind = this.Kind
        BitPix = this.BitPix
        Axes = this.Axes
        PCount = this.PCount
        GCount = this.GCount
    }

/// Validation of mandatory keywords and computation of data layout.
[<RequireQualifiedAccess>]
module HduLayout =
    /// Size in bytes of the data unit implied by a layout, before padding.
    let dataLength (layout: HduLayout) : int64 =
        if layout.Axes.Length = 0 then
            0L
        else
            let axes =
                match layout.Kind with
                | HduKind.RandomGroups -> layout.Axes[1..]
                | _ -> layout.Axes

            let pixels = axes |> Array.fold Checked.(*) 1L
            Checked.(*) (int64 layout.BitPix.Size) (Checked.(*) layout.GCount (Checked.(+) layout.PCount pixels))

    /// Keywords whose placement and values the standard dictates. These are removed from user
    /// headers and regenerated when writing.
    let mandatoryKeywords (axes: int) : string list = [
        "SIMPLE"
        "XTENSION"
        "BITPIX"
        "NAXIS"
        for n in 1..axes do
            $"NAXIS{n}"
        "PCOUNT"
        "GCOUNT"
        "EXTEND"
    ]

    /// Reads the mandatory keywords of a header, checking that they are present, in order, and
    /// valid. Under strict handling any violation is an error. Under lenient handling only
    /// violations that make the layout unknowable are errors, and the rest come back as warnings.
    let tryOfHeader
        (strictness: Strictness)
        (hduIndex: int)
        (header: Header)
        : Result<HduLayout * Issue list, Issue list> =
        let cards = header.Cards |> Array.ofList

        let locate (card: int option) (violation: Violation) = {
            Issue.create violation with
                Hdu = Some hduIndex
                Card = card
        }

        /// The value of a mandatory keyword expected at a position, with an issue when it sits
        /// elsewhere or is absent.
        let expectAt (position: int) (keyword: string) : CardValue option * Issue seq =
            let atPosition =
                if position < cards.Length then
                    match cards[position] with
                    | Card.Value(k, v, _) when k = keyword -> Some v
                    | _ -> None
                else
                    None

            match atPosition with
            | Some v -> Some v, Seq.empty
            | None ->
                match Header.tryValue keyword header with
                | Some v -> Some v, Seq.singleton (locate None (Violation.KeywordOutOfOrder(keyword, position)))
                | None -> None, Seq.singleton (locate None (Violation.MissingKeyword keyword))

        let expectInt (position: int) (keyword: string) : int64 option * Issue seq =
            match expectAt position keyword with
            | Some(CardValue.Integer i), issues -> Some i, issues
            | Some _, issues ->
                None,
                Seq.append
                    issues
                    (Seq.singleton (
                        locate (Some position) (Violation.InvalidKeywordValue(keyword, "expected an integer"))
                    ))
            | None, issues -> None, issues

        let kind, kindIssues =
            if hduIndex = 0 then
                let simple, issues = expectAt 0 "SIMPLE"

                let kind =
                    match Header.tryBool "GROUPS" header with
                    | Some true -> HduKind.RandomGroups
                    | _ -> HduKind.Primary

                kind,
                seq {
                    yield! issues

                    match simple with
                    | Some(CardValue.Logical true)
                    | None -> ()
                    | Some _ -> yield locate (Some 0) (Violation.InvalidKeywordValue("SIMPLE", "expected T"))
                }
            else
                match expectAt 0 "XTENSION" with
                | Some(CardValue.String "IMAGE"), issues -> HduKind.Image, issues
                | Some(CardValue.String "BINTABLE"), issues -> HduKind.BinaryTable, issues
                | Some(CardValue.String "TABLE"), issues -> HduKind.AsciiTable, issues
                | Some(CardValue.String name), issues -> HduKind.Extension name, issues
                | Some _, issues ->
                    HduKind.Extension "",
                    Seq.append
                        issues
                        (Seq.singleton (
                            locate (Some 0) (Violation.InvalidKeywordValue("XTENSION", "expected a string"))
                        ))
                | None, issues -> HduKind.Extension "", issues

        let bitPix, bitPixIssues =
            match expectInt 1 "BITPIX" with
            | Some code, issues ->
                match BitPix.TryOfCode code with
                | Some b -> Some b, issues
                | None ->
                    None,
                    Seq.append
                        issues
                        (Seq.singleton (
                            locate (Some 1) (Violation.InvalidKeywordValue("BITPIX", $"{code} is not a valid BITPIX"))
                        ))
            | None, issues -> None, issues

        let naxis, naxisIssues =
            match expectInt 2 "NAXIS" with
            | Some n, issues when n >= 0L && n <= 999L -> Some(int n), issues
            | Some n, issues ->
                None,
                Seq.append
                    issues
                    (Seq.singleton (locate (Some 2) (Violation.InvalidKeywordValue("NAXIS", $"{n} is outside 0..999"))))
            | None, issues -> None, issues

        let axes, axesIssues =
            match naxis with
            | Some n ->
                let results = [
                    for i in 1..n do
                        match expectInt (2 + i) $"NAXIS{i}" with
                        | Some v, issues when v >= 0L -> Some v, issues
                        | Some v, issues ->
                            None,
                            Seq.append
                                issues
                                (Seq.singleton (
                                    locate
                                        (Some(2 + i))
                                        (Violation.InvalidKeywordValue($"NAXIS{i}", $"{v} is negative"))
                                ))
                        | None, issues -> None, issues
                ]

                let values = results |> List.map fst

                let axes =
                    if values |> List.forall Option.isSome then
                        Some(values |> List.map Option.get |> Array.ofList)
                    else
                        None

                axes, results |> Seq.collect snd
            | None -> None, Seq.empty

        let afterAxes = 3 + defaultArg naxis 0

        let pcount, gcount, countIssues =
            match kind with
            | HduKind.Primary -> 0L, 1L, Seq.empty
            | HduKind.RandomGroups ->
                let _, groupsIssues = expectAt afterAxes "GROUPS"
                let p, pIssues = expectInt (afterAxes + 1) "PCOUNT"
                let g, gIssues = expectInt (afterAxes + 2) "GCOUNT"
                defaultArg p 0L, defaultArg g 1L, Seq.concat [ groupsIssues; pIssues; gIssues ]
            | _ ->
                let p, pIssues = expectInt afterAxes "PCOUNT"
                let g, gIssues = expectInt (afterAxes + 1) "GCOUNT"
                let p = defaultArg p 0L
                let g = defaultArg g 1L

                let shapeIssues =
                    match kind with
                    | HduKind.BinaryTable
                    | HduKind.AsciiTable ->
                        let _, fieldsIssues = expectInt (afterAxes + 2) "TFIELDS"

                        seq {
                            yield! fieldsIssues

                            match bitPix with
                            | Some BitPix.UInt8
                            | None -> ()
                            | Some _ ->
                                yield
                                    locate
                                        (Some 1)
                                        (Violation.InvalidKeywordValue("BITPIX", "tables require BITPIX = 8"))

                            if naxis.IsSome && naxis <> Some 2 then
                                yield
                                    locate (Some 2) (Violation.InvalidKeywordValue("NAXIS", "tables require NAXIS = 2"))
                        }
                    | HduKind.Image -> seq {
                        if p <> 0L then
                            yield
                                locate
                                    None
                                    (Violation.InvalidKeywordValue("PCOUNT", "image extensions require PCOUNT = 0"))

                        if g <> 1L then
                            yield
                                locate
                                    None
                                    (Violation.InvalidKeywordValue("GCOUNT", "image extensions require GCOUNT = 1"))
                      }
                    | _ -> Seq.empty

                p, g, Seq.concat [ pIssues; gIssues; shapeIssues ]

        let groupsIssues = seq {
            match kind, axes with
            | HduKind.RandomGroups, Some a when a.Length = 0 || a[0] <> 0L ->
                yield locate None (Violation.InvalidKeywordValue("NAXIS1", "random groups require NAXIS1 = 0"))
            | _ -> ()
        }

        let issues =
            Seq.concat [ kindIssues; bitPixIssues; naxisIssues; axesIssues; countIssues; groupsIssues ]
            |> List.ofSeq

        let fatal = issues |> List.exists Issue.isFatal

        match bitPix, axes with
        | Some bitPix, Some axes when not fatal && (strictness = Lenient || issues.IsEmpty) ->
            let layout = {
                Kind = kind
                BitPix = bitPix
                Axes = axes
                PCount = pcount
                GCount = gcount
            }

            try
                dataLength layout |> ignore
                Ok(layout, issues)
            with :? OverflowException ->
                Error [
                    locate None (Violation.InvalidKeywordValue("NAXIS", "axis product overflows"))
                ]
        | _ -> Error issues

    /// The mandatory cards for a layout, in the order the standard requires.
    let mandatoryCards (layout: HduLayout) : Card list =
        let naxis = layout.Axes.Length

        let axes = [
            for i in 1..naxis do
                Card.Value($"NAXIS{i}", CardValue.Integer layout.Axes[i - 1], Some $"length of data axis {i}")
        ]

        let bitPix =
            Card.Value("BITPIX", CardValue.Integer(int64 layout.BitPix.Code), Some "number of bits per data pixel")
        let naxisCard =
            Card.Value("NAXIS", CardValue.Integer(int64 naxis), Some "number of data axes")

        let counts = [
            Card.Value("PCOUNT", CardValue.Integer layout.PCount, Some "number of parameters")
            Card.Value("GCOUNT", CardValue.Integer layout.GCount, Some "number of groups")
        ]

        match layout.Kind with
        | HduKind.Primary -> [
            Card.Value("SIMPLE", CardValue.Logical true, Some "conforms to FITS standard")
            bitPix
            naxisCard
            yield! axes
            Card.Value("EXTEND", CardValue.Logical true, Some "extensions may be present")
          ]
        | HduKind.RandomGroups -> [
            Card.Value("SIMPLE", CardValue.Logical true, Some "conforms to FITS standard")
            bitPix
            naxisCard
            yield! axes
            Card.Value("GROUPS", CardValue.Logical true, Some "random groups")
            yield! counts
          ]
        | HduKind.Image -> [
            Card.Value("XTENSION", CardValue.String "IMAGE", Some "image extension")
            bitPix
            naxisCard
            yield! axes
            yield! counts
          ]
        | HduKind.BinaryTable -> [
            Card.Value("XTENSION", CardValue.String "BINTABLE", Some "binary table extension")
            bitPix
            naxisCard
            yield! axes
            yield! counts
          ]
        | HduKind.AsciiTable -> [
            Card.Value("XTENSION", CardValue.String "TABLE", Some "ASCII table extension")
            bitPix
            naxisCard
            yield! axes
            yield! counts
          ]
        | HduKind.Extension name -> [
            Card.Value("XTENSION", CardValue.String name, None)
            bitPix
            naxisCard
            yield! axes
            yield! counts
          ]
