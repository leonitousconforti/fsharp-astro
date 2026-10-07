namespace FSharp.Astro.Fits

open System
open System.Text

/// A card together with the image it was read from, so unchanged cards are written back byte for byte.
[<Struct>]
type internal HeaderEntry = { Card: Card; Raw: string option }

/// An ordered, immutable collection of header cards. END is implicit and never stored.
[<Sealed>]
type Header internal (entries: HeaderEntry[]) =
    let cards = lazy (entries |> Array.map (fun e -> e.Card) |> List.ofArray)

    member internal _.Entries: HeaderEntry[] = entries

    /// Number of logical cards.
    member _.Count: int = entries.Length

    /// The cards in order.
    member _.Cards: Card list = cards.Value

    static member internal FindIndex(entries: HeaderEntry[], keyword: string) : int option =
        entries
        |> Array.tryFindIndex (fun e ->
            match e.Card with
            | Card.Value(k, _, _) -> String.Equals(k, keyword, StringComparison.OrdinalIgnoreCase)
            | _ -> false
        )

    /// The value of the first value card with this keyword. Throws when absent.
    member this.Item
        with get (keyword: string): CardValue =
            match Header.FindIndex(entries, keyword) with
            | Some i ->
                match entries[i].Card with
                | Card.Value(_, value, _) -> value
                | _ -> failwith "unreachable"
            | None -> raise (Collections.Generic.KeyNotFoundException $"keyword '{keyword}' not found")

    override this.Equals(other: obj) =
        match other with
        | :? Header as h -> this.Cards = h.Cards
        | _ -> false

    override this.GetHashCode() = hash this.Cards

    override this.ToString() = $"Header ({entries.Length} cards)"

    interface IEquatable<Header> with
        member this.Equals(other: Header) = this.Cards = other.Cards

/// The result of parsing a header from bytes.
type ParsedHeader = {
    /// The parsed cards.
    Header: Header
    /// Number of bytes consumed, always a multiple of the block size.
    Length: int
    /// Recoverable violations found while parsing. Always empty under strict handling.
    Warnings: Issue list
}

/// Construction, lookup, editing, parsing and serialization of headers.
[<RequireQualifiedAccess>]
module Header =
    let private entry (card: Card) : HeaderEntry = { Card = card; Raw = None }

    /// A header with no cards.
    let empty: Header = Header [||]

    /// Builds a header from cards. END cards are dropped.
    let ofCards (cards: Card seq) : Header =
        cards
        |> Seq.filter (fun c -> c <> Card.End)
        |> Seq.map entry
        |> Array.ofSeq
        |> Header

    /// The cards in order.
    let cards (header: Header) : Card list = header.Cards

    /// Number of logical cards.
    let count (header: Header) : int = header.Count

    /// Keywords of all value cards, in order, including duplicates.
    let keywords (header: Header) : string list =
        header.Cards
        |> List.choose (fun c ->
            match c with
            | Card.Value(k, _, _) -> Some k
            | _ -> None
        )

    /// True when a value card with this keyword exists. Matching ignores case.
    let contains (keyword: string) (header: Header) : bool =
        (Header.FindIndex(header.Entries, keyword)).IsSome

    /// The first value card with this keyword.
    let tryFind (keyword: string) (header: Header) : Card option =
        Header.FindIndex(header.Entries, keyword)
        |> Option.map (fun i -> header.Entries[i].Card)

    /// The value of the first value card with this keyword.
    let tryValue (keyword: string) (header: Header) : CardValue option =
        match tryFind keyword header with
        | Some(Card.Value(_, value, _)) -> Some value
        | _ -> None

    /// The comment of the first value card with this keyword.
    let tryComment (keyword: string) (header: Header) : string option =
        match tryFind keyword header with
        | Some(Card.Value(_, _, comment)) -> comment
        | _ -> None

    /// The integer value of a keyword, when present and an integer.
    let tryInt (keyword: string) (header: Header) : int64 option =
        match tryValue keyword header with
        | Some(CardValue.Integer i) -> Some i
        | _ -> None

    /// The real value of a keyword. Integers are widened.
    let tryFloat (keyword: string) (header: Header) : float option =
        match tryValue keyword header with
        | Some(CardValue.Real r) -> Some r
        | Some(CardValue.Integer i) -> Some(float i)
        | _ -> None

    /// The string value of a keyword, when present and a string.
    let tryString (keyword: string) (header: Header) : string option =
        match tryValue keyword header with
        | Some(CardValue.String s) -> Some s
        | _ -> None

    /// The logical value of a keyword, when present and a logical.
    let tryBool (keyword: string) (header: Header) : bool option =
        match tryValue keyword header with
        | Some(CardValue.Logical b) -> Some b
        | _ -> None

    let private required (keyword: string) (value: 'T option) : 'T =
        match value with
        | Some v -> v
        | None ->
            raise (Collections.Generic.KeyNotFoundException $"keyword '{keyword}' is missing or has the wrong type")

    /// The integer value of a keyword. Throws when missing or not an integer.
    let getInt (keyword: string) (header: Header) : int64 =
        tryInt keyword header |> required keyword

    /// The real value of a keyword. Throws when missing or not numeric.
    let getFloat (keyword: string) (header: Header) : float =
        tryFloat keyword header |> required keyword

    /// The string value of a keyword. Throws when missing or not a string.
    let getString (keyword: string) (header: Header) : string =
        tryString keyword header |> required keyword

    /// The logical value of a keyword. Throws when missing or not a logical.
    let getBool (keyword: string) (header: Header) : bool =
        tryBool keyword header |> required keyword

    /// Text of all COMMENT cards, in order.
    let comments (header: Header) : string list =
        header.Cards
        |> List.choose (fun c ->
            match c with
            | Card.Commentary("COMMENT", text) -> Some text
            | _ -> None
        )

    /// Text of all HISTORY cards, in order.
    let history (header: Header) : string list =
        header.Cards
        |> List.choose (fun c ->
            match c with
            | Card.Commentary("HISTORY", text) -> Some text
            | _ -> None
        )

    /// Appends a card. END cards are ignored.
    let add (card: Card) (header: Header) : Header =
        if card = Card.End then
            header
        else
            Header(Array.append header.Entries [| entry card |])

    /// Appends several cards.
    let addAll (cards: Card seq) (header: Header) : Header =
        Seq.fold (fun h c -> add c h) header cards

    /// Inserts a card at the given position.
    let insertAt (index: int) (card: Card) (header: Header) : Header =
        if card = Card.End then
            header
        else
            Header(Array.insertAt index (entry card) header.Entries)

    /// Replaces the value of the first card with this keyword, keeping its comment, or appends a
    /// new card when none exists.
    let set (keyword: string) (value: CardValue) (header: Header) : Header =
        match Header.FindIndex(header.Entries, keyword) with
        | Some i ->
            let comment =
                match header.Entries[i].Card with
                | Card.Value(_, _, comment) -> comment
                | _ -> None

            let entries = Array.copy header.Entries
            entries[i] <- entry (Card.Value(keyword, value, comment))
            Header entries
        | None -> add (Card.Value(keyword, value, None)) header

    /// Replaces the value and comment of the first card with this keyword, or appends a new card.
    let setWithComment (keyword: string) (value: CardValue) (comment: string) (header: Header) : Header =
        let card = Card.Value(keyword, value, Some comment)

        match Header.FindIndex(header.Entries, keyword) with
        | Some i ->
            let entries = Array.copy header.Entries
            entries[i] <- entry card
            Header entries
        | None -> add card header

    /// Removes every value card with this keyword.
    let remove (keyword: string) (header: Header) : Header =
        header.Entries
        |> Array.filter (fun e ->
            match e.Card with
            | Card.Value(k, _, _) -> not (String.Equals(k, keyword, StringComparison.OrdinalIgnoreCase))
            | _ -> true
        )
        |> Header

    /// Removes every value card whose keyword is in the given set.
    let removeAll (keywords: string seq) (header: Header) : Header =
        let set =
            Collections.Generic.HashSet<string>(keywords, StringComparer.OrdinalIgnoreCase)

        header.Entries
        |> Array.filter (fun e ->
            match e.Card with
            | Card.Value(k, _, _) -> not (set.Contains k)
            | _ -> true
        )
        |> Header

    /// Keeps only the cards for which the predicate holds.
    let filter (predicate: Card -> bool) (header: Header) : Header =
        header.Entries |> Array.filter (fun e -> predicate e.Card) |> Header

    /// Appends a COMMENT card.
    let addComment (text: string) (header: Header) : Header =
        add (Card.Commentary("COMMENT", text)) header

    /// Appends a HISTORY card.
    let addHistory (text: string) (header: Header) : Header =
        add (Card.Commentary("HISTORY", text)) header

    /// Prepends cards, so they come before everything already in the header.
    let prepend (cards: Card seq) (header: Header) : Header =
        let front =
            cards |> Seq.filter (fun c -> c <> Card.End) |> Seq.map entry |> Array.ofSeq
        Header(Array.append front header.Entries)

    let private endKeyword = "END     "B

    /// Index of the END card within a buffer of card images, scanning complete cards only.
    let findEnd (bytes: ReadOnlySpan<byte>) : int option =
        let total = bytes.Length / Block.CardSize
        let mutable found = -1
        let mutable i = 0

        while found < 0 && i < total do
            if bytes.Slice(i * Block.CardSize, 8).SequenceEqual(ReadOnlySpan endKeyword) then
                found <- i

            i <- i + 1

        if found < 0 then None else Some found

    let private joinComment (a: string option) (b: string option) : string option =
        match a, b with
        | None, None -> None
        | Some a, None -> Some a
        | None, Some b -> Some b
        | Some a, Some b -> Some(a + b)

    /// One logical card starting at image `i`: the entry it yields, which is none for END, the
    /// issues found, and the index of the next image. A string value ending in '&' absorbs the
    /// CONTINUE cards that follow it.
    let private logicalCard
        (images: string[])
        (locate: int -> Violation -> Issue)
        (endIndex: int)
        (i: int)
        : HeaderEntry option * Issue seq * int =
        let card, violations = Card.parse images[i]
        let issues = violations |> Seq.map (locate i)

        match card with
        | Card.End -> None, issues, i + 1
        | Card.Value(keyword, CardValue.String s, comment) when s.EndsWith '&' ->
            let rec gather (j: int) (value: string) (comment: string option) (raw: string) (issues: Issue seq) =
                if j < endIndex && value.EndsWith '&' then
                    match Card.parse images[j] with
                    | Card.Continue(text, more), violations ->
                        gather
                            (j + 1)
                            (value.Substring(0, value.Length - 1) + text)
                            (joinComment comment more)
                            (raw + images[j])
                            (Seq.append issues (violations |> Seq.map (locate j)))
                    | _ -> j, value, comment, raw, issues
                else
                    j, value, comment, raw, issues

            let next, value, comment, raw, issues = gather (i + 1) s comment images[i] issues

            Some {
                Card = Card.Value(keyword, CardValue.String value, comment)
                Raw = Some raw
            },
            issues,
            next
        | Card.Continue _ ->
            Some { Card = card; Raw = Some images[i] },
            Seq.append issues (Seq.singleton (locate i Violation.DanglingContinue)),
            i + 1
        | _ -> Some { Card = card; Raw = Some images[i] }, issues, i + 1

    /// Parses a header from a buffer that holds complete blocks, starting at the first card and
    /// containing the END card. Issues are located relative to the HDU index and base offset given.
    /// Under strict handling any violation is an error. Under lenient handling only fatal violations
    /// are errors and the rest are returned as warnings.
    let parse
        (strictness: Strictness)
        (hduIndex: int)
        (baseOffset: int64)
        (bytes: ReadOnlySpan<byte>)
        : Result<ParsedHeader, Issue list> =
        let cardSize = Block.CardSize
        let total = bytes.Length / cardSize
        let images = Array.zeroCreate<string> total

        for i in 0 .. total - 1 do
            images[i] <- Encoding.Latin1.GetString(bytes.Slice(i * cardSize, cardSize))

        let locate (card: int) (violation: Violation) =
            Issue.atCard (Some hduIndex) card (Some(baseOffset + int64 (card * cardSize))) images[card] violation

        match findEnd bytes with
        | None ->
            Error [
                {
                    Issue.create Violation.MissingEnd with
                        Hdu = Some hduIndex
                        Offset = Some baseOffset
                }
            ]
        | Some endIndex ->
            let length = int (Block.padded (int64 ((endIndex + 1) * cardSize)))

            let garbage = seq {
                let afterEnd = seq { endIndex + 1 .. length / cardSize - 1 }

                match
                    afterEnd
                    |> Seq.tryFind (fun i -> images[i] |> String.exists (fun c -> c <> ' '))
                with
                | Some i -> yield locate i Violation.GarbageAfterEnd
                | None -> ()
            }

            let steps =
                List.unfold
                    (fun i ->
                        if i > endIndex then
                            None
                        else
                            let entry, issues, next = logicalCard images locate endIndex i
                            Some((entry, issues), next)
                    )
                    0

            let entries = steps |> List.choose fst |> Array.ofList
            let issues = Seq.append garbage (steps |> Seq.collect snd) |> List.ofSeq
            let fatal = issues |> List.exists Issue.isFatal

            if fatal || (strictness = Strict && not issues.IsEmpty) then
                Error issues
            else
                Ok {
                    Header = Header entries
                    Length = length
                    Warnings = issues
                }

    /// Serializes a header to complete blocks: every card, the END card, and space padding. Cards
    /// that were read from a file and never modified are written back byte for byte.
    let tryToBytes (header: Header) : Result<byte[], Issue list> =
        let formatted =
            header.Entries
            |> Array.mapi (fun i e ->
                match e.Raw with
                | Some raw -> Ok raw
                | None ->
                    Card.tryFormat e.Card
                    |> Result.mapError (fun v -> { Issue.create v with Card = Some i })
            )

        let issues =
            formatted
            |> Array.choose (fun r ->
                match r with
                | Error issue -> Some issue
                | Ok _ -> None
            )
            |> List.ofArray

        if not issues.IsEmpty then
            Error issues
        else
            let cards =
                formatted
                |> Array.map (fun r ->
                    match r with
                    | Ok image -> image
                    | Error _ -> ""
                )
                |> String.concat ""

            let withEnd = cards + "END".PadRight Block.CardSize
            let padded = withEnd.PadRight(int (Block.padded (int64 withEnd.Length)))
            Ok(Encoding.Latin1.GetBytes padded)

    /// Serializes a header, throwing FitsException when a card cannot be represented.
    let toBytes (header: Header) : byte[] =
        match tryToBytes header with
        | Ok bytes -> bytes
        | Error issues -> raise (FitsException(Violations issues))

    /// The header as text, one 80-character image per line, without END or padding.
    let toText (header: Header) : string =
        header.Entries
        |> Array.map (fun e ->
            let image =
                match e.Raw with
                | Some raw -> raw
                | None ->
                    match Card.tryFormat e.Card with
                    | Ok image -> image
                    | Error _ -> $"%A{e.Card}"

            [
                for start in 0 .. Block.CardSize .. image.Length - 1 ->
                    image.Substring(start, min Block.CardSize (image.Length - start))
            ]
            |> String.concat Environment.NewLine
        )
        |> String.concat Environment.NewLine
