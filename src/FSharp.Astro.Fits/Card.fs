namespace FSharp.Astro.Fits

open System
open System.Globalization
open System.Text
open System.Text.RegularExpressions

/// The value carried by a value card.
[<RequireQualifiedAccess>]
type CardValue =
    | Logical of bool
    | Integer of int64
    | Real of float
    | ComplexInt of real: int64 * imaginary: int64
    | ComplexReal of real: float * imaginary: float
    | String of string
    /// The keyword has a value indicator but no value.
    | Undefined

/// One logical header card. A long string value is one card even though it
/// occupies several 80-character images on disk.
[<RequireQualifiedAccess>]
type Card =
    /// A keyword with a value. Keywords longer than 8 characters are written
    /// using the HIERARCH convention.
    | Value of keyword: string * value: CardValue * comment: string option
    /// A card with no value indicator: COMMENT, HISTORY, a blank keyword, or
    /// any other keyword followed by text.
    | Commentary of keyword: string * text: string
    /// A CONTINUE card that could not be merged into a preceding string value.
    | Continue of text: string * comment: string option
    /// The END card.
    | End
    /// A card image that could not be parsed, kept verbatim.
    | Raw of image: string

/// Parsing and formatting of card images.
[<RequireQualifiedAccess>]
module Card =
    let private isRestrictedAscii (c: char) = c >= ' ' && c <= '~'

    let private isKeywordChar (c: char) =
        (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '-' || c = '_'

    let private isAscii (s: string) = String.forall isRestrictedAscii s

    /// True when the keyword can be written in the standard 8-character form.
    let isStandardKeyword (keyword: string) : bool =
        keyword.Length >= 1
        && keyword.Length <= 8
        && String.forall isKeywordChar keyword

    /// True when the keyword can be written using the HIERARCH convention.
    let isHierarchKeyword (keyword: string) : bool =
        keyword.Length >= 1
        && keyword.Length <= 64
        && keyword.Trim() = keyword
        && String.forall (fun c -> isRestrictedAscii c && c <> '=' && c <> '\'') keyword

    /// The keyword of a card. Blank for blank commentary, "END" for the END
    /// card.
    let keyword (card: Card) : string =
        match card with
        | Card.Value(keyword, _, _) -> keyword
        | Card.Commentary(keyword, _) -> keyword
        | Card.Continue _ -> "CONTINUE"
        | Card.End -> "END"
        | Card.Raw image -> image.Substring(0, min 8 image.Length).TrimEnd ' '

    let private integerPattern = Regex(@"^[+-]?[0-9]+$", RegexOptions.CultureInvariant)

    let private realPattern =
        Regex(@"^[+-]?([0-9]+\.?[0-9]*|\.[0-9]+)([ED][+-]?[0-9]+)?$", RegexOptions.CultureInvariant)

    let private looseRealPattern =
        Regex(@"^[+-]?([0-9]+\.?[0-9]*|\.[0-9]+)([EeDd][+-]?[0-9]+)?$", RegexOptions.CultureInvariant)

    let private parseReal (token: string) : float =
        Double.Parse(
            token.Replace('D', 'E').Replace('d', 'E').Replace('e', 'E'),
            NumberStyles.Float,
            CultureInfo.InvariantCulture
        )

    /// Parses an integer or real token. Choice1Of2 is an integer, Choice2Of2 a real. No value means
    /// the text is not a number at all.
    let private parseNumber (token: string) : Choice<int64, float> option * Violation seq =
        if integerPattern.IsMatch token then
            match Int64.TryParse(token, NumberStyles.AllowLeadingSign, CultureInfo.InvariantCulture) with
            | true, value -> Some(Choice1Of2 value), Seq.empty
            | _ -> None, Seq.singleton (Violation.InvalidValue token)
        elif realPattern.IsMatch token then
            Some(Choice2Of2(parseReal token)), Seq.empty
        elif looseRealPattern.IsMatch token then
            Some(Choice2Of2(parseReal token)), Seq.singleton (Violation.NonStandardNumber token)
        else
            None, Seq.empty

    /// Index of the first non-space character at or after `from`.
    let rec private skipSpaces (text: string) (from: int) : int =
        if from < text.Length && text[from] = ' ' then
            skipSpaces text (from + 1)
        else
            from

    /// Index of the first space or slash at or after `from`.
    let rec private tokenEnd (text: string) (from: int) : int =
        if from < text.Length && text[from] <> ' ' && text[from] <> '/' then
            tokenEnd text (from + 1)
        else
            from

    /// Reads a quoted string whose opening quote is at `start`, treating a doubled quote as one.
    /// Returns the text with trailing spaces removed, the index after the closing quote, and
    /// whether a closing quote was found.
    let private parseQuoted (text: string) (start: int) : string * int * bool =
        let sb = StringBuilder()

        let rec go (j: int) =
            if j >= text.Length then
                j, false
            elif text[j] <> '\'' then
                sb.Append text[j] |> ignore
                go (j + 1)
            elif j + 1 < text.Length && text[j + 1] = '\'' then
                sb.Append '\'' |> ignore
                go (j + 2)
            else
                j + 1, true

        let next, closed = go (start + 1)
        sb.ToString().TrimEnd ' ', next, closed

    /// Parses the comment after a value: optional spaces, a slash, then text. Anything else where
    /// the slash should be is kept as the comment and reported.
    let private parseComment (text: string) (from: int) : string option * Violation seq =
        let k = skipSpaces text from

        if k >= text.Length then
            None, Seq.empty
        elif text[k] = '/' then
            let comment = text.Substring(k + 1).Trim ' '
            (if comment = "" then None else Some comment), Seq.empty
        else
            let rest = text.Substring(k).TrimEnd ' '
            Some rest, Seq.singleton (Violation.InvalidValue rest)

    /// Parses a complex value "(re, im)" whose opening parenthesis is at `start`.
    let private parseComplex (text: string) (start: int) : CardValue option * string option * Violation seq =
        let close = text.IndexOf(')', start)

        if close < 0 then
            None, None, Seq.singleton (Violation.InvalidValue(text.Substring(start).TrimEnd ' '))
        else
            let parts = text.Substring(start + 1, close - start - 1).Split ','

            let value, valueIssues =
                if parts.Length <> 2 then
                    None, Seq.empty
                else
                    let re, reIssues = parseNumber (parts[0].Trim ' ')
                    let im, imIssues = parseNumber (parts[1].Trim ' ')

                    let value =
                        match re, im with
                        | Some(Choice1Of2 re), Some(Choice1Of2 im) -> Some(CardValue.ComplexInt(re, im))
                        | Some(Choice1Of2 re), Some(Choice2Of2 im) -> Some(CardValue.ComplexReal(float re, im))
                        | Some(Choice2Of2 re), Some(Choice1Of2 im) -> Some(CardValue.ComplexReal(re, float im))
                        | Some(Choice2Of2 re), Some(Choice2Of2 im) -> Some(CardValue.ComplexReal(re, im))
                        | _ -> None

                    value, Seq.append reIssues imIssues

            let comment, commentIssues = parseComment text (close + 1)

            value,
            comment,
            seq {
                yield! valueIssues

                if value.IsNone then
                    yield Violation.InvalidValue(text.Substring(start, close - start + 1))

                yield! commentIssues
            }

    /// Parses the text after a value indicator: optional spaces, a value, and an optional comment.
    /// The value is None when it cannot be interpreted.
    let private parseValueText (text: string) : CardValue option * string option * Violation seq =
        let i = skipSpaces text 0

        if i >= text.Length then
            Some CardValue.Undefined, None, Seq.empty
        else
            match text[i] with
            | '/' ->
                let comment, issues = parseComment text i
                Some CardValue.Undefined, comment, issues
            | '\'' ->
                let value, next, closed = parseQuoted text i

                let comment, commentIssues =
                    if closed then parseComment text next else None, Seq.empty

                Some(CardValue.String value),
                comment,
                seq {
                    if not closed then
                        yield Violation.UnterminatedString

                    yield! commentIssues
                }
            | '(' -> parseComplex text i
            | _ ->
                let j = tokenEnd text i
                let token = text.Substring(i, j - i)

                let value, valueIssues =
                    match token with
                    | "T" -> Some(CardValue.Logical true), Seq.empty
                    | "F" -> Some(CardValue.Logical false), Seq.empty
                    | _ ->
                        match parseNumber token with
                        | Some(Choice1Of2 value), issues -> Some(CardValue.Integer value), issues
                        | Some(Choice2Of2 value), issues -> Some(CardValue.Real value), issues
                        | None, issues when Seq.isEmpty issues -> None, Seq.singleton (Violation.InvalidValue token)
                        | None, issues -> None, issues

                let comment, commentIssues = parseComment text j
                value, comment, Seq.append valueIssues commentIssues

    /// Parses one 80-character card image. Returns the best interpretation together with every
    /// violation of the standard found on the way. A card that cannot be interpreted at all comes
    /// back as Card.Raw.
    let parse (image: string) : Card * Violation list =
        if image.Length <> Block.CardSize then
            invalidArg (nameof image) $"a card image must be exactly {Block.CardSize} characters"

        let ascii = seq {
            match image |> Seq.tryFindIndex (fun c -> not (isRestrictedAscii c)) with
            | Some column -> yield Violation.NonAsciiCharacter(column + 1)
            | None -> ()
        }

        let keyword = image.Substring(0, 8).TrimEnd ' '
        let rest = image.Substring 8

        let keywordIssues = seq {
            if not (isStandardKeyword keyword) then
                yield Violation.InvalidKeyword keyword
        }

        let card, issues =
            if keyword = "END" then
                Card.End,
                seq {
                    if rest.Trim ' ' <> "" then
                        yield Violation.MalformedEnd
                }
            elif keyword = "" || keyword = "COMMENT" || keyword = "HISTORY" then
                Card.Commentary(keyword, rest.TrimEnd ' '), Seq.empty
            elif keyword = "CONTINUE" then
                match parseValueText rest with
                | Some(CardValue.String text), comment, issues -> Card.Continue(text, comment), issues
                | _, _, issues -> Card.Raw image, Seq.append issues (Seq.singleton Violation.MalformedContinue)
            elif rest.StartsWith "= " then
                match parseValueText (rest.Substring 2) with
                | Some value, comment, issues -> Card.Value(keyword, value, comment), Seq.append keywordIssues issues
                | None, _, issues -> Card.Raw image, Seq.append keywordIssues issues
            elif keyword = "HIERARCH" && rest.Contains '=' then
                let eq = rest.IndexOf '='
                let hierarchKeyword = rest.Substring(0, eq).Trim ' '

                if hierarchKeyword = "" then
                    Card.Raw image, Seq.singleton (Violation.InvalidKeyword "HIERARCH")
                else
                    match parseValueText (rest.Substring(eq + 1)) with
                    | Some value, comment, issues -> Card.Value(hierarchKeyword, value, comment), issues
                    | None, _, issues -> Card.Raw image, issues
            else
                Card.Commentary(keyword, rest.TrimEnd ' '), keywordIssues

        card, List.ofSeq (Seq.append ascii issues)

    /// Formats a finite real so that it reads back exactly and is unambiguously
    /// a real.
    let private formatReal (value: float) : Result<string, Violation> =
        if Double.IsFinite value then
            let s = value.ToString("R", CultureInfo.InvariantCulture)

            let s =
                if s.Contains '.' then s
                elif s.Contains 'E' then s.Replace("E", ".0E")
                else s + ".0"

            Ok s
        else
            Error Violation.NonFiniteReal

    let private formatInteger (value: int64) =
        value.ToString CultureInfo.InvariantCulture

    /// The text of a non-string value. Fixed format pads to 20 characters for
    /// the standard keyword form.
    let private valueText (value: CardValue) (fixedFormat: bool) : Result<string, Violation> =
        let pad (s: string) = if fixedFormat then s.PadLeft 20 else s

        match value with
        | CardValue.Logical b -> Ok(pad (if b then "T" else "F"))
        | CardValue.Integer i -> Ok(pad (formatInteger i))
        | CardValue.Real r -> formatReal r |> Result.map pad
        | CardValue.ComplexInt(re, im) -> Ok $"({formatInteger re}, {formatInteger im})"
        | CardValue.ComplexReal(re, im) ->
            match formatReal re, formatReal im with
            | Ok re, Ok im -> Ok $"({re}, {im})"
            | Error e, _
            | _, Error e -> Error e
        | CardValue.Undefined -> Ok ""
        | CardValue.String _ -> invalidOp "string values are laid out separately"

    let private escape (s: string) = s.Replace("'", "''")

    /// Lays out a string value after the given prefix, spilling onto CONTINUE
    /// cards when it does not fit. The prefix is "KEYWORD = ", "HIERARCH ... =
    /// " or "CONTINUE  ". Returns a string whose length is a multiple of 80.
    let private layoutString (prefix: string) (minWidth: int) (value: string) (comment: string option) : string =
        let cardSize = Block.CardSize
        let continuePrefix = "CONTINUE  "
        let single = prefix + "'" + (escape value).PadRight minWidth + "'"

        let withComment (body: string) =
            match comment with
            | None -> body
            | Some c -> body + " / " + c

        if (withComment single).Length <= cardSize then
            (withComment single).PadRight cardSize
        else
            // Split the value into pieces whose escaped length leaves room for
            // a trailing '&'.
            let pieces = ResizeArray<string>()
            let sb = StringBuilder()
            let mutable used = 0
            let mutable room = cardSize - prefix.Length - 2

            for c in value do
                let width = if c = '\'' then 2 else 1

                if used + width > room - 1 then
                    pieces.Add(sb.ToString())
                    sb.Clear() |> ignore
                    used <- 0
                    room <- cardSize - continuePrefix.Length - 2

                sb.Append c |> ignore
                used <- used + width

            pieces.Add(sb.ToString())

            let cards = ResizeArray<string>()

            for i in 0 .. pieces.Count - 1 do
                let pre = if i = 0 then prefix else continuePrefix
                let width = if i = 0 then minWidth else 0

                if i < pieces.Count - 1 then
                    cards.Add((pre + "'" + escape pieces[i] + "&'").PadRight cardSize)
                else
                    let body = pre + "'" + (escape pieces[i]).PadRight width + "'"

                    match comment with
                    | None -> cards.Add(body.PadRight cardSize)
                    | Some c when body.Length + 3 + c.Length <= cardSize ->
                        cards.Add((body + " / " + c).PadRight cardSize)
                    | Some c ->
                        // The comment needs cards of its own. Keep the value
                        // open with '&' and close it on the final comment card
                        // with an empty string.
                        cards.Add((pre + "'" + escape pieces[i] + "&'").PadRight cardSize)
                        let lastCap = cardSize - (continuePrefix + "'' / ").Length
                        let moreCap = cardSize - (continuePrefix + "'&' / ").Length
                        let mutable rest = c

                        while rest <> "" do
                            if rest.Length <= lastCap then
                                cards.Add((continuePrefix + "'' / " + rest).PadRight cardSize)
                                rest <- ""
                            else
                                // Cut where neither side is a space, since
                                // readers trim comment edges.
                                let mutable cut = moreCap

                                while cut > 1 && (rest[cut - 1] = ' ' || rest[cut] = ' ') do
                                    cut <- cut - 1

                                cards.Add((continuePrefix + "'&' / " + rest.Substring(0, cut)).PadRight cardSize)
                                rest <- rest.Substring cut

            String.Concat cards

    /// Formats a card as one or more 80-character images. Fails when the card
    /// cannot be represented in the standard: a bad keyword, a non-finite real,
    /// non-ASCII text, or a comment that does not fit.
    let tryFormat (card: Card) : Result<string, Violation> =
        let cardSize = Block.CardSize

        match card with
        | Card.End -> Ok("END".PadRight cardSize)
        | Card.Raw image ->
            if image.Length > 0 && image.Length % cardSize = 0 && isAscii image then
                Ok image
            else
                Error Violation.InvalidRawImage
        | Card.Commentary(keyword, text) ->
            if not (keyword = "" || isStandardKeyword keyword) then
                Error(Violation.InvalidKeyword keyword)
            elif not (isAscii text) then
                Error(Violation.NonAsciiCharacter(9 + (text |> Seq.findIndex (fun c -> not (isRestrictedAscii c)))))
            else
                let width = cardSize - 8
                let prefix = keyword.PadRight 8

                let chunks =
                    if text.Length <= width then
                        [ text ]
                    else
                        [
                            for start in 0..width .. text.Length - 1 ->
                                text.Substring(start, min width (text.Length - start))
                        ]

                // Only COMMENT, HISTORY and the blank keyword are read as commentary whatever
                // follows them. Under any other keyword a card whose ninth and tenth bytes are
                // '= ' is a value card, so text that would land there cannot be written at all.
                // The padding supplies the space, so a chunk of exactly "=" is ambiguous too.
                let reserved = keyword = "" || keyword = "COMMENT" || keyword = "HISTORY"

                if
                    not reserved
                    && chunks |> List.exists (fun chunk -> chunk.PadRight(width).StartsWith "= ")
                then
                    Error(Violation.AmbiguousCommentary keyword)
                else
                    Ok(
                        chunks
                        |> List.map (fun chunk -> prefix + chunk.PadRight width)
                        |> String.concat ""
                    )
        | Card.Continue(text, comment) ->
            if not (isAscii text) || not (comment |> Option.forall isAscii) then
                Error(Violation.NonAsciiCharacter 11)
            else
                Ok(layoutString "CONTINUE  " 0 text comment)
        | Card.Value(keyword, value, comment) ->
            let prefix =
                if isStandardKeyword keyword then
                    Ok(keyword.PadRight 8 + "= ")
                elif isHierarchKeyword keyword then
                    Ok("HIERARCH " + keyword + " = ")
                else
                    Error(Violation.InvalidKeyword keyword)

            let fixedFormat = isStandardKeyword keyword

            match prefix with
            | Error e -> Error e
            | Ok prefix ->
                if not (comment |> Option.forall isAscii) then
                    Error(Violation.NonAsciiCharacter 11)
                else
                    match value with
                    | CardValue.String s ->
                        if isAscii s then
                            Ok(layoutString prefix (if fixedFormat then 8 else 0) s comment)
                        else
                            Error(Violation.NonAsciiCharacter 11)
                    | _ ->
                        match valueText value fixedFormat with
                        | Error e -> Error e
                        | Ok text ->
                            let body = prefix + text

                            if body.Length > cardSize then
                                Error(Violation.ValueTooLong keyword)
                            else
                                match comment with
                                | None -> Ok(body.PadRight cardSize)
                                | Some c ->
                                    let full = body + " / " + c

                                    if full.Length > cardSize then
                                        Error(Violation.CommentTooLong keyword)
                                    else
                                        Ok(full.PadRight cardSize)

    /// Formats a card, throwing FitsException when it cannot be represented.
    let format (card: Card) : string =
        match tryFormat card with
        | Ok image -> image
        | Error violation -> raise (FitsException(Violations [ Issue.create violation ]))
