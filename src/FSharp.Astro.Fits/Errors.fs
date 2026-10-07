namespace FSharp.Astro.Fits

open System

/// How much of the FITS standard a reader or writer enforces.
type Strictness =
    /// Any violation of the standard is an error.
    | Strict
    /// Recoverable violations are reported as warnings and parsing continues with a best effort.
    | Lenient

/// A specific way in which a file departs from the FITS standard.
[<RequireQualifiedAccess>]
type Violation =
    /// A byte outside the restricted ASCII range 0x20..0x7E at the given 1-based column of a card.
    | NonAsciiCharacter of column: int
    /// A keyword that is not 1 to 8 characters drawn from A-Z, 0-9, hyphen and underscore.
    | InvalidKeyword of keyword: string
    /// A string value with no closing quote.
    | UnterminatedString
    /// A value that could not be interpreted as any FITS value type.
    | InvalidValue of text: string
    /// A number that parses but uses syntax the standard does not allow, such as a lowercase exponent.
    | NonStandardNumber of text: string
    /// An END card with non-space bytes after the keyword.
    | MalformedEnd
    /// A CONTINUE card without a string value.
    | MalformedContinue
    /// A CONTINUE card that does not follow a string value ending in an ampersand.
    | DanglingContinue
    /// A real value that is NaN or infinite, which FITS cannot represent.
    | NonFiniteReal
    /// A value that does not fit in the 70 bytes available on a card.
    | ValueTooLong of keyword: string
    /// A comment that does not fit on the card after the value.
    | CommentTooLong of keyword: string
    /// A commentary card whose keyword is not COMMENT, HISTORY or blank and whose text would
    /// begin with the value indicator '= ', so that the card would read back as a value card.
    | AmbiguousCommentary of keyword: string
    /// A raw card image that is not a positive multiple of 80 restricted-ASCII characters.
    | InvalidRawImage
    /// No END card found in the header.
    | MissingEnd
    /// Non-space bytes between the END card and the end of the header block.
    | GarbageAfterEnd
    /// A mandatory keyword is absent.
    | MissingKeyword of keyword: string
    /// A mandatory keyword is present but not at the position the standard requires.
    | KeywordOutOfOrder of keyword: string * expected: int
    /// A mandatory keyword has a value of the wrong type or outside the allowed range.
    | InvalidKeywordValue of keyword: string * reason: string
    /// The input does not begin like a FITS file.
    | NotAFitsFile of reason: string
    /// The input ends inside a header.
    | TruncatedHeader
    /// The input ends before the data unit declared by the header.
    | TruncatedData of expected: int64 * actual: int64
    /// The data unit is complete but the padding to the next block boundary is missing.
    | UnpaddedData of missing: int64
    /// Bytes after the last HDU that are too short to form a block.
    | TrailingBytes of count: int64
    /// On write, the data supplied does not match the length implied by the header.
    | DataLengthMismatch of expected: int64 * actual: int64
    /// A TFORMn value that is not a valid binary table form.
    | InvalidTForm of column: int * text: string
    /// The column widths of a binary table do not add up to NAXIS1.
    | RowLengthMismatch of declared: int64 * columns: int64
    /// A variable-length array descriptor points outside the heap.
    | InvalidDescriptor of column: int * row: int
    /// A keyword or column holds a value of a different type than a decoder asked for.
    | TypeMismatch of name: string * expected: string * actual: string
    /// A table has no column with the requested name.
    | MissingColumn of name: string

/// Functions over violations.
[<RequireQualifiedAccess>]
module Violation =
    /// True when the violation makes it impossible to continue, regardless of strictness.
    let isFatal (violation: Violation) : bool =
        match violation with
        | Violation.MissingEnd
        | Violation.NotAFitsFile _
        | Violation.TruncatedHeader
        | Violation.TruncatedData _
        | Violation.DataLengthMismatch _
        | Violation.InvalidTForm _
        | Violation.InvalidDescriptor _ -> true
        | Violation.RowLengthMismatch(declared, columns) -> columns > declared
        | Violation.MissingKeyword keyword ->
            keyword = "SIMPLE"
            || keyword = "XTENSION"
            || keyword = "BITPIX"
            || keyword.StartsWith "NAXIS"
        | Violation.InvalidKeywordValue(keyword, _) -> keyword = "BITPIX" || keyword.StartsWith "NAXIS"
        | _ -> false

    /// A human readable description.
    let describe (violation: Violation) : string =
        match violation with
        | Violation.NonAsciiCharacter column -> $"non-ASCII character at column {column}"
        | Violation.InvalidKeyword keyword -> $"invalid keyword '{keyword}'"
        | Violation.UnterminatedString -> "unterminated string value"
        | Violation.InvalidValue text -> $"invalid value '{text}'"
        | Violation.NonStandardNumber text -> $"non-standard number syntax '{text}'"
        | Violation.MalformedEnd -> "END card has non-space bytes after the keyword"
        | Violation.MalformedContinue -> "CONTINUE card has no string value"
        | Violation.DanglingContinue -> "CONTINUE card does not follow a string ending in '&'"
        | Violation.NonFiniteReal -> "real value is NaN or infinite"
        | Violation.ValueTooLong keyword -> $"value of '{keyword}' does not fit on a card"
        | Violation.CommentTooLong keyword -> $"comment of '{keyword}' does not fit on the card"
        | Violation.AmbiguousCommentary keyword ->
            $"commentary card '{keyword}' begins with '= ', which would read back as a value"
        | Violation.InvalidRawImage -> "raw card image is not a positive multiple of 80 ASCII characters"
        | Violation.MissingEnd -> "no END card"
        | Violation.GarbageAfterEnd -> "non-space bytes after the END card"
        | Violation.MissingKeyword keyword -> $"mandatory keyword '{keyword}' is missing"
        | Violation.KeywordOutOfOrder(keyword, expected) -> $"mandatory keyword '{keyword}' is not at card {expected}"
        | Violation.InvalidKeywordValue(keyword, reason) -> $"invalid value for '{keyword}': {reason}"
        | Violation.NotAFitsFile reason -> $"not a FITS file: {reason}"
        | Violation.TruncatedHeader -> "input ends inside a header"
        | Violation.TruncatedData(expected, actual) -> $"data unit truncated: expected {expected} bytes, found {actual}"
        | Violation.UnpaddedData missing -> $"data unit is missing {missing} bytes of padding"
        | Violation.TrailingBytes count -> $"{count} trailing bytes after the last HDU"
        | Violation.DataLengthMismatch(expected, actual) ->
            $"header implies {expected} data bytes but {actual} were supplied"
        | Violation.InvalidTForm(column, text) -> $"TFORM{column} '{text}' is not a valid binary table form"
        | Violation.RowLengthMismatch(declared, columns) ->
            $"NAXIS1 is {declared} but the columns occupy {columns} bytes"
        | Violation.InvalidDescriptor(column, row) ->
            $"variable-length array descriptor of column {column} row {row} points outside the heap"
        | Violation.TypeMismatch(name, expected, actual) -> $"'{name}' is {actual}, expected {expected}"
        | Violation.MissingColumn name -> $"no column named '{name}'"

/// A violation together with where it was found.
type Issue = {
    /// Zero-based index of the HDU, when known.
    Hdu: int option
    /// Zero-based index of the card within the header, when the issue is about a card.
    Card: int option
    /// Byte offset in the file, when known.
    Offset: int64 option
    /// The 80-character card image, when the issue is about a card.
    Image: string option
    /// What went wrong.
    Violation: Violation
}

/// Functions over issues.
[<RequireQualifiedAccess>]
module Issue =
    /// Builds an issue with no location information.
    let create (violation: Violation) : Issue = {
        Hdu = None
        Card = None
        Offset = None
        Image = None
        Violation = violation
    }

    /// Builds an issue located at a card.
    let atCard (hdu: int option) (card: int) (offset: int64 option) (image: string) (violation: Violation) : Issue = {
        Hdu = hdu
        Card = Some card
        Offset = offset
        Image = Some image
        Violation = violation
    }

    /// True when the issue makes it impossible to continue.
    let isFatal (issue: Issue) : bool = Violation.isFatal issue.Violation

    /// A human readable description including the location.
    let describe (issue: Issue) : string =
        let location =
            [
                issue.Hdu |> Option.map (fun h -> $"HDU {h}")
                issue.Card |> Option.map (fun c -> $"card {c}")
                issue.Offset |> Option.map (fun o -> $"offset {o}")
            ]
            |> List.choose id

        let prefix =
            if List.isEmpty location then
                ""
            else
                String.Join(", ", location) + ": "

        let image =
            match issue.Image with
            | Some image -> $" in \"{image.TrimEnd()}\""
            | None -> ""

        prefix + Violation.describe issue.Violation + image

/// Why a FITS operation failed.
type FitsError =
    /// The input or output violates the standard. Fatal violations always appear here; under strict
    /// handling every violation does.
    | Violations of Issue list
    /// The underlying I/O failed.
    | IoError of exn

/// Functions over errors.
[<RequireQualifiedAccess>]
module FitsError =
    /// A human readable description, one line per issue.
    let describe (error: FitsError) : string =
        match error with
        | Violations issues -> issues |> List.map Issue.describe |> String.concat Environment.NewLine
        | IoError exn -> $"I/O error: {exn.Message}"

/// Thrown by the non-Result variants of the API.
type FitsException(error: FitsError) =
    inherit Exception(FitsError.describe error)

    /// The underlying error.
    member _.Error: FitsError = error
