namespace FSharp.Astro.Units

open System
open System.Globalization

/// Sexagesimal angles: `hh:mm:ss.s` for right ascension and `dd:mm:ss.s` for declination.
///
/// Formatting rounds the seconds to a fixed number of decimal places and carries the overflow
/// upward, so 23:59:59.96 to one place is 00:00:00.0 and not 23:59:60.0. The carry is done in
/// integer arithmetic on the rounded seconds, which is the only way to get it right: rounding
/// the three fields independently produces a sixtieth that does not exist.
///
/// Parsing is liberal about separators. Colons, spaces and the `h m s`, `d m s` and `° ' "`
/// markers all work, a leading `+` or `-` applies to the whole angle, and one or two fields mean
/// the trailing ones are zero. It is strict about ranges: minutes and seconds must be below 60,
/// and only the last field written may have a fractional part.
[<RequireQualifiedAccess>]
module Sexagesimal =

    /// The fields of a sexagesimal angle after rounding. `Units` is hours or degrees and is never
    /// negative; the sign of the angle is in `Sign`, which is `1` or `-1`.
    [<Struct>]
    type Parts = { Sign: int; Units: int; Minutes: int; Seconds: float }

    let private ticksPerSecond (decimals: int) : int64 =
        if decimals < 0 || decimals > 9 then
            invalidArg "decimals" "decimals must be between 0 and 9"

        pown 10L decimals

    /// Splits the magnitude of a value in hours or degrees into whole units, whole minutes, whole
    /// seconds and a fraction of a second expressed in units of 10^-decimals seconds. Everything
    /// after the rounding is integer arithmetic, so the carry cannot produce a 60.
    let private split (decimals: int) (value: float) : int64 * int64 * int64 * int64 =
        let perSecond = ticksPerSecond decimals

        if not (Double.IsFinite value) then
            invalidArg "value" "value must be finite"

        let scaled = abs value * 3600.0 * float perSecond

        if scaled >= 9.2e18 then
            invalidArg "value" "value is too large to render as sexagesimal"

        let ticks = int64 (Math.Round(scaled, MidpointRounding.AwayFromZero))
        let perMinute = 60L * perSecond
        let perUnit = 60L * perMinute
        let rest = ticks % perUnit
        ticks / perUnit, rest / perMinute, rest % perMinute / perSecond, rest % perSecond

    let private render (alwaysSign: bool) (decimals: int) (value: float) : string =
        let units, minutes, seconds, fraction = split decimals value
        let sign =
            if value < 0.0 then "-"
            elif alwaysSign then "+"
            else ""

        let secondsText =
            if decimals = 0 then
                string(seconds).PadLeft(2, '0')
            else
                string(seconds).PadLeft(2, '0') + "." + string(fraction).PadLeft(decimals, '0')

        String.Concat(sign, string(units).PadLeft(2, '0'), ":", string(minutes).PadLeft(2, '0'), ":", secondsText)

    /// Rounds a value in hours or degrees to `decimals` places on the seconds and returns its
    /// fields. The same rounding and carry the formatters use.
    let parts (decimals: int) (value: float) : Parts =
        let units, minutes, seconds, fraction = split decimals value

        {
            Sign = if value < 0.0 then -1 else 1
            Units = int units
            Minutes = int minutes
            Seconds = float seconds + float fraction / float (ticksPerSecond decimals)
        }

    /// Formats an hour angle as `hh:mm:ss` with `decimals` places on the seconds. A negative angle
    /// gets a leading minus; right ascensions are conventionally wrapped into `[0h, 24h)` first
    /// with `Angle.wrap 24.0<hourangle>`.
    let formatHours (decimals: int) (x: float<hourangle>) : string = render false decimals (float x)

    /// Formats an angle as `+dd:mm:ss` with `decimals` places on the seconds. The sign is always
    /// written, the way a declination is.
    let formatDegrees (decimals: int) (x: float<deg>) : string = render true decimals (float x)

    let private separators = [|
        ':'
        ' '
        '\t'
        'h'
        'H'
        'd'
        'D'
        'm'
        'M'
        's'
        'S'
        '\''
        '"'
        '°'
        '′'
        '″'
    |]

    /// Parses the magnitude and sign of a sexagesimal string into a value in hours or degrees.
    let private tryParseValue (text: string) : float option =
        if String.IsNullOrWhiteSpace text then
            None
        else
            let trimmed = text.Trim()
            let negative = trimmed.[0] = '-'

            let body =
                if negative || trimmed.[0] = '+' then
                    trimmed.Substring 1
                else
                    trimmed

            let fields = body.Split(separators, StringSplitOptions.RemoveEmptyEntries)

            let parsed =
                fields
                |> Array.map (fun field ->
                    match Double.TryParse(field, NumberStyles.AllowDecimalPoint, CultureInfo.InvariantCulture) with
                    | true, v when Double.IsFinite v && v >= 0.0 -> Some v
                    | _ -> None
                )

            if parsed.Length = 0 || parsed.Length > 3 || Array.exists Option.isNone parsed then
                None
            else
                let v = Array.map Option.get parsed
                let whole (x: float) = x = Math.Floor x

                // Only the last field written may carry a fraction, and the sixtieths must be
                // sixtieths: "12:60:00" and "12:30.5:00" are both rejected.
                let valid =
                    (v.Length < 2 || (whole v.[0] && v.[1] < 60.0))
                    && (v.Length < 3 || (whole v.[1] && v.[2] < 60.0))

                if not valid then
                    None
                else
                    let magnitude =
                        v.[0]
                        + (if v.Length > 1 then v.[1] / 60.0 else 0.0)
                        + (if v.Length > 2 then v.[2] / 3600.0 else 0.0)

                    Some(if negative then -magnitude else magnitude)

    /// Parses `hh:mm:ss.s` into an hour angle, or `None` if the string is not a sexagesimal angle.
    let tryParseHours (text: string) : float<hourangle> option =
        tryParseValue text |> Option.map (fun v -> v * 1.0<hourangle>)

    /// Parses `dd:mm:ss.s` into degrees, or `None` if the string is not a sexagesimal angle.
    let tryParseDegrees (text: string) : float<deg> option =
        tryParseValue text |> Option.map (fun v -> v * 1.0<deg>)

    /// Parses `hh:mm:ss.s` into an hour angle. Raises `FormatException` if the string is not a
    /// sexagesimal angle.
    let parseHours (text: string) : float<hourangle> =
        match tryParseHours text with
        | Some v -> v
        | None -> raise (FormatException $"'%s{text}' is not a sexagesimal hour angle")

    /// Parses `dd:mm:ss.s` into degrees. Raises `FormatException` if the string is not a
    /// sexagesimal angle.
    let parseDegrees (text: string) : float<deg> =
        match tryParseDegrees text with
        | Some v -> v
        | None -> raise (FormatException $"'%s{text}' is not a sexagesimal angle")
