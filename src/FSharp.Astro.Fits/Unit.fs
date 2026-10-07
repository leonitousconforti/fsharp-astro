namespace FSharp.Astro.Fits

open System
open System.Globalization

// The unit strings FITS carries in TUNITn, BUNIT and the comment field, as section 4.3 of the
// standard describes them. A unit string is a product of components separated by spaces or
// asterisks, divided by a solidus, raised to powers with ** or ^, and optionally scaled by a
// leading numeric factor, as in `10**(-20) J/(s m**2 Hz)`.
//
// Nothing here knows what a symbol means. Parsing is pure syntax, so a unit this library has never
// heard of still comes back as an expression. Measure.fs gives the symbols their values.

/// A rational exponent. The standard allows a ratio in parentheses, as in `m**(3/2)`, so an
/// exponent is not always whole. Build one with the Power module, which reduces it.
type Power = private {
    Num: int
    Den: int
} with

    /// The numerator, which carries the sign.
    member this.Numerator: int = this.Num

    /// The denominator, always positive.
    member this.Denominator: int = this.Den

    /// The exponent as a number.
    member this.Value: float = float this.Num / float this.Den

    /// The exponent when it is whole, which is what an F# unit of measure can express.
    member this.Whole: int option = if this.Den = 1 then Some this.Num else None

    override this.ToString() : string =
        if this.Den = 1 then
            string this.Num
        else
            $"{this.Num}/{this.Den}"

/// Arithmetic on exponents.
[<RequireQualifiedAccess>]
module Power =
    let rec private gcd (a: int) (b: int) : int = if b = 0 then abs a else gcd b (a % b)

    /// An exponent of numerator over denominator, reduced to lowest terms with the sign on top.
    let create (numerator: int) (denominator: int) : Power =
        if denominator = 0 then
            invalidArg (nameof denominator) "an exponent cannot have a zero denominator"

        let divisor = max 1 (gcd numerator denominator)
        let sign = if denominator < 0 then -1 else 1

        {
            Num = sign * numerator / divisor
            Den = sign * denominator / divisor
        }

    /// A whole exponent.
    let ofInt (n: int) : Power = create n 1

    let zero: Power = ofInt 0
    let one: Power = ofInt 1

    /// One half, the exponent sqrt stands for.
    let half: Power = create 1 2

    /// True for an exponent of zero, which cancels the symbol it applies to.
    let isZero (p: Power) : bool = p.Numerator = 0

    /// The sum of two exponents, which is what multiplying two powers of one symbol does.
    let add (a: Power) (b: Power) : Power =
        create (a.Numerator * b.Denominator + b.Numerator * a.Denominator) (a.Denominator * b.Denominator)

    /// The negation of an exponent, which is what dividing by a power does.
    let negate (a: Power) : Power = create -a.Numerator a.Denominator

    /// The product of two exponents, which is what raising a power to a power does.
    let multiply (a: Power) (b: Power) : Power =
        create (a.Numerator * b.Numerator) (a.Denominator * b.Denominator)

/// The parse tree of a FITS unit string.
[<RequireQualifiedAccess>]
type UnitExpr =
    /// The dimensionless unit, written as an empty string or as "1".
    | Unity
    /// A unit symbol with any prefix still attached: "Jy", "km", "beam".
    | Symbol of string
    /// A numeric factor, such as the 10 of "10**(-20) J/(s m**2 Hz)".
    | Factor of float
    /// A unit raised to an exponent.
    | Raised of UnitExpr * Power
    /// Two units multiplied, written with a space, an asterisk or a period.
    | Product of UnitExpr * UnitExpr
    /// Two units divided, written with a solidus.
    | Quotient of UnitExpr * UnitExpr
    /// One of log, ln, exp or sqrt applied to a unit, which the standard's Table 9 allows.
    | Apply of name: string * argument: UnitExpr

/// A unit expanded into a numeric factor and a product of powers of symbols.
type UnitTerms = {
    /// The numeric factor, 1.0 when the unit carries none.
    Factor: float
    /// Each symbol with its exponent, merged, ordered by symbol, with zero exponents dropped.
    Terms: (string * Power) list
}

/// Parsing, formatting and expansion of FITS unit strings.
[<RequireQualifiedAccess>]
module UnitExpr =
    let private functions = [ "log"; "ln"; "exp"; "sqrt" ]

    /// Characters a unit symbol is made of. The standard's symbols are letters; the percent sign
    /// is the one exception it names.
    let private isSymbolChar (c: char) = Char.IsLetter c || c = '%'

    /// The index of the first character at or after `i` that is not a space.
    let private spaces (s: string) (i: int) : int =
        let mutable j = i

        while j < s.Length && s[j] = ' ' do
            j <- j + 1

        j

    /// A decimal number, as a leading factor is written.
    let private number (s: string) (i: int) : (float * int) option =
        let mutable j = i

        while j < s.Length && Char.IsDigit s[j] do
            j <- j + 1

        if j = i then
            None
        else
            if j + 1 < s.Length && s[j] = '.' && Char.IsDigit s[j + 1] then
                j <- j + 1

                while j < s.Length && Char.IsDigit s[j] do
                    j <- j + 1

            if j < s.Length && (s[j] = 'e' || s[j] = 'E') then
                let mutable k = j + 1

                if k < s.Length && (s[k] = '+' || s[k] = '-') then
                    k <- k + 1

                let digits = k

                while k < s.Length && Char.IsDigit s[k] do
                    k <- k + 1

                if k > digits then
                    j <- k

            match Double.TryParse(s.AsSpan(i, j - i), NumberStyles.Float, CultureInfo.InvariantCulture) with
            | true, value -> Some(value, j)
            | _ -> None

    /// A signed whole number, as an exponent is written.
    let private integer (s: string) (i: int) : (int * int) option =
        let mutable j = i

        if j < s.Length && (s[j] = '+' || s[j] = '-') then
            j <- j + 1

        let digits = j

        while j < s.Length && Char.IsDigit s[j] do
            j <- j + 1

        if j = digits then
            None
        else
            match Int32.TryParse(s.AsSpan(i, j - i), NumberStyles.AllowLeadingSign, CultureInfo.InvariantCulture) with
            | true, value -> Some(value, j)
            | _ -> None

    /// An exponent after ** or ^: a whole number, or a ratio in parentheses.
    let private exponent (s: string) (i: int) : (Power * int) option =
        let i = spaces s i

        if i < s.Length && s[i] = '(' then
            match integer s (spaces s (i + 1)) with
            | None -> None
            | Some(numerator, j) ->
                let j = spaces s j

                if j < s.Length && s[j] = '/' then
                    match integer s (spaces s (j + 1)) with
                    | Some(denominator, k) when denominator <> 0 ->
                        let k = spaces s k

                        if k < s.Length && s[k] = ')' then
                            Some(Power.create numerator denominator, k + 1)
                        else
                            None
                    | _ -> None
                elif j < s.Length && s[j] = ')' then
                    Some(Power.ofInt numerator, j + 1)
                else
                    None
        else
            integer s i |> Option.map (fun (n, j) -> Power.ofInt n, j)

    /// A parenthesised unit, a function application, a number, or a symbol.
    let rec private atom (s: string) (i: int) : (UnitExpr * int) option =
        let i = spaces s i

        if i >= s.Length then
            None
        elif s[i] = '(' then
            match product s (i + 1) with
            | Some(inner, j) ->
                let j = spaces s j
                if j < s.Length && s[j] = ')' then
                    Some(inner, j + 1)
                else
                    None
            | None -> None
        elif Char.IsDigit s[i] then
            number s i |> Option.map (fun (value, j) -> UnitExpr.Factor value, j)
        elif isSymbolChar s[i] then
            let mutable j = i

            while j < s.Length && isSymbolChar s[j] do
                j <- j + 1

            let name = s.Substring(i, j - i)
            let afterSpaces = spaces s j

            if List.contains name functions && afterSpaces < s.Length && s[afterSpaces] = '(' then
                match product s (afterSpaces + 1) with
                | Some(inner, k) ->
                    let k = spaces s k

                    if k < s.Length && s[k] = ')' then
                        Some(UnitExpr.Apply(name, inner), k + 1)
                    else
                        None
                | None -> None
            else
                // Digits straight after a symbol are an exponent. The standard writes "cm**2",
                // but the IAU style "erg s-1 cm-2" is everywhere in real files.
                match integer s j with
                | Some(n, k) when k > j && (Char.IsDigit s[j] || s[j] = '-') ->
                    Some(UnitExpr.Raised(UnitExpr.Symbol name, Power.ofInt n), k)
                | _ -> Some(UnitExpr.Symbol name, j)
        else
            None

    /// An atom with its exponent, if it has one.
    and private power (s: string) (i: int) : (UnitExpr * int) option =
        match atom s i with
        | None -> None
        | Some(baseUnit, j) ->
            let k = spaces s j

            if k + 1 < s.Length && s[k] = '*' && s[k + 1] = '*' then
                exponent s (k + 2) |> Option.map (fun (p, m) -> UnitExpr.Raised(baseUnit, p), m)
            elif k < s.Length && s[k] = '^' then
                exponent s (k + 1) |> Option.map (fun (p, m) -> UnitExpr.Raised(baseUnit, p), m)
            else
                Some(baseUnit, j)

    /// A run of powers multiplied and divided, left to right. The solidus binds to the one
    /// component that follows it, so "J/s m**2" is (J/s) m**2 and "J/(s m**2)" is not.
    and private product (s: string) (i: int) : (UnitExpr * int) option =
        match power s i with
        | None -> None
        | Some(first, start) ->
            let mutable acc = first
            let mutable at = start
            let mutable running = true
            let mutable ok = true

            while running do
                let k = spaces s at

                let next =
                    if k < s.Length && (s[k] = '*' || s[k] = '.') then
                        Some(true, k + 1)
                    elif k < s.Length && s[k] = '/' then
                        Some(false, k + 1)
                    elif k > at && k < s.Length && (s[k] = '(' || Char.IsDigit s[k] || isSymbolChar s[k]) then
                        // A space on its own multiplies.
                        Some(true, k)
                    else
                        None

                match next with
                | None -> running <- false
                | Some(multiply, from) ->
                    match power s from with
                    | Some(operand, m) ->
                        acc <-
                            if multiply then
                                UnitExpr.Product(acc, operand)
                            else
                                UnitExpr.Quotient(acc, operand)

                        at <- m
                    | None ->
                        ok <- false
                        running <- false

            if ok then Some(acc, at) else None

    /// Parses a FITS unit string. Surrounding square brackets, the convention for a unit written
    /// in a comment field, are stripped. Returns None when the string is not a unit expression.
    let tryParse (text: string) : UnitExpr option =
        let trimmed = text.Trim()

        let inner =
            if trimmed.Length >= 2 && trimmed.StartsWith "[" && trimmed.EndsWith "]" then
                trimmed.Substring(1, trimmed.Length - 2).Trim()
            else
                trimmed

        if inner = "" || inner = "1" then
            Some UnitExpr.Unity
        else
            match product inner 0 with
            | Some(expr, j) when spaces inner j = inner.Length -> Some expr
            | _ -> None

    let private isCompound (expr: UnitExpr) =
        match expr with
        | UnitExpr.Product _
        | UnitExpr.Quotient _ -> true
        | _ -> false

    let private formatPower (p: Power) =
        if p.Denominator = 1 && p.Numerator >= 0 then
            string p.Numerator
        else
            $"({p})"

    /// The unit string for an expression, in the form the standard writes.
    let rec format (expr: UnitExpr) : string =
        let bracketed (e: UnitExpr) =
            let text = format e
            if isCompound e then $"({text})" else text

        match expr with
        | UnitExpr.Unity -> "1"
        | UnitExpr.Symbol symbol -> symbol
        | UnitExpr.Factor value -> value.ToString("R", CultureInfo.InvariantCulture)
        | UnitExpr.Raised(baseUnit, p) -> bracketed baseUnit + "**" + formatPower p
        | UnitExpr.Product(a, b) -> format a + " " + bracketed b
        | UnitExpr.Quotient(a, b) -> format a + "/" + bracketed b
        | UnitExpr.Apply(name, argument) -> $"{name}({format argument})"

    /// Merges repeated symbols, drops the ones that cancel and orders what is left.
    let internal collect (terms: (string * Power) list) : (string * Power) list =
        terms
        |> List.groupBy fst
        |> List.map (fun (symbol, group) ->
            symbol, (Power.zero, group) ||> List.fold (fun acc (_, p) -> Power.add acc p)
        )
        |> List.filter (snd >> Power.isZero >> not)
        |> List.sortBy fst

    /// Expands a unit into a factor and a product of symbol powers. sqrt becomes an exponent of
    /// one half; log, ln and exp are not products of powers, so a unit using them returns None.
    let tryFlatten (expr: UnitExpr) : UnitTerms option =
        let rec go (scale: Power) (e: UnitExpr) : (float * (string * Power) list) option =
            match e with
            | UnitExpr.Unity -> Some(1.0, [])
            | UnitExpr.Symbol symbol -> Some(1.0, [ symbol, scale ])
            | UnitExpr.Factor value -> Some(value ** scale.Value, [])
            | UnitExpr.Raised(baseUnit, p) -> go (Power.multiply scale p) baseUnit
            | UnitExpr.Product(a, b) -> join (go scale a) (go scale b)
            | UnitExpr.Quotient(a, b) -> join (go scale a) (go (Power.negate scale) b)
            | UnitExpr.Apply("sqrt", argument) -> go (Power.multiply scale Power.half) argument
            | UnitExpr.Apply _ -> None

        and join a b =
            match a, b with
            | Some(fa, ta), Some(fb, tb) -> Some(fa * fb, ta @ tb)
            | _ -> None

        go Power.one expr
        |> Option.map (fun (factor, terms) -> { Factor = factor; Terms = collect terms })

    /// Parses a unit string and expands it in one step.
    let tryTerms (text: string) : UnitTerms option = tryParse text |> Option.bind tryFlatten
