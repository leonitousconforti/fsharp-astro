namespace FSharp.Astro.Fits

open System

// The random groups layout of section 6 of the standard: a primary HDU with NAXIS1 = 0 and
// GROUPS = T, holding GCOUNT groups, each of PCOUNT parameters followed by an array of the shape
// the remaining NAXISn declare. Everything in the data unit is stored in the one BITPIX type.
//
// The layout is legacy, kept alive by radio interferometry UV data. Nothing new should be written
// in it, which is why the writer here asks for the groups already in stored form rather than
// offering the conveniences the image and table writers do.

/// What the header says about one group parameter.
type ParameterInfo = {
    /// One-based parameter number, as in PTYPEn.
    Number: int
    /// PTYPEn, or empty when absent. Several parameters may share a name, which is how a value too
    /// wide for the stored type is split across two of them.
    Name: string
    /// PSCALn.
    Scale: float
    /// PZEROn.
    Zero: float
}

/// The random groups of a primary HDU, with the data unit loaded. Groups are decoded on request.
type RandomGroups = {
    Header: Header
    /// GCOUNT, the number of groups.
    Count: int
    Parameters: ParameterInfo[]
    /// Axis lengths of one group's array in C order. NAXIS1 is dropped, being the zero that marks
    /// the layout, so the last entry is NAXIS2 and varies fastest on disk.
    Shape: int[]
    /// The type every value in the data unit is stored as.
    BitPix: BitPix
    /// BZERO, the offset applied to stored array values to get physical values.
    BZero: float
    /// BSCALE, the factor applied to stored array values to get physical values.
    BScale: float
    /// BLANK, the stored integer value that means undefined.
    Blank: int64 option
    /// The entire data unit.
    Bytes: byte[]
} with

    /// Elements in one group's array.
    member this.GroupLength: int =
        if this.Shape.Length = 0 then
            0
        else
            this.Shape |> Array.fold (*) 1

    /// Bytes one group occupies: its parameters and then its array.
    member this.GroupSize: int =
        (this.Parameters.Length + this.GroupLength) * this.BitPix.Size

/// One group in stored form, for writing.
type Group = {
    /// The parameter values, one per PTYPEn, as stored rather than scaled.
    Parameters: ImageData
    /// The array values, as stored rather than scaled.
    Data: ImageData
}

/// Reading, building and encoding of random groups.
[<RequireQualifiedAccess>]
module Groups =

    /// Interprets the data unit of a random groups HDU. Under strict handling any violation is an
    /// error. Under lenient handling recoverable violations come back as warnings.
    let tryOfHdu
        (strictness: Strictness)
        (info: HduInfo)
        (bytes: byte[])
        : Result<RandomGroups * Issue list, Issue list> =
        if info.Kind <> HduKind.RandomGroups then
            invalidOp $"HDU {info.Index} is a {info.Kind}, not random groups"

        let locate (violation: Violation) = { Issue.create violation with Hdu = Some info.Index }

        let parameters = [|
            for n in 1 .. int info.PCount ->
                {
                    Number = n
                    Name = defaultArg (Header.tryString $"PTYPE{n}" info.Header) ""
                    Scale = defaultArg (Header.tryFloat $"PSCAL{n}" info.Header) 1.0
                    Zero = defaultArg (Header.tryFloat $"PZERO{n}" info.Header) 0.0
                }
        |]

        // NAXIS1 is the zero that marks the layout, so the array shape is the rest of the axes.
        let shape = Image.shapeOfAxes info.Axes[1..]

        let groups = {
            Header = info.Header
            Count = int info.GCount
            Parameters = parameters
            Shape = shape
            BitPix = info.BitPix
            BZero = defaultArg (Header.tryFloat "BZERO" info.Header) 0.0
            BScale = defaultArg (Header.tryFloat "BSCALE" info.Header) 1.0
            Blank = Header.tryInt "BLANK" info.Header
            Bytes = bytes
        }

        let needed = int64 groups.Count * int64 groups.GroupSize

        let issues = [
            if needed > int64 bytes.Length then
                locate (Violation.TruncatedData(needed, int64 bytes.Length))
        ]

        if issues |> List.exists Issue.isFatal then
            Error issues
        elif strictness = Strict && not issues.IsEmpty then
            Error issues
        else
            Ok(groups, issues)

    let private checkIndex (groups: RandomGroups) (index: int) =
        if index < 0 || index >= groups.Count then
            invalidArg (nameof index) $"group {index} is outside the {groups.Count} groups"

    /// The stored values of one group's parameters, in the type BITPIX declares.
    let storedParameters (index: int) (groups: RandomGroups) : ImageData =
        checkIndex groups index
        let size = groups.BitPix.Size
        let at = index * groups.GroupSize
        Image.decode groups.BitPix (ReadOnlySpan(groups.Bytes, at, groups.Parameters.Length * size))

    /// Physical values of one group's parameters: PZEROn + PSCALn * stored.
    let parameters (index: int) (groups: RandomGroups) : float[] =
        let stored =
            Image.toFloat64 (Image.create [| groups.Parameters.Length |] (storedParameters index groups))

        stored
        |> Array.mapi (fun i value ->
            let p = groups.Parameters[i]
            p.Zero + p.Scale * value
        )

    /// The array of one group as an image, carrying the BZERO, BSCALE and BLANK of the HDU, so
    /// that Image.toFloat64 and the rest of the image functions work on it.
    let image (index: int) (groups: RandomGroups) : Image =
        checkIndex groups index
        let size = groups.BitPix.Size
        let at = index * groups.GroupSize + groups.Parameters.Length * size

        {
            Shape = groups.Shape
            Data = Image.decode groups.BitPix (ReadOnlySpan(groups.Bytes, at, groups.GroupLength * size))
            BZero = groups.BZero
            BScale = groups.BScale
            Blank = groups.Blank
        }

    /// The one-based numbers of the parameters with this name, ignoring case. A name may be shared
    /// by two parameters whose values are added to recover one quantity, so this gives them all.
    let numbersOf (name: string) (groups: RandomGroups) : int[] =
        groups.Parameters
        |> Array.filter (fun p -> String.Equals(p.Name, name, StringComparison.OrdinalIgnoreCase))
        |> Array.map (fun p -> p.Number)

    /// Physical values of one parameter across every group, by one-based number.
    let parameterValues (number: int) (groups: RandomGroups) : float[] =
        if number < 1 || number > groups.Parameters.Length then
            invalidArg (nameof number) $"parameter {number} is outside the {groups.Parameters.Length} parameters"

        Array.init groups.Count (fun g -> (parameters g groups)[number - 1])

    /// Physical values of a named parameter across every group, summing the parameters that share
    /// the name, which is the convention for a value too wide for the stored type.
    let tryParameterValues (name: string) (groups: RandomGroups) : float[] option =
        match numbersOf name groups with
        | [||] -> None
        | numbers ->
            numbers
            |> Array.map (fun n -> parameterValues n groups)
            |> Array.reduce (Array.map2 (+))
            |> Some

    // Building

    /// A parameter with no scaling.
    let parameterInfo (name: string) : ParameterInfo = { Number = 0; Name = name; Scale = 1.0; Zero = 0.0 }

    /// A parameter whose physical value is PZERO + PSCAL * stored.
    let withScaling (zero: float) (scale: float) (parameter: ParameterInfo) : ParameterInfo = {
        parameter with
            Zero = zero
            Scale = scale
    }

    /// The PTYPEn, PSCALn and PZEROn cards for parameters, numbered from one in order.
    let parameterCards (parameters: ParameterInfo seq) : Card list = [
        for i, p in Seq.indexed parameters do
            let n = i + 1

            if p.Name <> "" then
                Card.Value($"PTYPE{n}", CardValue.String p.Name, Some "name of parameter")

            if p.Scale <> 1.0 then
                Card.Value($"PSCAL{n}", CardValue.Real p.Scale, Some "physical = PZERO + PSCAL * stored")

            if p.Zero <> 0.0 then
                Card.Value($"PZERO{n}", CardValue.Real p.Zero, Some "physical = PZERO + PSCAL * stored")
    ]

    /// Checks that every group holds the declared number of parameters and array elements, all of
    /// one type, and returns that type.
    let private validate (shape: int[]) (parameterCount: int) (groups: Group list) : BitPix =
        let expected = if shape.Length = 0 then 0 else shape |> Array.fold (*) 1

        let types =
            groups
            |> List.collect (fun g ->
                if g.Parameters.Length <> parameterCount then
                    invalidArg
                        (nameof groups)
                        $"a group holds {g.Parameters.Length} parameters but {parameterCount} are declared"

                if g.Data.Length <> expected then
                    invalidArg
                        (nameof groups)
                        $"a group holds {g.Data.Length} elements but the shape implies {expected}"

                [ g.Parameters.BitPix; g.Data.BitPix ]
            )
            |> List.distinct

        match types with
        | [] -> BitPix.UInt8
        | [ one ] -> one
        | _ -> invalidArg (nameof groups) $"the groups are not all stored as one type: %A{types}"

    /// Encodes groups into the bytes of a data unit, parameters before the array of each. Returns
    /// the stored type and the bytes.
    let encode (shape: int[]) (parameterCount: int) (groups: Group list) : BitPix * byte[] =
        let bitPix = validate shape parameterCount groups

        let bytes =
            groups
            |> List.collect (fun g -> [ Image.encode g.Parameters; Image.encode g.Data ])
            |> Array.concat

        bitPix, bytes
