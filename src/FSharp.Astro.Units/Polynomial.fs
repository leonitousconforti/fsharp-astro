namespace FSharp.Astro.Units

/// Polynomials with coefficients copied from a paper, which several of the IAU
/// series and the extinction curves are.
module internal Polynomial =

    /// Evaluates a polynomial at `x` from its coefficients in ascending order
    /// of power, `c0 + c1 x + c2 x^2 + ...`, by Horner's rule. The same
    /// operations in the same order as the nested form `c0 + x (c1 + x (c2 +
    /// ...))`, so the result is bit for bit identical, but the coefficients
    /// read the way the papers print them.
    let horner (coefficients: float[]) (x: float) : float =
        let mutable acc = 0.0

        for i in coefficients.Length - 1 .. -1 .. 0 do
            acc <- acc * x + coefficients.[i]

        acc
