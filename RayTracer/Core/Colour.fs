namespace Tracer.Basics

open System

exception ColourException

/// Linear RGB radiance.
///
/// This is a STRUCT. The path integrator performs several colour operations per
/// path vertex - throughput multiply, radiance accumulate, and a handful per
/// light sample - and as a reference type each one was a heap allocation.
/// `default(Colour)` is (0,0,0), which is black and therefore always valid, so
/// the zero-initialized struct needs no constructor to have run.
[<Struct; NoComparison>]
type Colour (r:float, g:float, b:float)=

    // The primary constructor is UNCHECKED.
    //
    // It used to validate finiteness and non-negativity, but a struct cannot
    // have a `do` binding - the default constructor would skip it - and the
    // check also ran on every colour operation in the hot path. Use
    // `Colour.Checked` where an invariant needs enforcing. Rendered output is
    // still validated at the boundary: the worker rejects any non-finite,
    // negative or non-float32-representable pixel before writing an image, so
    // a bad value cannot escape into a file unnoticed.

    //- PUBLIC FIELDS
    member this.R = r
    member this.G = g
    member this.B = b
    member this.IsBlack = r = 0. && g = 0. && b = 0.

    //- PUBLIC METHODS
    override this.ToString() = 
        "["+r.ToString()+","+g.ToString()+","+b.ToString()+"]"
    member this.Scale (s:float) = 
        if not (Double.IsFinite s) || s < 0.0 then invalidArg (nameof s) "A colour scale must be finite and nonnegative."
        elif s = 0. then Colour(0., 0., 0.)
        elif s = 1. then this
        else Colour(r*s,g*s,b*s)
    member this.Merge (w: float) (c: Colour) =
        let w' = 1.0 - w
        if w >= 0.0 && w <= 1.0 then
          Colour(w*r + w'*c.R, w*g + w'*c.G, w*b + w'*c.B)
        else
          raise ColourException

    member this.Average = 
        (r + b + g) / 3.

    member this.ToColor = 
      System.Drawing.Color.FromArgb(int (min 1. (sqrt r) * 255.),
                                    int (min 1. (sqrt g) * 255.),
                                    int (min 1. (sqrt b) * 255.))

    /// ACES filmic tone mapping curve (Narkowicz 2015 approximation).
    ///
    /// The other transfers hard-clip at 1.0, so every value above white lands on
    /// exactly white and all detail in a highlight disappears into a flat patch.
    /// Real film and real cameras roll off instead, which is a large part of why
    /// a clipped render reads as computer graphics. This compresses the highlight
    /// shoulder and adds a slight toe, then sRGB-encodes the result.
    static member private AcesFilmic(value: float) =
        let x = max 0. value
        let mapped = (x * (2.51 * x + 0.03)) / (x * (2.43 * x + 0.59) + 0.14)
        max 0. (min 1. mapped)

    static member private EncodeSrgb(value: float) =
        if value <= 0.0031308 then 12.92 * value
        else 1.055 * value ** (1. / 2.4) - 0.055

    member this.ToDisplayColor(transfer: string) =
        let convert value =
            let encoded =
                match transfer with
                | "gamma2" -> sqrt (min 1. value)
                | "srgb" ->
                    let value = min 1. value
                    if value = 1. then 1. else Colour.EncodeSrgb value
                | "linear" -> min 1. value
                | "aces" -> Colour.EncodeSrgb(Colour.AcesFilmic value)
                | _ -> invalidArg (nameof transfer) "Unknown colour transfer; use gamma2, srgb, linear, or aces."
            int (max 0. (min 1. encoded) * 255.)
        System.Drawing.Color.FromArgb(convert r, convert g, convert b)

    new (c:System.Drawing.Color) = 
        let newR = (System.Math.Pow (float c.R / 255.0, 2.0))
        let newG = (System.Math.Pow (float c.G / 255.0, 2.0))
        let newB = (System.Math.Pow (float c.B / 255.0, 2.0))
        Colour(newR, newG, newB)

    static member (+) (a:Colour, b:Colour) =
        if a.IsBlack then b
        elif b.IsBlack then a
        else Colour(a.R + b.R, a.G + b.G, a.B + b.B)
    static member (-) (a:Colour, b:Colour) =
        let r = max 0. (a.R-b.R)
        let g = max 0. (a.G-b.G)
        let b = max 0. (a.B-b.B)
        Colour(r,g,b)
    static member (*) (a:Colour, b:Colour) =
        if a.IsBlack || b.IsBlack then Colour(0., 0., 0.)
        else Colour(a.R * b.R, a.G * b.G, a.B * b.B)
    static member (*) (a:Colour, s:float) = a.Scale s
    static member (*) (s:float, a:Colour) = a.Scale s
    static member (/) (s:float, a:Colour) = Colour(s / a.R, s / a.G, s / a.B)
    static member (/) (a:Colour, s:float) = a.Scale(1./s)
    static member (/) (a:Colour, s:int) = a.Scale(1./float(s))
    static member (/) (s:int, a:Colour) = Colour(float s / a.R, float s / a.G, float s / a.B)
    static member DivideByInt(a: Colour, s: int) = a.Scale(1./float(s))

    // Predefined colours
    /// Validating constructor. Raises ColourException on a non-finite or
    /// negative component.
    static member Checked(r: float, g: float, b: float) =
        if not (Double.IsFinite r && Double.IsFinite g && Double.IsFinite b) || r < 0.0 || g < 0.0 || b < 0.0 then
            raise ColourException
        Colour(r, g, b)

    static member Zero = Colour(0., 0., 0.)
    static member Black = Colour(0., 0., 0.)
    static member Red = Colour(1., 0., 0.)
    static member Blue = Colour(0., 0., 1.)
    static member Green = Colour(0., 1., 0.)
    static member White = Colour(1., 1., 1.)
    static member Yellow = Colour(1., 1., 0.)