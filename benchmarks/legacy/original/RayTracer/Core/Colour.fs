namespace Tracer.Basics

exception ColourException
type Colour (r:float, g:float, b:float)= 
    
    //- CONSTRUCTOR VALIDATION 
    do
        if r < 0.0 || g < 0.0 || b < 0.0 then
            raise ColourException

    //- PRIVATE FIELDS
    let r = r
    let g = g
    let b = b

    //- PUBLIC FIELDS
    member this.R = r
    member this.G = g
    member this.B = b

    //- PUBLIC METHODS
    override this.ToString() = 
        "["+r.ToString()+","+g.ToString()+","+b.ToString()+"]"
    member this.Scale (s:float) = 
        if s < 0.0 then Colour.White
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
      System.Drawing.Color.FromArgb(max 0 (min (int (sqrt r*255.0)) 255),
                                    max 0 (min (int (sqrt g*255.0)) 255),
                                    max 0 (min (int (sqrt b*255.0)) 255))

    new (c:System.Drawing.Color) = 
        let newR = (System.Math.Pow (float c.R / 255.0, 2.0))
        let newG = (System.Math.Pow (float c.G / 255.0, 2.0))
        let newB = (System.Math.Pow (float c.B / 255.0, 2.0))
        Colour(newR, newG, newB)

    static member (+) (a:Colour, b:Colour) = Colour(a.R + b.R, a.G + b.G, a.B + b.B)
    static member (-) (a:Colour, b:Colour) =
        let r = max 0. (a.R-b.R)
        let g = max 0. (a.G-b.G)
        let b = max 0. (a.B-b.B)
        Colour(r,g,b)
    static member (*) (a:Colour, b:Colour) = Colour(a.R * b.R, a.G * b.G, a.B * b.B)
    static member (*) (a:Colour, s:float) = a.Scale s
    static member (*) (s:float, a:Colour) = a.Scale s
    static member (/) (s:float, a:Colour) = a.Scale(1./s)
    static member (/) (a:Colour, s:float) = a.Scale(1./s)
    static member (/) (a:Colour, s:int) = a.Scale(1./float(s))
    static member (/) (s:int, a:Colour) = a.Scale(1./float(s))
    static member DivideByInt(a: Colour, s: int) = a.Scale(1./float(s))

    // Predefined colours
    static member Zero = Colour(0.,0.,0.)
    static member Black = Colour(0., 0., 0.)
    static member Red = Colour(1., 0., 0.)
    static member Blue = Colour(0., 0., 1.)
    static member Green = Colour(0., 1., 0.)
    static member White = Colour(1.,1.,1.)
    static member Yellow = Colour(1.,1.,0.)