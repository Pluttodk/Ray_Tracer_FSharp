namespace Tracer.Basics

type Vector(x:float, y:float, z:float) = 
    static let zero = Vector(0.,0.,0.)
    
    // Private fields
    let x = x
    let y = y
    let z = z

    // Public fields
    member this.X = x
    member this.Y = y
    member this.Z = z

    // Public methods
    override this.ToString() = "[" + x.ToString() + "," + y.ToString() + "," + z.ToString() + "]"
    override this.GetHashCode() = hash (this.X, this.Y, this.Z)
    override this.Equals(other) = 
        match other with
        | :? Vector as v -> if(this.X.Equals(v.X)
                               && this.Y.Equals(v.Y)
                               && this.Z.Equals(v.Z)) then true
                            else false
        | _ -> false
    member this.MkVector x y z = new Vector(x, y, z)
    member this.GetCoord = x,y,z
    member this.MultScalar s = new Vector(x*s,y*s,z*s) 
    member this.Invert = new Vector(-x,-y,-z)
    member this.IsFinite = System.Double.IsFinite x && System.Double.IsFinite y && System.Double.IsFinite z
    member this.IsZero = x = 0. && y = 0. && z = 0.
    member this.MagnitudeSquared = x*x + y*y + z*z
    member this.Magnitude =
        let scale = max (abs x) (max (abs y) (abs z))
        if scale = 0. then 0.
        elif System.Double.IsInfinity scale then infinity
        else
            let sx, sy, sz = x / scale, y / scale, z / scale
            scale * sqrt (sx*sx + sy*sy + sz*sz)
    member this.DotProduct (o: Vector) = x*o.X + y*o.Y + z*o.Z
    member this.CrossProduct (o: Vector) = 
        new Vector(y*o.Z - z*o.Y, z*o.X - x * o.Z, x * o.Y - y * o.X)
    member this.AngleBetween (a: Vector) (b: Vector) =
        if not a.IsFinite || not b.IsFinite || a = Vector.Zero || b = Vector.Zero then
            invalidArg "vector" "An angle requires two finite, nonzero vectors."
        System.Math.Acos(max -1. (min 1. (a.Normalise.DotProduct b.Normalise)))
        
    member this.Normalise: Vector =
        let scale = max (abs x) (max (abs y) (abs z))
        if scale = 0. then this
        else
            let sx, sy, sz = x / scale, y / scale, z / scale
            let length = sqrt (sx*sx + sy*sy + sz*sz)
            Vector(sx / length, sy / length, sz / length)
    member this.Round (d:int) = new Vector(System.Math.Round(x,d),System.Math.Round(y,d),System.Math.Round(z,d))
    static member Zero = zero
    static member DivideByInt(a: Vector, s: int): Vector = a.MultScalar (1. / float s)

    // Operators
    static member ( ~- ) (v: Vector) = new Vector(-v.X,-v.Y,-v.Z)
    static member ( + ) (u: Vector, v:Vector) = new Vector(u.X+v.X, u.Y+v.Y, u.Z+v.Z)
    static member ( + ) (s:float, v:Vector) = v + new Vector(s,s,s)
    static member ( + ) (v:Vector, s:float) = v + new Vector(s,s,s)
    static member ( - ) (u: Vector,v: Vector) = new Vector(u.X-v.X, u.Y-v.Y, u.Z-v.Z)
    static member ( * ) (s:float, v:Vector) = v.MultScalar s
    static member ( * ) (v:Vector, s:float) = v.MultScalar s
    static member ( * ) (u:Vector, v:Vector) = u.DotProduct v
    static member ( *+ ) (u:Vector, v:Vector) = new Vector(u.X * v.X, u.Y * v.Y, u.Z * v.Z)
    static member ( ** ) (e:int, v:Vector) = new Vector(pown v.X e, pown v.Y e, pown v.Z e)
    static member ( ** ) (v:Vector, e:int) = new Vector(pown v.X e, pown v.Y e, pown v.Z e)
    static member ( % ) (u:Vector, v:Vector) = u.CrossProduct v
    static member ( / ) (v:Vector, f:float): Vector = v.MultScalar (1.0/f)
    static member ( / ) (u:Vector, v:Vector) = new Vector(u.X/v.X,u.Y/v.Y,u.Z/v.Z)