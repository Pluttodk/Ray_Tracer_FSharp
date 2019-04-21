namespace Tracer.Basics

type Vector(x:float, y:float, z:float) = 
    
    // Private fields
    let x = x
    let y = y
    let z = z
    let magnitude = sqrt (x*x + y*y + z*z)

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
    member this.Magnitude = magnitude
    member this.DotProduct (o: Vector) = x*o.X + y*o.Y + z*o.Z
    member this.CrossProduct (o: Vector) = 
        new Vector(y*o.Z - z*o.Y, z*o.X - x * o.Z, x * o.Y - y * o.X)
    member this.AngleBetween (a: Vector) (b: Vector) =
        (a.Magnitude * b.Magnitude) / (a.DotProduct b)
        
    member this.Normalise = 
      match this.Magnitude with
      | 0.        -> this
      | length    -> new Vector (x / length, y / length, z / length)
    member this.Round (d:int) = new Vector(System.Math.Round(x,d),System.Math.Round(y,d),System.Math.Round(z,d))
    static member Zero = Vector(0.,0.,0.)
    static member DivideByInt(a: Vector, s: int) = a / float(s)

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
    static member ( / ) (v:Vector, f:float) = v.MultScalar (1.0/f)
    static member ( / ) (u:Vector, v:Vector) = new Vector(u.X/v.X,u.Y/v.Y,u.Z/v.Z)