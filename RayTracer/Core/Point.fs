namespace Tracer.Basics

type Point(x:float, y:float, z:float) = 
    static let zero = Point(0.,0.,0.)
    //- PRIVATE FIELDS
    let x = x
    let y = y
    let z = z

    //- PUBLIC FIELDS
    member this.X = x
    member this.Y = y
    member this.Z = z
    member this.IsFinite = System.Double.IsFinite x && System.Double.IsFinite y && System.Double.IsFinite z
    
    //- PUBLIC METHODS
    override this.ToString() = 
        "("+x.ToString()+","+y.ToString()+","+z.ToString()+")"
    override this.GetHashCode() = hash (this.X, this.Y, this.Z)
    override this.Equals (point) =
        match point with
        | :? Point as p -> this.X.Equals(p.X) && this.Y.Equals(p.Y) && this.Z.Equals(p.Z)
        | _ -> false
    member this.GetCoord = (x,y,z)
    member this.Move (v: Vector) = new Point(x+v.X, y+v.Y, z+v.Z)
    member this.Distance (q: Point) = new Vector(q.X-x, q.Y-y, q.Z-z)
    member this.Direction (p:Point) (q:Point) = p.Distance(q).Normalise
    member this.Round (d:int) = new Point(System.Math.Round(x,d), System.Math.Round(y,d), System.Math.Round(z,d))
    member this.ToVector = new Vector(x,y,z)
    member this.Lowest (p:Point) =
        Point(min x p.X, min y p.Y, min z p.Z)
    member this.Highest (p:Point) =
        Point(max x p.X, max y p.Y, max z p.Z)
    static member ( + ) (p:Point, v: Vector) : Point = p.Move v 
    static member ( - ) (p:Point, v: Vector) : Point = 
        p.Move v.Invert
    member this.OrthonormalTransform (u: Vector, v: Vector, w: Vector) = 
        let newX = u * x
        let newY = v * y
        let newZ = w * z
        newX + newY + newZ

    static member Zero = zero
    static member ( - ) (p1: Point, p2: Point) = new Vector(p1.X - p2.X, p1.Y - p2.Y, p1.Z - p2.Z)
    static member ( * ) (p: Point, n: float) = new Point((p.X*n), (p.Y*n), (p.Z*n))
    static member ( * ) (n: float, p: Point) = new Point((p.X*n), (p.Y*n), (p.Z*n))
    static member ( - ) (v:Vector, p:Point) : Vector = 
        new Vector (((v.X) - (p.X)), ((v.Y) - (p.Y)), ((v.Z) - (p.Z)))
    static member ( - ) (p: Point, n:float) = new Point((p.X-n), (p.Y-n), (p.Z-n))
    static member ( / ) (p: Point, n:float) = new Point((p.X/n), (p.Y/n), (p.Z/n))
    static member ( / ) (n:float, p: Point) = new Point((p.X/n), (p.Y/n), (p.Z/n))

    new(hemispherePoint: float * float * float) = 
        let (x,y,z) = hemispherePoint
        Point(x,y,z)
    new(vector: Vector) = 
        Point(vector.X, vector.Y, vector.Z)