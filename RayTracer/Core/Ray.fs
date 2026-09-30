namespace Tracer.Basics

/// shutterTime is the instant (in scene seconds) the ray samples; it drives motion blur and is
/// unrelated to the ray parameter t used by PointAtTime/TimeAtPoint.
type Ray(origin: Point, direction: Vector, shutterTime: float) = 
    static let none = Ray(Point.Zero, Vector.Zero)
    new(origin: Point, direction: Vector) = Ray(origin, direction, 0.)
    member this.GetOrigin = origin
    member this.GetDirection = direction
    member this.ShutterTime = shutterTime
    member this.IsValid =
        origin.IsFinite && direction.IsFinite
        && (direction.X <> 0. || direction.Y <> 0. || direction.Z <> 0.)

    // Returns a point from a given time/length of the ray
    member this.PointAtTime (t:float) =
        Point(System.Math.FusedMultiplyAdd(t, direction.X, origin.X),
              System.Math.FusedMultiplyAdd(t, direction.Y, origin.Y),
              System.Math.FusedMultiplyAdd(t, direction.Z, origin.Z))

    // Returns a time/length from a given point of the ray
    member this.TimeAtPoint (p:Point) =
        if not this.IsValid then invalidOp "A ray parameter requires a finite, nonzero direction."
        if not p.IsFinite then invalidArg "p" "A ray point must be finite."
        let dx, dy, dz = abs direction.X, abs direction.Y, abs direction.Z
        let parameter coordinate origin direction =
            let difference = coordinate-origin
            if System.Double.IsInfinity difference then coordinate/direction-origin/direction
            else difference/direction
        // Use the best-conditioned component without squaring a very large/small direction.
        if dx >= dy && dx >= dz then parameter p.X origin.X direction.X
        elif dy >= dz then parameter p.Y origin.Y direction.Y
        else parameter p.Z origin.Z direction.Z

    member this.Invert = 
        Ray(origin, direction.Invert, shutterTime)

    static member None = 
        none

    override this.GetHashCode() = hash (this.GetOrigin, this.GetDirection, shutterTime)
    override this.Equals(other) =
        match other with
        | :? Ray as r -> if (this.GetOrigin.Equals(r.GetOrigin)
                         && this.GetDirection.Equals(r.GetDirection)
                         && shutterTime.Equals(r.ShutterTime)) then true
                         else false
        | _ -> false
    