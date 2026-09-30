namespace Tracer.Basics

open System

type BBox(lowPoint: Point, highPoint: Point) =
    let empty =
        lowPoint.X > highPoint.X || lowPoint.Y > highPoint.Y || lowPoint.Z > highPoint.Z
        || Double.IsNaN lowPoint.X || Double.IsNaN lowPoint.Y || Double.IsNaN lowPoint.Z
        || Double.IsNaN highPoint.X || Double.IsNaN highPoint.Y || Double.IsNaN highPoint.Z

    let slab origin direction low high =
        if direction = 0. then
            if origin < low || origin > high then struct (infinity, -infinity)
            else struct (-infinity, infinity)
        else
            let parameter bound =
                let difference = bound-origin
                if Double.IsFinite bound && Double.IsInfinity difference then bound/direction-origin/direction
                else difference/direction
            let a, b = parameter low, parameter high
            struct (min a b, max a b)

    let intervals (ray: Ray) =
        let o, d = ray.GetOrigin, ray.GetDirection
        let struct (tx, tx') = slab o.X d.X lowPoint.X highPoint.X
        let struct (ty, ty') = slab o.Y d.Y lowPoint.Y highPoint.Y
        let struct (tz, tz') = slab o.Z d.Z lowPoint.Z highPoint.Z
        struct (max tx (max ty tz), min tx' (min ty' tz'), tx, ty, tz, tx', ty', tz')

    member _.lowPoint = lowPoint
    member _.highPoint = highPoint
    member _.IsEmpty = empty
    static member Empty = BBox(Point(infinity, infinity, infinity), Point(-infinity, -infinity, -infinity))
    member _.isInside(p: Point) =
        not empty
        && lowPoint.X <= p.X && p.X <= highPoint.X
        && lowPoint.Y <= p.Y && p.Y <= highPoint.Y
        && lowPoint.Z <= p.Z && p.Z <= highPoint.Z

    member _.IntersectInterval(ray: Ray, tMin: float, tMax: float) =
        if empty || not ray.IsValid || Double.IsNaN tMin || Double.IsNaN tMax || tMin > tMax then None
        else
            let struct (entry, exit, _, _, _, _, _, _) = intervals ray
            let lo, hi = max entry tMin, min exit tMax
            if lo <= hi then Some(lo, hi) else None

    member _.intersect(ray: Ray) =
        if empty || not ray.IsValid then None
        else
            let struct (entry, exit, _, _, _, _, _, _) = intervals ray
            if entry <= exit && exit > 0. then Some(entry, exit) else None

    member _.intersectRG(ray: Ray) =
        if empty || not ray.IsValid then None
        else
            let struct (entry, exit, tx, ty, tz, tx', ty', tz') = intervals ray
            if entry <= exit && exit > 0. then Some(entry, exit, tx, ty, tz, tx', ty', tz')
            else None

    member _.boundingBoxIntersect(other: BBox) =
        not empty && not other.IsEmpty
        && lowPoint.X <= other.highPoint.X && other.lowPoint.X <= highPoint.X
        && lowPoint.Y <= other.highPoint.Y && other.lowPoint.Y <= highPoint.Y
        && lowPoint.Z <= other.highPoint.Z && other.lowPoint.Z <= highPoint.Z

    override _.ToString() =
        "BBox(Max: " + highPoint.ToString() + ", Min: " + lowPoint.ToString() + ")"
    override _.GetHashCode() = hash (lowPoint, highPoint)
    override _.Equals(other) =
        match other with
        | :? BBox as box -> lowPoint = box.lowPoint && highPoint = box.highPoint
        | _ -> false
