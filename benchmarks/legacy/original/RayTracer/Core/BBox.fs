namespace Tracer.Basics

type BBox(lowPoint:Point, highPoint:Point) =
    member this.lowPoint = lowPoint
    member this.highPoint = highPoint
    member this.isInside (p:Point) =
        if lowPoint.X <= p.X && p.X <= highPoint.X then
            if lowPoint.Y <= p.Y && p.Y <= highPoint.Y then
                if lowPoint.Z <= p.Z && p.Z <= highPoint.Z then true
                else false
            else false
        else false

    override this.ToString() =
            "BBox(Max: "+highPoint.ToString()+", Min: "+lowPoint.ToString()+")"
    override this.GetHashCode() =
        hash (lowPoint, highPoint)
    override this.Equals(x) = 
        match x with
        | :? BBox as box -> this.highPoint = box.highPoint && 
                                    this.lowPoint = box.lowPoint
        | _ -> false
    member this.intersect (r:Ray) =
        let boolX = r.GetDirection.X >= 0.0
        let boolY = r.GetDirection.Y >= 0.0
        let boolZ = r.GetDirection.Z >= 0.0
        
        let tx =  match boolX with
                  |true -> (lowPoint.X - r.GetOrigin.X)/r.GetDirection.X 
                  |false -> (highPoint.X - r.GetOrigin.X)/r.GetDirection.X
        let tx' = match boolX with
                  |true -> (highPoint.X - r.GetOrigin.X)/r.GetDirection.X 
                  |false -> (lowPoint.X - r.GetOrigin.X)/r.GetDirection.X
        let ty =  match boolY with
                  |true -> (lowPoint.Y - r.GetOrigin.Y)/r.GetDirection.Y 
                  |false -> (highPoint.Y - r.GetOrigin.Y)/r.GetDirection.Y
        let ty' = match boolY with
                  |true -> (highPoint.Y - r.GetOrigin.Y)/r.GetDirection.Y 
                  |false -> (lowPoint.Y - r.GetOrigin.Y)/r.GetDirection.Y
        let tz =  match boolZ with
                  |true -> (lowPoint.Z - r.GetOrigin.Z)/r.GetDirection.Z 
                  |false -> (highPoint.Z - r.GetOrigin.Z)/r.GetDirection.Z
        let tz' = match boolZ with
                  |true -> (highPoint.Z - r.GetOrigin.Z)/r.GetDirection.Z 
                  |false -> (lowPoint.Z - r.GetOrigin.Z)/r.GetDirection.Z
        

        let t = max tx (max ty tz)

        let t' = min tx' (min ty' tz')

        match (t < t' && t' > 0.0) with
        |true -> Some(t, t') 
        |false -> None

    member this.intersectRG (r:Ray) =
        let boolX = r.GetDirection.X >= 0.0
        let boolY = r.GetDirection.Y >= 0.0
        let boolZ = r.GetDirection.Z >= 0.0
        
        let tx = if boolX then (lowPoint.X - r.GetOrigin.X)/r.GetDirection.X else (highPoint.X - r.GetOrigin.X)/r.GetDirection.X
        let tx' = if boolX then (highPoint.X - r.GetOrigin.X)/r.GetDirection.X else (lowPoint.X - r.GetOrigin.X)/r.GetDirection.X
        let ty = if boolY then (lowPoint.Y - r.GetOrigin.Y)/r.GetDirection.Y else (highPoint.Y - r.GetOrigin.Y)/r.GetDirection.Y
        let ty' = if boolY then (highPoint.Y - r.GetOrigin.Y)/r.GetDirection.Y else (lowPoint.Y - r.GetOrigin.Y)/r.GetDirection.Y
        let tz = if boolZ then (lowPoint.Z - r.GetOrigin.Z)/r.GetDirection.Z else (highPoint.Z - r.GetOrigin.Z)/r.GetDirection.Z
        let tz' = if boolZ then (highPoint.Z - r.GetOrigin.Z)/r.GetDirection.Z else (lowPoint.Z - r.GetOrigin.Z)/r.GetDirection.Z
        

        let t = max tx (max ty tz)

        let t' = min tx' (min ty' tz')

        if t < t' && t' > 0.0 then Some(t, t', tx, ty, tz, tx', ty', tz') else None

    member this.boundingBoxIntersect (other:BBox) : bool =
        let newLow = Point((min lowPoint.X other.lowPoint.X), (min lowPoint.Y other.lowPoint.Y), (min lowPoint.Z other.lowPoint.Z))
        let newHigh = Point((max highPoint.X other.highPoint.X), (max highPoint.Y other.highPoint.Y), (max highPoint.Z other.highPoint.Z))
        
        let halfDistX = (newHigh.X-newLow.X)/2.
        let halfDistY = (newHigh.Y-newLow.Y)/2.
        let halfDistZ = (newHigh.Z-newLow.Z)/2.

        lowPoint.X <= halfDistX && halfDistX <= newHigh.X
        || lowPoint.Y <= halfDistY && halfDistY <= newHigh.Y
        || lowPoint.Z <= halfDistZ && halfDistZ <= newHigh.Z



