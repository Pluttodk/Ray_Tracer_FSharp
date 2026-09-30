namespace Tracer.Basics

open System

type CSGOperator = Union | Intersection | Subtraction | Grouping

type CSG(s1: Shape, s2: Shape, op: CSGOperator) =
    inherit Shape()
    let combine operation a b =
        match operation with
        | Union | Grouping -> a || b
        | Intersection -> a && b
        | Subtraction -> a && not b
    let bounds = lazy (
        match op, s1.Bounds, s2.Bounds with
        | (Union | Grouping), Some a, Some b ->
            if a.IsEmpty then Some b
            elif b.IsEmpty then Some a
            else Some(BBox(a.lowPoint.Lowest b.lowPoint, a.highPoint.Highest b.highPoint))
        | Intersection, Some a, Some b ->
            Some(BBox(a.lowPoint.Highest b.lowPoint, a.highPoint.Lowest b.highPoint))
        | Intersection, Some box, None | Intersection, None, Some box -> Some box
        | Subtraction, Some box, _ -> Some box
        | _ -> None)
    let requireBounds () =
        bounds.Value |> Option.defaultWith (fun () -> invalidOp "This CSG shape does not have a finite bounding box.")

    member _.s1 = s1
    member _.s2 = s2
    member _.op = op
    member _.epsilon = 0.00005
    member _.bBox = requireBounds()
    override _.Bounds = bounds.Value
    override _.IsOpaque = s1.IsOpaque && s2.IsOpaque
    override _.getBoundingBox() = requireBounds()
    override _.isInside point = combine op (s1.isInside point) (s2.isInside point)

    member private this.CopyHit(ray: Ray, time: float, hit: HitPoint, reverse: bool) =
        let geometric, shading =
            if reverse then -hit.GeometricNormal, -hit.ShadingNormal
            else hit.GeometricNormal, hit.ShadingNormal
        HitPoint(ray, time, geometric, shading, hit.Material, this, hit.U, hit.V,
                 hit.BarycentricBeta, hit.BarycentricGamma, hit.DidHit)

    member private this.Trace(operation, ray: Ray, minimum: float, maximum: float) =
        if not ray.IsValid || minimum >= maximum then HitPoint(ray)
        else
            let mutable hit1, hit2 =
                if operation = Grouping then
                    Geometry.hitWithin s1 ray minimum maximum, Geometry.hitWithin s2 ray minimum maximum
                else s1.hitFunction ray, s2.hitFunction ray
            let valid (hit: HitPoint) = hit.DidHit && hit.Time > 0. && Double.IsFinite hit.Time
            let time hit = if valid hit then hit.Time else infinity
            if operation = Grouping then
                let hit = if time hit1 <= time hit2 then hit1 else hit2
                if valid hit then this.CopyHit(ray, hit.Time, hit, false) else HitPoint(ray)
            else
                let d, o = ray.GetDirection, ray.GetOrigin
                let maxDirection = max (abs d.X) (max (abs d.Y) (abs d.Z))
                let originScale = max (abs o.X) (max (abs o.Y) (abs o.Z)) / maxDirection
                let tolerance t = 32. * 2.2204460492503131e-16 * max (abs t) originScale
                let advance t = Math.BitIncrement(t + tolerance t)
                let firstTime = min (time hit1) (time hit2)
                let initialTime = min (advance 0.) (firstTime / 2.)
                let initialPoint = ray.PointAtTime initialTime
                let mutable inside1, inside2 = s1.isInside initialPoint, s2.isInside initialPoint
                let nextHit (shape: Shape) after =
                    Geometry.hitWithin shape ray after infinity
                let mutable result = HitPoint(ray)
                let mutable searching = true
                let mutable events = 0
                while searching do
                    let t1, t2 = time hit1, time hit2
                    let t = min t1 t2
                    if not (Double.IsFinite t) || t >= maximum then searching <- false
                    else
                        events <- events + 1
                        if events > 100000 then
                            invalidOp "CSG exceeded 100000 boundary events; check the input solids for degeneracy."
                        let at1, at2 = abs (t1-t) <= tolerance t, abs (t2-t) <= tolerance t
                        let before = combine operation inside1 inside2
                        let dot1 = if at1 then d * hit1.GeometricNormal else 0.
                        let dot2 = if at2 then d * hit2.GeometricNormal else 0.
                        let after1 = if not at1 || dot1 = 0. then inside1 else dot1 < 0.
                        let after2 = if not at2 || dot2 = 0. then inside2 else dot2 < 0.
                        let after = combine operation after1 after2
                        let tangent1 =
                            at1 && dot1 = 0.
                            && (match operation with Union -> not inside2 | Intersection -> inside2 | Subtraction -> not inside2 | _ -> true)
                        let tangent2 =
                            at2 && dot2 = 0.
                            && (match operation with Union -> not inside1 | Intersection | Subtraction -> inside1 | _ -> true)
                        if (before <> after || tangent1 || tangent2) && t > minimum then
                            let fromFirst =
                                if tangent1 then true
                                elif tangent2 then false
                                elif at1 && not at2 then true
                                elif at2 && not at1 then false
                                else
                                    // Select a boundary whose oriented normal agrees with the resulting transition.
                                    (dot1 < 0.) = after
                            let hit = if fromFirst then hit1 else hit2
                            result <- this.CopyHit(ray, t, hit, operation = Subtraction && not fromFirst)
                            searching <- false
                        else
                            inside1 <- after1
                            inside2 <- after2
                            if at1 then hit1 <- nextHit s1 t
                            if at2 then hit2 <- nextHit s2 t
                result

    member this.unionHitFunction ray = this.Trace(Union, ray, 0., infinity)
    member this.groupingHitFunction ray = this.Trace(Grouping, ray, 0., infinity)
    member this.unionHitFunctionInside (originalRay: Ray) (ray: Ray) =
        let hit = this.Trace(Union, ray, 0., infinity)
        if hit.DidHit then this.CopyHit(originalRay, originalRay.TimeAtPoint hit.Point, hit, false)
        else HitPoint(originalRay)
    member this.intersectionHitFunction (originalRay: Ray) (ray: Ray) =
        let hit = this.Trace(Intersection, ray, 0., infinity)
        if hit.DidHit then this.CopyHit(originalRay, originalRay.TimeAtPoint hit.Point, hit, false)
        else HitPoint(originalRay)
    member this.subtractionHitFunction (originalRay: Ray) (ray: Ray) =
        let hit = this.Trace(Subtraction, ray, 0., infinity)
        if hit.DidHit then this.CopyHit(originalRay, originalRay.TimeAtPoint hit.Point, hit, false)
        else HitPoint(originalRay)
    override this.hitFunction ray = this.Trace(op, ray, 0., infinity)
    interface IIntervalShape with
        member this.HitWithin(ray, minimum, maximum) = this.Trace(op, ray, minimum, maximum)
