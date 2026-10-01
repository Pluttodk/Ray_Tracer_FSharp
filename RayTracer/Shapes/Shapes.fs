namespace Tracer.Basics

open System
open Transformation
open Textures

module internal Geometry =
    let within minimum maximum (hit: HitPoint) =
        if hit.DidHit && hit.Time > minimum && hit.Time < maximum then hit else HitPoint(hit.Ray)

    let hitWithin (shape: Shape) (ray: Ray) minimum maximum =
        if minimum >= maximum then HitPoint(ray)
        else
            match (shape :> obj) with
            | :? IIntervalShape as interval -> interval.HitWithin(ray, minimum, maximum)
            | _ ->
                let first = shape.hitFunction ray
                if not first.DidHit || first.Time > minimum then within minimum maximum first
                else
                    let o, d = ray.GetOrigin, ray.GetDirection
                    let directionScale = max (abs d.X) (max (abs d.Y) (abs d.Z))
                    let originScale = max (abs o.X) (max (abs o.Y) (abs o.Z)) / directionScale
                    let cursor = Math.BitIncrement(minimum + 32.*2.2204460492503131e-16*max minimum originScale)
                    if not (Double.IsFinite cursor) || cursor >= maximum then HitPoint(ray)
                    else
                        let hit = shape.hitFunction(Ray(ray.PointAtTime cursor, d, ray.ShutterTime))
                        if not hit.DidHit then HitPoint(ray)
                        else
                            HitPoint(ray, cursor+hit.Time, hit.GeometricNormal, hit.ShadingNormal,
                                     hit.Material, hit.Shape, hit.U, hit.V, hit.BarycentricBeta,
                                     hit.BarycentricGamma, true) |> within minimum maximum

    let positive name value =
        if not (Double.IsFinite value) || value <= 0. then
            invalidArg name "A dimension must be finite and positive."

    let finitePoint name (point: Point) =
        if not point.IsFinite then invalidArg name "A point must have finite coordinates."

    let paddedBounds (low: Point) (high: Point) =
        let lower x = let y = x - 0.000001 in if y = x then Math.BitDecrement x else y
        let upper x = let y = x + 0.000001 in if y = x then Math.BitIncrement x else y
        BBox(Point(lower low.X, lower low.Y, lower low.Z),
             Point(upper high.X, upper high.Y, upper high.Z))

    let triangleBounds (a: Point) (b: Point) (c: Point) =
        paddedBounds ((a.Lowest b).Lowest c) ((a.Highest b).Highest c)

    let radialRoots radius ox oy oz dx dy dz =
        let directionScale = max (abs dx) (max (abs dy) (abs dz))
        let spatialScale = max radius (max (abs ox) (max (abs oy) (abs oz)))
        if directionScale = 0. || not (Double.IsFinite spatialScale) then ValueNone
        else
            let sx, sy, sz = dx/directionScale, dy/directionScale, dz/directionScale
            let length = sqrt (sx*sx + sy*sy + sz*sz)
            let ux, uy, uz = sx/length, sy/length, sz/length
            let x, y, z, radius = ox/spatialScale, oy/spatialScale, oz/spatialScale, radius/spatialScale
            let projection = -Math.FusedMultiplyAdd(x, ux, Math.FusedMultiplyAdd(y, uy, z*uz))
            let cx, cy, cz =
                Math.FusedMultiplyAdd(projection, ux, x),
                Math.FusedMultiplyAdd(projection, uy, y),
                Math.FusedMultiplyAdd(projection, uz, z)
            let distance = Vector(cx,cy,cz).Magnitude
            if radius = 0. || distance > radius then ValueNone
            else
                // Closest approach avoids catastrophic cancellation in b*b - 4*a*c for distant rays.
                let ratio = distance/radius
                let halfChord = radius * sqrt (max 0. ((1.-ratio)*(1.+ratio)))
                let parameter value =
                    let ratio = spatialScale/directionScale
                    if Double.IsFinite ratio && ratio <> 0. then value/length*ratio
                    else (value*spatialScale)/length/directionScale
                ValueSome(struct (parameter (projection-halfChord), parameter (projection+halfChord)))

    // Dominant-axis shear gives adjacent triangles identical edge tests, including shared edges.
    let intersectTriangleCoordinates (ray: Ray) ax ay az bx by bz cx cy cz =
        if not ray.IsValid then ValueNone
        else
            let d = ray.GetDirection
            let kz =
                if abs d.X >= abs d.Y && abs d.X >= abs d.Z then 0
                elif abs d.Y >= abs d.Z then 1 else 2
            let coordinate axis x y z = if axis = 0 then x elif axis = 1 then y else z
            let dc axis = if axis = 0 then d.X elif axis = 1 then d.Y else d.Z
            let kx0, ky0 = (kz + 1) % 3, (kz + 2) % 3
            let kx, ky = if dc kz < 0. then ky0, kx0 else kx0, ky0
            let sx, sy = -dc kx / dc kz, -dc ky / dc kz
            let o = ray.GetOrigin
            let vertex x y z =
                let depth = coordinate kz x y z - coordinate kz o.X o.Y o.Z
                struct (coordinate kx x y z - coordinate kx o.X o.Y o.Z + sx*depth,
                        coordinate ky x y z - coordinate ky o.X o.Y o.Z + sy*depth, depth / dc kz)
            let struct (ax, ay, az) = vertex ax ay az
            let struct (bx, by, bz) = vertex bx by bz
            let struct (cx, cy, cz) = vertex cx cy cz
            let ea, eb, ec = bx*cy - by*cx, cx*ay - cy*ax, ax*by - ay*bx
            if (ea < 0. || eb < 0. || ec < 0.) && (ea > 0. || eb > 0. || ec > 0.) then ValueNone
            else
                let determinant = ea + eb + ec
                if determinant = 0. then ValueNone
                else
                    let time = (ea*az + eb*bz + ec*cz) / determinant
                    if time > 0. && Double.IsFinite time then
                        ValueSome(struct (time, eb / determinant, ec / determinant))
                    else ValueNone

    let intersectTriangle ray (a: Point) (b: Point) (c: Point) =
        intersectTriangleCoordinates ray a.X a.Y a.Z b.X b.Y b.Z c.X c.Y c.Z

type Rectangle(bottomLeft: Point, topLeft: Point, bottomRight: Point, tex: Texture) =
    inherit Shape()
    let horizontal, vertical = bottomRight - bottomLeft, topLeft - bottomLeft
    let hh, hv, vv = horizontal * horizontal, horizontal * vertical, vertical * vertical
    let determinant = hh*vv - hv*hv
    do
        Geometry.finitePoint "bottomLeft" bottomLeft
        Geometry.finitePoint "topLeft" topLeft
        Geometry.finitePoint "bottomRight" bottomRight
        if not (Double.IsFinite determinant) || determinant <= 0. then
            invalidArg "corners" "A rectangle requires two nonzero, independent edges."
    let surfaceNormal = (horizontal % vertical).Normalise
    let topRight = topLeft + horizontal
    let bounds =
        Geometry.paddedBounds
            (((bottomLeft.Lowest topLeft).Lowest bottomRight).Lowest topRight)
            (((bottomLeft.Highest topLeft).Highest bottomRight).Highest topRight)
    member _.bottomLeft = bottomLeft
    member _.topLeft = topLeft
    member _.bottomRight = bottomRight
    member _.tex = tex
    member _.width = horizontal.Magnitude
    member _.height = vertical.Magnitude
    member _.normal = surfaceNormal
    member _.bBox = bounds
    override _.IsOpaque = Textures.isOpaque tex
    override _.isInside _ = invalidOp "Cannot be inside a two-dimensional shape."
    override _.getBoundingBox() = bounds
    override this.hitFunction(ray: Ray) =
        let denominator = ray.GetDirection * surfaceNormal
        if not ray.IsValid || denominator = 0. then HitPoint(ray)
        else
            let time = ((bottomLeft - ray.GetOrigin) * surfaceNormal) / denominator
            if time <= 0. || not (Double.IsFinite time) then HitPoint(ray)
            else
                let p = ray.PointAtTime time - bottomLeft
                let ph, pv = p * horizontal, p * vertical
                let u, v = (ph*vv - pv*hv) / determinant, (pv*hh - ph*hv) / determinant
                if u >= 0. && u <= 1. && v >= 0. && v <= 1. then
                    HitPoint(ray, time, surfaceNormal, getFunc tex u v, this, u, v)
                else HitPoint(ray)
    interface IIntervalShape with
        member this.HitWithin(ray, minimum, maximum) = this.hitFunction ray |> Geometry.within minimum maximum

type Disc(center: Point, radius: float, tex: Texture) =
    inherit Shape()
    let discRadius = abs radius
    do
        Geometry.finitePoint "center" center
        Geometry.positive "radius" discRadius
    let surfaceNormal = Vector(0., 0., 1.)
    let bounds =
        Geometry.paddedBounds (center - Vector(discRadius, discRadius, 0.)) (center + Vector(discRadius, discRadius, 0.))
    member _.center = center
    member _.radius = discRadius
    member _.tex = tex
    member _.normal = surfaceNormal
    member _.bBox = bounds
    override _.IsOpaque = Textures.isOpaque tex
    override _.isInside _ = invalidOp "Cannot be inside a two-dimensional shape."
    override _.getBoundingBox() = bounds
    override this.hitFunction(ray: Ray) =
        if not ray.IsValid || ray.GetDirection.Z = 0. then HitPoint(ray)
        else
            let time = (center.Z - ray.GetOrigin.Z) / ray.GetDirection.Z
            if time <= 0. || not (Double.IsFinite time) then HitPoint(ray)
            else
                let p = ray.PointAtTime time - center
                if Vector(p.X,p.Y,0.).Magnitude <= discRadius then
                    let u, v = (p.X + discRadius)/(2.*discRadius), (p.Y + discRadius)/(2.*discRadius)
                    HitPoint(ray, time, surfaceNormal, getFunc tex u v, this, u, v)
                else HitPoint(ray)
    interface IIntervalShape with
        member this.HitWithin(ray, minimum, maximum) = this.hitFunction ray |> Geometry.within minimum maximum

type Triangle(a: Point, b: Point, c: Point, mat: Material) =
    inherit Shape()
    do
        Geometry.finitePoint "a" a
        Geometry.finitePoint "b" b
        Geometry.finitePoint "c" c
    let edgeU, edgeV = a-b, a-c
    let areaNormal = edgeU % edgeV
    let normal = areaNormal.Normalise
    let nondegenerate = normal.IsFinite && (normal.X <> 0. || normal.Y <> 0. || normal.Z <> 0.)
    let bounds = Geometry.triangleBounds a b c
    member _.a = a
    member _.b = b
    member _.c = c
    member _.mat = mat
    member _.u = edgeU
    member _.v = edgeV
    member _.n = areaNormal
    member _.pa = edgeU.X
    member _.pb = edgeV.X
    member _.e = edgeU.Y
    member _.f = edgeV.Y
    member _.i = edgeU.Z
    member _.j = edgeV.Z
    member _.bBox = bounds
    override _.IsOpaque = not (mat :? TransparentMaterial)
    override _.isInside _ = invalidOp "Cannot be inside a two-dimensional shape."
    override _.getBoundingBox() = bounds
    override this.hitFunction ray =
        match (if nondegenerate then Geometry.intersectTriangle ray a b c else ValueNone) with
        | ValueSome(struct (time, beta, gamma)) ->
            HitPoint(ray, time, normal, normal, mat, this, 0., 0., beta, gamma, true)
        | ValueNone -> HitPoint(ray)
    interface IIntervalShape with
        member this.HitWithin(ray, minimum, maximum) = this.hitFunction ray |> Geometry.within minimum maximum

type SphereShape(origin: Point, radius: float, tex: Texture) =
    inherit Shape()
    do
        Geometry.finitePoint "origin" origin
        Geometry.positive "radius" radius
    let bounds =
        Geometry.paddedBounds (origin - Vector(radius, radius, radius)) (origin + Vector(radius, radius, radius))
    member _.origin = origin
    member _.radius = radius
    member _.tex = tex
    member _.bBox = bounds
    override _.IsOpaque = Textures.isOpaque tex
    override _.isInside(p: Point) = (p - origin).Magnitude < radius
    override _.getBoundingBox() = bounds
    member _.NormalAtPoint(p: Point) = (p - origin).Normalise
    member this.getTextureCoords(p: Point) =
        let n = this.NormalAtPoint p
        let u = Math.Atan2(n.X, n.Z) / (2.*Math.PI)
        (if u < 0. then u + 1. else u), 1. - Math.Acos(max -1. (min 1. n.Y))/Math.PI
    member this.determineHitPoint(ray: Ray) time =
        let p = ray.PointAtTime time
        let u, v = this.getTextureCoords p
        HitPoint(ray, time, this.NormalAtPoint p, getFunc tex u v, this, u, v)
    member private this.Intersect(ray: Ray, minimum: float, maximum: float) =
        if not ray.IsValid then HitPoint(ray)
        else
            let o, d = ray.GetOrigin - origin, ray.GetDirection
            match Geometry.radialRoots radius o.X o.Y o.Z d.X d.Y d.Z with
            | ValueSome(struct (near, far)) ->
                let time = if near > minimum then near else far
                if time > minimum && time < maximum && Double.IsFinite time then this.determineHitPoint ray time else HitPoint(ray)
            | ValueNone -> HitPoint(ray)
    override this.hitFunction ray = this.Intersect(ray, 0., infinity)
    interface IIntervalShape with
        member this.HitWithin(ray, minimum, maximum) = this.Intersect(ray, minimum, maximum)

type HollowCylinder(center: Point, radius: float, height: float, tex: Texture) =
    inherit Shape()
    do
        Geometry.finitePoint "center" center
        Geometry.positive "radius" radius
        Geometry.positive "height" height
    let halfHeight = height/2.
    let bounds =
        Geometry.paddedBounds (center - Vector(radius, halfHeight, radius)) (center + Vector(radius, halfHeight, radius))
    member _.center = center
    member _.radius = radius
    member _.height = height
    member _.tex = tex
    member _.bBox = bounds
    override _.IsOpaque = Textures.isOpaque tex
    override _.isInside _ = invalidOp "Cannot be inside an open cylinder."
    override _.getBoundingBox() = bounds
    member _.NormalAtPoint(p: Point) = Vector(p.X-center.X, 0., p.Z-center.Z).Normalise
    member this.getTextureCoords(p: Point) =
        let n = this.NormalAtPoint p
        let phi = Math.Atan2(n.X, n.Z)
        (if phi < 0. then phi + 2.*Math.PI else phi)/(2.*Math.PI), (p.Y-center.Y)/height + 0.5
    member this.determineHitPoint(ray: Ray) (p: Point) =
        let u, v = this.getTextureCoords p
        HitPoint(ray, ray.TimeAtPoint p, this.NormalAtPoint p, getFunc tex u v, this, u, v)
    member _.determineIfPointIsInsideHeight(p: Point) =
        p.Y >= center.Y-halfHeight && p.Y <= center.Y+halfHeight
    member private this.Intersect(ray: Ray, minimum: float, maximum: float) =
        if not ray.IsValid then HitPoint(ray)
        else
            let o, d = ray.GetOrigin - center, ray.GetDirection
            match Geometry.radialRoots radius o.X 0. o.Z d.X 0. d.Z with
            | ValueNone -> HitPoint(ray)
            | ValueSome(struct (near, far)) ->
                let valid t = t > minimum && t < maximum && Double.IsFinite t && this.determineIfPointIsInsideHeight(ray.PointAtTime t)
                let time = if valid near then near elif valid far then far else -1.
                if time <= 0. then HitPoint(ray)
                else
                    let p = ray.PointAtTime time
                    let u, v = this.getTextureCoords p
                    HitPoint(ray, time, this.NormalAtPoint p, getFunc tex u v, this, u, v)
    override this.hitFunction ray = this.Intersect(ray, 0., infinity)
    interface IIntervalShape with
        member this.HitWithin(ray, minimum, maximum) = this.Intersect(ray, minimum, maximum)

module Transform =
    let transformRay (ray: Ray) transformation =
        let inverse = getInvMatrix transformation
        Ray(transformPoint(ray.GetOrigin, inverse), transformVector(ray.GetDirection, inverse), ray.ShutterTime)

    let transformNormal (normal: Vector) transformation =
        transformVector(normal, (getInvMatrix transformation).transpose)

    let internal transformBoundsBy matrix (box: BBox) =
        if box.IsEmpty then box
        else
            let low, high = box.lowPoint, box.highPoint
            let mutable newLow = transformPoint(low, matrix)
            let mutable newHigh = newLow
            for x in [low.X; high.X] do
                for y in [low.Y; high.Y] do
                    for z in [low.Z; high.Z] do
                        let p = transformPoint(Point(x,y,z), matrix)
                        newLow <- newLow.Lowest p
                        newHigh <- newHigh.Highest p
            BBox(newLow, newHigh)

    let internal intersectLocal (shape: Shape) (owner: Shape) (ray: Ray) minimum maximum inverse (normalMatrix: QuickMatrix) =
        // Do not normalize this direction: affine transforms preserve the original ray parameter.
        let localRay = Ray(transformPoint(ray.GetOrigin, inverse), transformVector(ray.GetDirection, inverse), ray.ShutterTime)
        let hit = Geometry.hitWithin shape localRay minimum maximum
        if not hit.DidHit then HitPoint(ray)
        else
            HitPoint(ray, hit.Time, transformVector(hit.GeometricNormal, normalMatrix),
                     transformVector(hit.ShadingNormal, normalMatrix), hit.Material, owner,
                     hit.U, hit.V, hit.BarycentricBeta, hit.BarycentricGamma, true)

    /// HitWithin(...).DidHit of `intersectLocal`, without building the hit: the inner shape's occluder
    /// answers when it has one.
    let internal occludesLocal (shape: Shape) (ray: Ray) minimum maximum inverse =
        let localRay = Ray(transformPoint(ray.GetOrigin, inverse), transformVector(ray.GetDirection, inverse), ray.ShutterTime)
        match shape :> obj with
        | :? IOccluder as occluder -> minimum < maximum && occluder.Occludes(localRay, minimum, maximum)
        | _ -> (Geometry.hitWithin shape localRay minimum maximum).DidHit

    let transform (shape: Shape) transformation =
        let matrix, inverse = getMatrix transformation, getInvMatrix transformation
        let normalMatrix = inverse.transpose
        let bounds = lazy (shape.Bounds |> Option.map (transformBoundsBy matrix))
        let intersect (owner: Shape) (ray: Ray) minimum maximum =
            intersectLocal shape owner ray minimum maximum inverse normalMatrix
        { new Shape() with
            member this.hitFunction ray = intersect this ray 0. infinity
            member _.Bounds = bounds.Value
            member _.IsOpaque = shape.IsOpaque
            member _.getBoundingBox() =
                bounds.Value |> Option.defaultWith (fun () -> invalidOp "An unbounded transformed shape has no finite bounding box.")
            member _.isInside p = shape.isInside(transformPoint(p, inverse))
          interface IIntervalShape with
            member this.HitWithin(ray, minimum, maximum) = intersect (this :?> Shape) ray minimum maximum
          interface IOccluder with
            member _.Occludes(ray, minimum, maximum) = occludesLocal shape ray minimum maximum inverse }

/// Instancing whose transform changes during the shutter: each ray is intersected against the pose at its
/// ShutterTime, which is what produces motion blur.
module MotionTransform =
    /// Rotating keys sweep arcs that can leave the keyed boxes, so bounds are also sampled between keys.
    let private boundsSubsteps = 8

    let transform (shape: Shape) (motion: AnimatedTransform) =
        if motion.IsStatic then Transform.transform shape (Transformation.ofAffine motion.Matrices.[0])
        else
            let bounds =
                lazy (
                    shape.Bounds |> Option.map (fun box ->
                        if box.IsEmpty then box
                        else
                            let times = motion.Times
                            let samples =
                                [| for i in 0 .. times.Length - 2 do
                                       for step in 0 .. boundsSubsteps - 1 do
                                           yield times.[i] + (times.[i + 1] - times.[i]) * float step / float boundsSubsteps
                                   yield times.[times.Length - 1] |]
                            let boxes =
                                samples |> Array.map (fun time ->
                                    let struct (matrix, _) = motion.At time
                                    Transform.transformBoundsBy matrix box)
                            let low = boxes |> Array.fold (fun (p: Point) b -> p.Lowest b.lowPoint) boxes.[0].lowPoint
                            let high = boxes |> Array.fold (fun (p: Point) b -> p.Highest b.highPoint) boxes.[0].highPoint
                            // Pad for the chord-versus-arc error between sub-samples.
                            let pad = 1e-3 * (high - low).Magnitude + 1e-9
                            BBox(low - Vector(pad, pad, pad), high + Vector(pad, pad, pad))))
            let intersect (owner: Shape) (ray: Ray) minimum maximum =
                let struct (_, inverse) = motion.At ray.ShutterTime
                Transform.intersectLocal shape owner ray minimum maximum inverse inverse.transpose
            let midpoint = 0.5 * (motion.Start + motion.End)
            { new Shape() with
                member this.hitFunction ray = intersect this ray 0. infinity
                member _.Bounds = bounds.Value
                member _.IsOpaque = shape.IsOpaque
                member _.getBoundingBox() =
                    bounds.Value |> Option.defaultWith (fun () -> invalidOp "An unbounded moving shape has no finite bounding box.")
                // Inside tests (CSG, media) have no ray time; use the pose at mid-shutter.
                member _.isInside p =
                    let struct (_, inverse) = motion.At midpoint
                    shape.isInside(transformPoint(p, inverse))
              interface IIntervalShape with
                member this.HitWithin(ray, minimum, maximum) = intersect (this :?> Shape) ray minimum maximum
              interface IOccluder with
                member _.Occludes(ray, minimum, maximum) =
                    let struct (_, inverse) = motion.At ray.ShutterTime
                    Transform.occludesLocal shape ray minimum maximum inverse }

type SolidCylinder(center: Point, radius: float, height: float, cylinder: Texture, top: Texture, bottom: Texture) =
    inherit Shape()
    let side = HollowCylinder(center, radius, height, cylinder)
    let cap texture angle y =
        Transform.transform (Disc(Point.Zero, radius, texture))
            (mergeTransformations [rotateX angle; translate center.X y center.Z])
    let topCap = cap top (-Math.PI/2.) (center.Y + height/2.)
    let bottomCap = cap bottom (Math.PI/2.) (center.Y - height/2.)
    member _.center = center
    member _.radius = radius
    member _.height = height
    member _.cylinder = cylinder
    member _.top = top
    member _.bottom = bottom
    member _.topDisc = topCap
    member _.bottomDisc = bottomCap
    member _.hollowCylinder = side
    member _.bBox = side.bBox
    override _.IsOpaque = side.IsOpaque && topCap.IsOpaque && bottomCap.IsOpaque
    override _.getBoundingBox() = side.bBox
    override _.isInside(p: Point) =
        let p = p - center
        Vector(p.X,0.,p.Z).Magnitude <= radius && abs p.Y <= height/2.
    member private this.Intersect(ray: Ray, minimum: float, maximum: float) =
        let mutable closest = Geometry.hitWithin topCap ray minimum maximum
        let consider (hit: HitPoint) =
            if hit.DidHit && (not closest.DidHit || hit.Time < closest.Time) then closest <- hit
        consider (Geometry.hitWithin bottomCap ray minimum maximum)
        consider (Geometry.hitWithin side ray minimum maximum)
        if closest.DidHit then closest.WithShape this else HitPoint(ray)
    override this.hitFunction ray = this.Intersect(ray, 0., infinity)
    interface IIntervalShape with
        member this.HitWithin(ray, minimum, maximum) = this.Intersect(ray, minimum, maximum)

type Box(low: Point, high: Point, front: Texture, back: Texture, top: Texture,
         bottom: Texture, left: Texture, right: Texture) =
    inherit Shape()
    do
        Geometry.finitePoint "low" low
        Geometry.finitePoint "high" high
        Geometry.positive "width" (high.X-low.X)
        Geometry.positive "height" (high.Y-low.Y)
        Geometry.positive "depth" (high.Z-low.Z)
    let boxWidth, boxHeight, boxDepth = high.X-low.X, high.Y-low.Y, high.Z-low.Z
    let exactBounds, bounds = BBox(low, high), Geometry.paddedBounds low high
    member _.low = low
    member _.high = high
    member _.front = front
    member _.back = back
    member _.top = top
    member _.bottom = bottom
    member _.left = left
    member _.right = right
    member _.width = boxWidth
    member _.height = boxHeight
    member _.depth = boxDepth
    member _.getMatFromTex tex u v = getFunc tex u v
    override _.IsOpaque = [front; back; top; bottom; left; right] |> List.forall Textures.isOpaque
    override _.isInside p = exactBounds.isInside p
    override _.getBoundingBox() = bounds
    member private this.Intersect(ray: Ray, minimum: float, maximum: float) =
        match exactBounds.intersectRG ray with
        | None -> HitPoint(ray)
        | Some(entry, exit, _, _, _, _, _, _) when exit <= minimum || entry >= maximum -> HitPoint(ray)
        | Some(entry, exit, tx, ty, tz, tx', ty', tz') ->
            let entering = entry > minimum
            let time = if entering then entry else exit
            let x, y, z = if entering then tx, ty, tz else tx', ty', tz'
            let axis =
                if (entering && x >= y && x >= z) || (not entering && x <= y && x <= z) then 0
                elif (entering && y >= z) || (not entering && y <= z) then 1 else 2
            let p, d = ray.PointAtTime time, ray.GetDirection
            let direction = if axis = 0 then d.X elif axis = 1 then d.Y else d.Z
            let positiveNormal = (direction < 0.) = entering
            let sign = if positiveNormal then 1. else -1.
            let normal, tex, u, v =
                match axis with
                | 0 -> Vector(sign,0.,0.), (if positiveNormal then right else left), (p.Y-low.Y)/boxHeight, (p.Z-low.Z)/boxDepth
                | 1 -> Vector(0.,sign,0.), (if positiveNormal then top else bottom), (p.X-low.X)/boxWidth, (p.Z-low.Z)/boxDepth
                | _ -> Vector(0.,0.,sign), (if positiveNormal then front else back), (p.X-low.X)/boxWidth, (p.Y-low.Y)/boxHeight
            if time < maximum then HitPoint(ray, time, normal, getFunc tex u v, this, u, v) else HitPoint(ray)
    override this.hitFunction ray = this.Intersect(ray, 0., infinity)
    interface IIntervalShape with
        member this.HitWithin(ray, minimum, maximum) = this.Intersect(ray, minimum, maximum)

type InfinitePlane(tex: Texture) =
    inherit Shape()
    member _.tex = tex
    override _.Bounds = None
    override _.IsOpaque = Textures.isOpaque tex
    override _.isInside _ = invalidOp "Cannot be inside a two-dimensional shape."
    override _.getBoundingBox() = invalidOp "An infinite plane does not have a finite bounding box."
    override this.hitFunction(ray: Ray) =
        if not ray.IsValid || ray.GetDirection.Z = 0. then HitPoint(ray)
        else
            let time = -ray.GetOrigin.Z / ray.GetDirection.Z
            if time > 0. && Double.IsFinite time then
                let p = ray.PointAtTime time
                HitPoint(ray, time, Vector(0.,0.,-1.), getFunc tex p.X p.Y, this, p.X, p.Y)
            else HitPoint(ray)
    interface IIntervalShape with
        member this.HitWithin(ray, minimum, maximum) = this.hitFunction ray |> Geometry.within minimum maximum
