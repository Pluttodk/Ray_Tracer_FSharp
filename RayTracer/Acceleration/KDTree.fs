namespace Tracer.Basics

open System
open System.Collections.Generic

module internal AccelerationCommon =
    [<Struct>]
    type Bounds =
        { MinX: float; MinY: float; MinZ: float
          MaxX: float; MaxY: float; MaxZ: float }
        member b.Minimum axis =
            match axis with 0 -> b.MinX | 1 -> b.MinY | _ -> b.MinZ
        member b.Maximum axis =
            match axis with 0 -> b.MaxX | 1 -> b.MaxY | _ -> b.MaxZ
        member b.Centroid axis = b.Minimum axis * 0.5 + b.Maximum axis * 0.5
        member b.HalfExtent axis = b.Maximum axis * 0.5 - b.Minimum axis * 0.5
        member b.IsEmpty = b.MinX > b.MaxX || b.MinY > b.MaxY || b.MinZ > b.MaxZ
        member b.IsFinite =
            Double.IsFinite b.MinX && Double.IsFinite b.MinY && Double.IsFinite b.MinZ
            && Double.IsFinite b.MaxX && Double.IsFinite b.MaxY && Double.IsFinite b.MaxZ
            && b.MinX <= b.MaxX && b.MinY <= b.MaxY && b.MinZ <= b.MaxZ
        member b.ToBBox() = BBox(Point(b.MinX, b.MinY, b.MinZ), Point(b.MaxX, b.MaxY, b.MaxZ))

    let emptyBounds =
        { MinX = infinity; MinY = infinity; MinZ = infinity
          MaxX = -infinity; MaxY = -infinity; MaxZ = -infinity }

    let unbounded =
        { MinX = -infinity; MinY = -infinity; MinZ = -infinity
          MaxX = infinity; MaxY = infinity; MaxZ = infinity }

    let ofBBox (box: BBox) =
        if isNull (box :> obj) then nullArg "box"
        let result =
            { MinX = box.lowPoint.X; MinY = box.lowPoint.Y; MinZ = box.lowPoint.Z
              MaxX = box.highPoint.X; MaxY = box.highPoint.Y; MaxZ = box.highPoint.Z }
        if Double.IsNaN result.MinX || Double.IsNaN result.MinY || Double.IsNaN result.MinZ
           || Double.IsNaN result.MaxX || Double.IsNaN result.MaxY || Double.IsNaN result.MaxZ then
            invalidArg "box" "Bounding-box coordinates must not be NaN."
        result

    let union a b =
        { MinX = min a.MinX b.MinX; MinY = min a.MinY b.MinY; MinZ = min a.MinZ b.MinZ
          MaxX = max a.MaxX b.MaxX; MaxY = max a.MaxY b.MaxY; MaxZ = max a.MaxZ b.MaxZ }

    let ofBoundedBBox argumentName box =
        let bounds = ofBBox box
        if not bounds.IsFinite && not bounds.IsEmpty then
            invalidArg argumentName "Bounds must be finite or empty; use Shape.Bounds=None for unbounded geometry."
        bounds

    let cacheBounds (shapes: Shape array) =
        shapes |> Array.map (fun shape ->
            if isNull (shape :> obj) then nullArg "shapes"
            match shape.Bounds with
            | None -> unbounded
            | Some box -> ofBoundedBBox "shapes" box)

    let partitionBounds (boxes: Bounds array) =
        let finite = ResizeArray<int>()
        let fallback = ResizeArray<int>()
        for index = 0 to boxes.Length - 1 do
            if not boxes.[index].IsEmpty then
                if boxes.[index].IsFinite then finite.Add index else fallback.Add index
        finite.ToArray(), fallback.ToArray()

    let rangeBounds (boxes: Bounds array) (indices: int array) start count =
        let mutable bounds = emptyBounds
        for offset = start to start + count - 1 do
            bounds <- union bounds boxes.[indices.[offset]]
        bounds

    [<Struct>]
    type RayData =
        { X: float; Y: float; Z: float
          DX: float; DY: float; DZ: float
          InvX: float; InvY: float; InvZ: float }
        member r.Origin axis = match axis with 0 -> r.X | 1 -> r.Y | _ -> r.Z
        member r.Direction axis = match axis with 0 -> r.DX | 1 -> r.DY | _ -> r.DZ
        member r.Inverse axis = match axis with 0 -> r.InvX | 1 -> r.InvY | _ -> r.InvZ

    let rayData (ray: Ray) =
        { X = ray.GetOrigin.X; Y = ray.GetOrigin.Y; Z = ray.GetOrigin.Z
          DX = ray.GetDirection.X; DY = ray.GetDirection.Y; DZ = ray.GetDirection.Z
          InvX = 1. / ray.GetDirection.X; InvY = 1. / ray.GetDirection.Y; InvZ = 1. / ray.GetDirection.Z }

    let validateQuery (ray: Ray) minimum maximum =
        if Double.IsNaN minimum || Double.IsNaN maximum then
            invalidArg "minimum" "Ray bounds must not be NaN."
        let r = rayData ray
        if not (Double.IsFinite r.X && Double.IsFinite r.Y && Double.IsFinite r.Z
                && Double.IsFinite r.DX && Double.IsFinite r.DY && Double.IsFinite r.DZ)
           || (r.DX = 0. && r.DY = 0. && r.DZ = 0.) then
            invalidArg "ray" "A ray needs a finite origin and a finite, nonzero direction."
        r

    let boundaryTime bound origin direction =
        let difference = bound - origin
        if Double.IsInfinity difference && Double.IsFinite bound && Double.IsFinite origin then
            bound / direction - origin / direction
        else difference / direction

    // Packed nodes are already validated at build time. Parallel/subnormal directions avoid 0 * infinity.
    let intersectFinite (bounds: Bounds) (ray: RayData) minimum maximum =
        let mutable near = minimum
        let mutable far = maximum
        let mutable axis = 0
        let mutable overlaps = near <= far
        while overlaps && axis < 3 do
            let origin, direction = ray.Origin axis, ray.Direction axis
            let low, high = bounds.Minimum axis, bounds.Maximum axis
            if direction = 0. then
                overlaps <- origin >= low && origin <= high
            else
                let inverse = ray.Inverse axis
                let lowDelta, highDelta = low - origin, high - origin
                let a =
                    if Double.IsFinite inverse && Double.IsFinite lowDelta then lowDelta * inverse
                    else boundaryTime low origin direction
                let b =
                    if Double.IsFinite inverse && Double.IsFinite highDelta then highDelta * inverse
                    else boundaryTime high origin direction
                let first, last = min a b, max a b
                let first = if Double.IsFinite first then Math.BitDecrement(first - abs first * 6.661338147750943e-16) else first
                let last = if Double.IsFinite last then Math.BitIncrement(last + abs last * 6.661338147750943e-16) else last
                near <- max near first
                far <- min far last
                overlaps <- near <= far
            axis <- axis + 1
        if overlaps then ValueSome(struct (near, far)) else ValueNone

    /// One axis of `intersectFinite`, written out so the hot loop has no axis dispatch: same arithmetic,
    /// same widening, same answers.
    let inline private slab (low: float) (high: float) (origin: float) (direction: float) (inverse: float)
                            (near: byref<float>) (far: byref<float>) =
        if direction = 0. then origin >= low && origin <= high
        else
            let lowDelta, highDelta = low - origin, high - origin
            let a =
                if Double.IsFinite inverse && Double.IsFinite lowDelta then lowDelta * inverse
                else boundaryTime low origin direction
            let b =
                if Double.IsFinite inverse && Double.IsFinite highDelta then highDelta * inverse
                else boundaryTime high origin direction
            let first, last = min a b, max a b
            let first = if Double.IsFinite first then Math.BitDecrement(first - abs first * 6.661338147750943e-16) else first
            let last = if Double.IsFinite last then Math.BitIncrement(last + abs last * 6.661338147750943e-16) else last
            near <- max near first
            far <- min far last
            near <= far

    /// `intersectFinite` on a box given by its corners, returning only the entry distance, NaN for a miss.
    let intersectBox minX minY minZ maxX maxY maxZ (ray: RayData) minimum maximum =
        let mutable near = minimum
        let mutable far = maximum
        if near <= far
           && slab minX maxX ray.X ray.DX ray.InvX &near &far
           && slab minY maxY ray.Y ray.DY ray.InvY &near &far
           && slab minZ maxZ ray.Z ray.DZ ray.InvZ &near &far then near
        else nan

    /// `intersectFinite` returning only the entry distance, NaN for a miss.
    let intersectNear (bounds: Bounds) (ray: RayData) minimum maximum =
        intersectBox bounds.MinX bounds.MinY bounds.MinZ bounds.MaxX bounds.MaxY bounds.MaxZ ray minimum maximum

    let intersect (bounds: Bounds) ray minimum maximum =
        if bounds.IsEmpty then ValueNone
        elif bounds.IsFinite then intersectFinite bounds ray minimum maximum
        else ValueSome(struct (minimum, maximum))

    [<Struct; NoEquality; NoComparison>]
    type Candidate =
        { Distance: float
          Index: int
          Hit: HitPoint }
        member c.Found = c.Index <> Int32.MaxValue

    let noCandidate maximum =
        { Distance = maximum; Index = Int32.MaxValue; Hit = Unchecked.defaultof<HitPoint> }

    let consider (shapes: Shape array) index (ray: Ray) minimum maximum candidate =
        // Keep exact-distance ties visible even after another primitive tightens the upper bound.
        let primitiveMaximum = min maximum (Math.BitIncrement candidate.Distance)
        let hit = Geometry.hitWithin shapes.[index] ray minimum primitiveMaximum
        if hit.DidHit && Double.IsFinite hit.Time && hit.Time > minimum && hit.Time < maximum
           && (hit.Time < candidate.Distance || (hit.Time = candidate.Distance && index < candidate.Index)) then
            { Distance = hit.Time; Index = index; Hit = hit }
        else candidate

    /// Per-slot fast paths, null where a shape offers none; empty when no shape does.
    let fastPaths<'T when 'T: null> (shapes: Shape array) =
        let paths = shapes |> Array.map (fun shape -> match shape :> obj with :? 'T as path -> path | _ -> null)
        if paths |> Array.forall isNull then [||] else paths

    /// `consider` for a primitive with an allocation-free hit time. The candidate it records carries no
    /// HitPoint; `materialize` builds the winner's afterwards. Same acceptance test as `consider`.
    let considerTime (fast: IHitTime) index (ray: Ray) minimum maximum candidate =
        let primitiveMaximum = min maximum (Math.BitIncrement candidate.Distance)
        let time = if minimum < primitiveMaximum then fast.HitTime(ray, minimum, primitiveMaximum) else nan
        if time > minimum && time < maximum && Double.IsFinite time
           && (time < candidate.Distance || (time = candidate.Distance && index < candidate.Index)) then
            { Distance = time; Index = index; Hit = Unchecked.defaultof<HitPoint> }
        else candidate

    /// `consider` for an any-hit query: an occluder answers without shading. The distance it records is
    /// a placeholder, since an any-hit query stops at the first candidate.
    let considerOccluder (occluder: IOccluder) index (ray: Ray) minimum maximum candidate =
        let primitiveMaximum = min maximum (Math.BitIncrement candidate.Distance)
        if minimum < primitiveMaximum && occluder.Occludes(ray, minimum, primitiveMaximum) then
            { Distance = minimum; Index = index; Hit = Unchecked.defaultof<HitPoint> }
        else candidate

    /// The candidate's HitPoint, rebuilt through the shape's own intersection when a fast path skipped it.
    let materialize (shapes: Shape array) (ray: Ray) minimum maximum (candidate: Candidate) =
        if not candidate.Found then HitPoint ray
        elif isNull (box candidate.Hit) then Geometry.hitWithin shapes.[candidate.Index] ray minimum maximum
        else candidate.Hit

    let finish (ray: Ray) (candidate: Candidate) = if candidate.Found then candidate.Hit else HitPoint ray

module KD_tree =
    open AccelerationCommon

    type ShapeBBox(box: BBox, shape: int) =
        member _.box = box
        member _.shape = shape

    type KDTree =
        | Node of int * float * BBox * KDTree * KDTree
        | Leaf of BBox * ShapeBBox list

    let findMaxMin (boxes: ShapeBBox list) =
        if List.isEmpty boxes then Point.Zero, Point.Zero
        else
            let bounds = boxes |> List.fold (fun acc item -> union acc (ofBBox item.box)) emptyBounds
            Point(bounds.MaxX, bounds.MaxY, bounds.MaxZ), Point(bounds.MinX, bounds.MinY, bounds.MinZ)

    let partitionAfterSelect (boxes: ShapeBBox list) splitX splitY splitZ =
        let split axis value =
            List.filter (fun (item: ShapeBBox) -> (ofBBox item.box).Minimum axis <= value) boxes,
            List.filter (fun (item: ShapeBBox) -> (ofBBox item.box).Maximum axis >= value) boxes
        let leftX, rightX = split 0 splitX
        let leftY, rightY = split 1 splitY
        let leftZ, rightZ = split 2 splitZ
        leftX, rightX, leftY, rightY, leftZ, rightZ

    let findSplitValues (boxes: ShapeBBox list) =
        if List.isEmpty boxes then 0., 0., 0., 0., 0., 0., 0., 0., 0.
        else
            let high, low = findMaxMin boxes
            let n = float boxes.Length
            let x, y, z =
                boxes |> List.fold (fun (x, y, z) item ->
                    x + item.box.lowPoint.X / n, y + item.box.lowPoint.Y / n, z + item.box.lowPoint.Z / n) (0., 0., 0.)
            x, y, z, low.X, high.X, low.Y, high.Y, low.Z, high.Z

    let findPlane fx sx fy sy fz sz (high: Point) (low: Point) runX runY runZ =
        [| 0, high.X - low.X, runX && fx < 1.6 && sx < 1.6
           1, high.Y - low.Y, runY && fy < 1.6 && sy < 1.6
           2, high.Z - low.Z, runZ && fz < 1.6 && sz < 1.6 |]
        |> Array.filter (fun (_, _, usable) -> usable)
        |> Array.sortByDescending (fun (_, extent, _) -> extent)
        |> Array.tryHead
        |> Option.map (fun (axis, _, _) -> axis)
        |> Option.defaultValue 3

    let createKDTreeFromList currentDepth (hi: Point) (lo: Point) (boxes: ShapeBBox list) =
        let mutable budget = max 64L (int64 boxes.Length * 32L)
        let rec build depth (region: Bounds) (items: ShapeBBox array) =
            let leaf () = Leaf(region.ToBBox(), Array.toList items)
            if items.Length <= 8 || depth >= 48 || not region.IsFinite then leaf ()
            else
                let mutable axis = 0
                for candidate = 1 to 2 do
                    if region.HalfExtent candidate > region.HalfExtent axis then axis <- candidate
                let plane = region.Centroid axis
                if plane <= region.Minimum axis || plane >= region.Maximum axis then leaf ()
                else
                    let left = items |> Array.filter (fun item -> (ofBBox item.box).Minimum axis <= plane)
                    let right = items |> Array.filter (fun item -> (ofBBox item.box).Maximum axis >= plane)
                    let cost = int64 left.Length + int64 right.Length
                    if left.Length = 0 || right.Length = 0
                       || (left.Length = items.Length && right.Length = items.Length) || cost > budget then leaf ()
                    else
                        budget <- budget - cost
                        let leftBounds, rightBounds =
                            match axis with
                            | 0 -> { region with MaxX = plane }, { region with MinX = plane }
                            | 1 -> { region with MaxY = plane }, { region with MinY = plane }
                            | _ -> { region with MaxZ = plane }, { region with MinZ = plane }
                        Node(axis, plane, region.ToBBox(), build (depth + 1) leftBounds left, build (depth + 1) rightBounds right)
        build currentDepth (ofBBox (BBox(lo, hi))) (List.toArray boxes)

    let internal buildBounded (boxes: Bounds array) (indices: int array) =
        if indices.Length = 0 then Leaf(BBox(Point.Zero, Point.Zero), [])
        else
            let region = rangeBounds boxes indices 0 indices.Length
            let items = indices |> Array.map (fun index -> ShapeBBox(boxes.[index].ToBBox(), index)) |> Array.toList
            createKDTreeFromList 0 (Point(region.MaxX, region.MaxY, region.MaxZ)) (Point(region.MinX, region.MinY, region.MinZ)) items

    let buildKDTree (shapes: Shape array) =
        let boxes = cacheBounds shapes
        let finite, fallback = partitionBounds boxes
        let tree = buildBounded boxes finite
        if fallback.Length = 0 then tree
        else
            let leaf = Leaf(unbounded.ToBBox(), fallback |> Array.map (fun index -> ShapeBBox(unbounded.ToBBox(), index)) |> Array.toList)
            if finite.Length = 0 then leaf else Node(0, 0., unbounded.ToBBox(), tree, leaf)

    let findRayDirectionFromA axis (ray: Ray) =
        if axis < 0 || axis > 2 then invalidArg "axis" "Axis must be 0, 1, or 2."
        (rayData ray).Direction axis

    let findRayOriginFromA axis (ray: Ray) =
        if axis < 0 || axis > 2 then invalidArg "axis" "Axis must be 0, 1, or 2."
        (rayData ray).Origin axis

    let order (direction: float, left: KDTree, right: KDTree) =
        if direction >= 0. then left, right else right, left

    let private bounds = function Node(_, _, box, _, _) | Leaf(box, _) -> ofBBox box

    let internal query tree ray data shapes minimum maximum initial stopAtFirst =
        let stack = Stack<struct (KDTree * float)>()
        let mutable result = initial
        let mutable stopped = false
        let push node =
            match intersect (bounds node) data minimum result.Distance with
            | ValueSome(struct (entry, _)) -> stack.Push(struct (node, entry))
            | ValueNone -> ()
        if minimum < maximum then push tree
        while stack.Count > 0 && not stopped do
            let struct (node, entry) = stack.Pop()
            if entry <= result.Distance then
                match node with
                | Leaf(_, items) ->
                    let mutable remaining = items
                    while not stopped && not (List.isEmpty remaining) do
                        let item = List.head remaining
                        result <- consider shapes item.shape ray minimum maximum result
                        stopped <- stopAtFirst && result.Found
                        remaining <- List.tail remaining
                | Node(_, _, _, left, right) ->
                    match intersect (bounds left) data minimum result.Distance,
                          intersect (bounds right) data minimum result.Distance with
                    | ValueSome(struct (a, _)), ValueSome(struct (b, _)) ->
                        if a <= b then
                            stack.Push(struct (right, b))
                            stack.Push(struct (left, a))
                        else
                            stack.Push(struct (left, a))
                            stack.Push(struct (right, b))
                    | ValueSome(struct (a, _)), ValueNone -> stack.Push(struct (left, a))
                    | ValueNone, ValueSome(struct (b, _)) -> stack.Push(struct (right, b))
                    | _ -> ()
        result

    let closestHit (shapeBoxes: ShapeBBox list) ray shapes =
        let result = shapeBoxes |> List.fold (fun acc item -> consider shapes item.shape ray 0. infinity acc) (noCandidate infinity)
        if result.Found then Some result.Hit else None

    let searchKDTree tree ray minimum maximum shapes =
        let data = validateQuery ray minimum maximum
        query tree ray data shapes minimum maximum (noCandidate maximum) false |> finish ray

    let traverseKDTree tree ray shapes = searchKDTree tree ray 0. infinity shapes
