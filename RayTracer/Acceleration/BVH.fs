namespace Tracer.Basics

module BVH =
    open System
    open System.Collections.Generic
    open AccelerationCommon

    let debugBuildCounts = false

    type BVHStructure =
        | Leaf of int list * BBox
        | Node of BVHStructure * BVHStructure * BBox * int

    let findAxisMinMaxValues (box: BBox) axis =
        match axis with
        | 0 -> box.lowPoint.X, box.highPoint.X
        | 1 -> box.lowPoint.Y, box.highPoint.Y
        | 2 -> box.lowPoint.Z, box.highPoint.Z
        | _ -> invalidArg "axis" "Axis must be 0, 1, or 2."

    let sortListByAxis (indices: int list) (boxes: BBox array) axis =
        indices |> List.sortBy (fun index -> findAxisMinMaxValues boxes.[index] axis |> fst)

    let findOuterBoundingBoxLowHighPoints (boxes: BBox array) =
        if boxes.Length = 0 then Point.Zero, Point.Zero
        else
            let bounds = boxes |> Array.fold (fun acc box -> union acc (ofBBox box)) emptyBounds
            Point(bounds.MinX, bounds.MinY, bounds.MinZ), Point(bounds.MaxX, bounds.MaxY, bounds.MaxZ)

    let findLargestBoundingBoxSideLengths (low: Point, high: Point) =
        let x, y, z = high.X - low.X, high.Y - low.Y, high.Z - low.Z
        if x >= y && x >= z then 0, x
        elif y >= z then 1, y
        else 2, z

    let getBoxArrFromIndexes (indices: int list) (boxes: BBox array) =
        indices |> List.map (fun index -> boxes.[index]) |> List.toArray

    let convertShapesToBBoxes shapes = cacheBounds shapes |> Array.map (fun bounds -> bounds.ToBBox())

    let internal buildBounded (boxes: Bounds array) (sourceIndices: int array) =
        let indices = Array.copy sourceIndices
        let rec build start count depth =
            if count = 0 then Leaf([], BBox(Point.Zero, Point.Zero))
            else
                let bounds = rangeBounds boxes indices start count
                let mutable axis = 0
                for candidate = 1 to 2 do
                    if bounds.HalfExtent candidate > bounds.HalfExtent axis then axis <- candidate
                if count <= 2 || depth >= 64 then
                    Leaf([for position = start to start + count - 1 do indices.[position]], bounds.ToBBox())
                else
                    let comparison =
                        Comparer<int>.Create(fun a b ->
                            let result = compare (boxes.[a].Centroid axis) (boxes.[b].Centroid axis)
                            if result = 0 then compare a b else result)
                    Array.Sort(indices, start, count, comparison)
                    let half = count / 2
                    Node(build start half (depth + 1), build (start + half) (count - half) (depth + 1), bounds.ToBBox(), axis)
        build 0 indices.Length 0

    let buildStructure (shapes: Shape array) =
        let boxes = cacheBounds shapes
        let finite, fallback = partitionBounds boxes
        let tree = buildBounded boxes finite
        if fallback.Length = 0 then tree
        else
            let leaf = Leaf(Array.toList fallback, unbounded.ToBBox())
            if finite.Length = 0 then leaf else Node(tree, leaf, unbounded.ToBBox(), 0)

    let build shapes = buildStructure shapes

    let order (direction: float) left right =
        if direction >= 0. then left, right else right, left

    let isLeaf = function Leaf _ -> true | _ -> false

    let getRayDirectionValue (ray: Ray) axis =
        match axis with
        | 0 -> ray.GetDirection.X
        | 1 -> ray.GetDirection.Y
        | 2 -> ray.GetDirection.Z
        | _ -> invalidArg "axis" "Axis must be 0, 1, or 2."

    let getBbox = function Node(_, _, box, _) | Leaf(_, box) -> box

    let closestHit structure ray shapes =
        match structure with
        | Leaf(indices, _) ->
            let result = indices |> List.fold (fun acc index -> consider shapes index ray 0. infinity acc) (noCandidate infinity)
            if result.Found then Some result.Hit else None
        | Node _ -> None

    let internal query tree ray data shapes minimum maximum initial stopAtFirst =
        let stack = Stack<struct (BVHStructure * float)>()
        let mutable result = initial
        let mutable stopped = false
        match intersect (ofBBox (getBbox tree)) data minimum result.Distance with
        | ValueSome(struct (entry, _)) when minimum < maximum -> stack.Push(struct (tree, entry))
        | _ -> ()
        while stack.Count > 0 && not stopped do
            let struct (node, entry) = stack.Pop()
            if entry <= result.Distance then
                match node with
                | Leaf(indices, _) ->
                    let mutable remaining = indices
                    while not stopped && not (List.isEmpty remaining) do
                        result <- consider shapes (List.head remaining) ray minimum maximum result
                        stopped <- stopAtFirst && result.Found
                        remaining <- List.tail remaining
                | Node(left, right, _, _) ->
                    match intersect (ofBBox (getBbox left)) data minimum result.Distance,
                          intersect (ofBBox (getBbox right)) data minimum result.Distance with
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

    let searchStructure tree ray shapes maximum =
        let data = validateQuery ray 0. maximum
        let result = query tree ray data shapes 0. maximum (noCandidate maximum) false
        if result.Found then Some result.Hit else None

    let traverse tree (ray: Ray) shapes =
        match searchStructure tree ray shapes infinity with
        | Some hit -> hit
        | None -> HitPoint ray
