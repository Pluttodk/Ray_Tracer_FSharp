namespace Tracer.Basics

module Acceleration =
    open System
    open AccelerationCommon

    type Acceleration =
        | KDTree
        | BVH
        | RegularGrid
        | FlatBVH
        | BruteForce

    type internal Structure =
        | Kd of KD_tree.KDTree
        | MedianBvh of BVH.BVHStructure
        | Grid of RegularGrids.RGStructure
        | Flat of Tracer.Basics.FlatBVH.FlatBVHStructure
        | Linear

    /// Immutable acceleration over a snapshot of shape references. Geometry itself must remain stable.
    [<Sealed>]
    type IAcceleration internal (kind: Acceleration, shapes: Shape array, structure: Structure, fallback: int array, empty: int array) =
        member internal _.Shapes = shapes
        member internal _.Structure = structure
        member internal _.Fallback = fallback
        member internal _.Empty = empty
        member _.Kind = kind
        member _.IsEmpty = shapes.Length = empty.Length
        member _.PrimitiveCount = shapes.Length
        member _.ActivePrimitiveCount = shapes.Length - empty.Length
        member _.EmptyPrimitiveCount = empty.Length
        member _.UnboundedPrimitiveCount = if kind = BruteForce then 0 else fallback.Length
        member _.FlatBVH =
            match structure with Flat tree -> Some tree | _ -> None
        member internal _.MatchesShapes(other: Shape array) =
            shapes.Length = other.Length && Array.forall2 (fun a b -> Object.ReferenceEquals(a, b)) shapes other

    let private build kind flatOptions (source: Shape array) =
        if isNull source then nullArg "shapes"
        let shapes = Array.copy source
        if kind = BruteForce then
            if shapes |> Array.exists (fun shape -> isNull (shape :> obj)) then nullArg "shapes"
            IAcceleration(kind, shapes, Linear, [| 0 .. shapes.Length - 1 |], [||])
        else
            let boxes = cacheBounds shapes
            let finite, fallback = partitionBounds boxes
            let empty = [| for index = 0 to boxes.Length - 1 do if boxes.[index].IsEmpty then yield index |]
            let structure =
                match kind with
                | KDTree -> Kd(KD_tree.buildBounded boxes finite)
                | BVH -> MedianBvh(BVH.buildBounded boxes finite)
                | RegularGrid -> Grid(RegularGrids.buildBounded boxes finite)
                | FlatBVH -> Flat(Tracer.Basics.FlatBVH.buildBounded flatOptions boxes finite)
                | BruteForce -> Linear
            IAcceleration(kind, shapes, structure, fallback, empty)

    /// Builds per-scene state without reading or modifying the legacy acceleration registry.
    /// Bounds=None is unbounded; Some BBox.Empty is an empty source slot. BruteForce bypasses bounds entirely.
    let buildWith kind shapes = build kind Tracer.Basics.FlatBVH.defaultOptions shapes

    let buildFlatWithOptions options shapes = build FlatBVH options shapes

    /// Exports detached packed nodes and original shape indices, including explicit empty/unbounded slots.
    /// Never rebuilds another selected accelerator.
    let tryExportFlat (accel: IAcceleration) =
        match accel.Structure with
        | Flat tree -> Some(Tracer.Basics.FlatBVH.export tree accel.Fallback accel.Empty)
        | _ -> None

    let private query (accel: IAcceleration) ray minimum maximum stopAtFirst =
        let data = validateQuery ray minimum maximum
        let mutable result = noCandidate maximum
        if minimum < maximum then
            let mutable position = 0
            while position < accel.Fallback.Length && not (stopAtFirst && result.Found) do
                result <- consider accel.Shapes accel.Fallback.[position] ray minimum maximum result
                position <- position + 1
            if not (stopAtFirst && result.Found) then
                result <-
                    match accel.Structure with
                    | Kd tree -> KD_tree.query tree ray data accel.Shapes minimum maximum result stopAtFirst
                    | MedianBvh tree -> BVH.query tree ray data accel.Shapes minimum maximum result stopAtFirst
                    | Grid grid -> RegularGrids.query grid ray data accel.Shapes minimum maximum result stopAtFirst
                    | Flat tree -> Tracer.Basics.FlatBVH.query tree ray data accel.Shapes minimum maximum result stopAtFirst
                    | Linear -> result
        result

    /// Returns the closest primitive hit strictly inside (tMin,tMax); original input order wins exact ties.
    /// Uses geometry's IIntervalShape contract without reparameterizing native primitive hits.
    let traverseClosest accel ray tMin tMax = query accel ray tMin tMax false |> finish ray

    /// Opaque visibility only: every intersected primitive is a blocker, irrespective of its material.
    let anyHit accel ray tMin tMax = (query accel ray tMin tMax true).Found

    // These symbols remain for old mesh/render callers; buildWith never consults them.
    let mutable acceleration = "FlatBVH"

    type shapeArray(number: int, shapes: Shape array, acceleration: IAcceleration option) =
        member _.number = number
        member _.shapes = shapes
        member _.acceleration = acceleration

    let mutable listOfAccel: shapeArray list = []
    let private legacyGate = obj()

    let private selectedAcceleration () =
        match acceleration with
        | "KDTree" -> KDTree
        | "BVH" -> BVH
        | "RG" -> RegularGrid
        | "FlatBVH" -> FlatBVH
        | "BruteForce" -> BruteForce
        | _ -> invalidOp "Unknown legacy acceleration selection."

    let createAcceleration (shape: shapeArray) =
        lock legacyGate (fun () ->
            let kind = selectedAcceleration ()
            let matches (candidate: IAcceleration) = candidate.Kind = kind && candidate.MatchesShapes shape.shapes
            match shape.acceleration with
            | Some candidate when matches candidate -> candidate
            | _ ->
                let cached =
                    listOfAccel
                    |> List.tryPick (fun entry ->
                        if entry.number = shape.number then entry.acceleration |> Option.filter matches
                        else None)
                match cached with
                | Some candidate -> candidate
                | None ->
                    let candidate = buildWith kind shape.shapes
                    listOfAccel <- shapeArray(shape.number, shape.shapes, Some candidate) :: listOfAccel
                    candidate)

    /// The shape argument is retained for source compatibility; the prepared snapshot owns its shapes.
    let traverseIAcceleration accel ray (_shapes: Shape array) = traverseClosest accel ray 0. infinity

    let setAcceleration kind =
        lock legacyGate (fun () ->
            acceleration <-
                match kind with
                | KDTree -> "KDTree"
                | BVH -> "BVH"
                | RegularGrid -> "RG"
                | FlatBVH -> "FlatBVH"
                | BruteForce -> "BruteForce")
