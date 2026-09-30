module AccelerationRegressionTests

open System
open System.Runtime.InteropServices
open System.Threading.Tasks
open Assert
open Tracer.Basics
open Tracer.Basics.Acceleration

type private Probe(bounds: unit -> BBox, intersection: Ray -> float option,
                   ?onHit: unit -> unit, ?interval: (Ray -> float -> float -> float option),
                   ?hasBounds: bool) as this =
    inherit Shape()
    override _.isInside _ = false
    override _.getBoundingBox() = bounds ()
    override _.Bounds = if defaultArg hasBounds true then Some(bounds ()) else None
    member private _.Intersect(ray: Ray, minimum, maximum) =
        onHit |> Option.iter (fun action -> action ())
        let time =
            match interval with
            | Some native -> native ray minimum maximum
            | None -> intersection ray
        match time with
        | Some time when time > minimum && time < maximum -> HitPoint(ray, time, Vector(0., 1., 0.), Material.None, this)
        | _ -> HitPoint ray
    override this.hitFunction ray = this.Intersect(ray, 0., infinity)
    interface IIntervalShape with
        member this.HitWithin(ray, minimum, maximum) = this.Intersect(ray, minimum, maximum)

let private sphereWithBounds (center: Point) radius bounds =
    let intersect (ray: Ray) minimum maximum =
        let offset = ray.GetOrigin - center
        let direction = ray.GetDirection
        let a = direction * direction
        let b = offset * direction
        let c = offset * offset - radius * radius
        let discriminant = b * b - a * c
        if discriminant < 0. then None
        else
            let root = sqrt discriminant
            let first, second = (-b - root) / a, (-b + root) / a
            if first > minimum && first < maximum then Some first
            elif second > minimum && second < maximum then Some second
            else None
    Probe(bounds, (fun ray -> intersect ray 0. infinity), interval = intersect) :> Shape

let private unboundedSphere (center: Point) radius =
    let finite = SphereShape(center, radius, Textures.mkMatTexture Material.None)
    let hit (ray: Ray) minimum maximum =
        let result = (finite :> IIntervalShape).HitWithin(ray, minimum, maximum)
        if result.DidHit then Some result.Time else None
    Probe((fun () -> invalidOp "An unbounded shape's bounding-box getter must not be called."),
          (fun ray -> hit ray 0. infinity), interval = hit, hasBounds = false) :> Shape

let private sphere (center: Point) radius =
    sphereWithBounds center radius (fun () ->
        BBox(Point(center.X - radius, center.Y - radius, center.Z - radius),
             Point(center.X + radius, center.Y + radius, center.Z + radius)))

let private pointShape x =
    Probe((fun () -> BBox(Point(x, 0., 0.), Point(x, 0., 0.))), fun ray ->
        if ray.GetDirection.X = 0. then None
        else
            let time = (x - ray.GetOrigin.X) / ray.GetDirection.X
            let point = ray.PointAtTime time
            if time > 0. && point.Y = 0. && point.Z = 0. then Some time else None) :> Shape

let private sheet x y =
    Probe((fun () -> BBox(Point(x - 0.5, y - 0.5, 5.), Point(x + 0.5, y + 0.5, 5.))), fun ray ->
        if ray.GetDirection.Z = 0. then None
        else
            let time = (5. - ray.GetOrigin.Z) / ray.GetDirection.Z
            let point = ray.PointAtTime time
            if time > 0. && abs (point.X - x) <= 0.5 && abs (point.Y - y) <= 0.5 then Some time else None) :> Shape

let private planeTime coordinate origin direction =
    if direction = 0. then None
    else
        let difference = coordinate - origin
        let time = if Double.IsInfinity difference then coordinate / direction - origin / direction else difference / direction
        if time > 0. then Some time else None

let private brute (shapes: Shape array) (ray: Ray) minimum maximum =
    let mutable result = HitPoint ray
    let mutable limit = maximum
    for shape in shapes do
        let hit =
            match (shape :> obj) with
            | :? IIntervalShape as interval -> interval.HitWithin(ray, minimum, limit)
            | _ -> shape.hitFunction ray
        if hit.DidHit && Double.IsFinite hit.Time && hit.Time > minimum && hit.Time < limit then
            result <- hit
            limit <- hit.Time
    result

let private sameHit (expected: HitPoint) (actual: HitPoint) =
    expected.DidHit = actual.DidHit
    && (not expected.DidHit
        || (abs (expected.Time - actual.Time) <= 1e-12 * max 1. (abs expected.Time)
            && Object.ReferenceEquals(expected.Shape, actual.Shape)))

let private algorithms = [| KDTree; BVH; RegularGrid; FlatBVH; BruteForce |]

let private compareScene name shapes rays intervals =
    for algorithm in algorithms do
        let acceleration = buildWith algorithm shapes
        let mutable closestCorrect = true
        let mutable anyCorrect = true
        for ray in rays do
            for minimum, maximum in intervals do
                let expected = brute shapes ray minimum maximum
                closestCorrect <- closestCorrect && sameHit expected (traverseClosest acceleration ray minimum maximum)
                anyCorrect <- anyCorrect && (expected.DidHit = anyHit acceleration ray minimum maximum)
        Assert.True(closestCorrect, sprintf "%s-%A-closest-distance-and-identity" name algorithm)
        Assert.True(anyCorrect, sprintf "%s-%A-bounded-any-hit" name algorithm)

let closestAgainstBruteForce () =
    let random = Random 1729
    let coordinate extent = (random.NextDouble() * 2. - 1.) * extent
    let centers = Array.init 96 (fun _ -> Point(coordinate 12., coordinate 12., coordinate 12.))
    let shapes = centers |> Array.map (fun center -> sphere center (0.2 + random.NextDouble()))
    let rays =
        Array.init 384 (fun index ->
            let origin = Point(coordinate 20., coordinate 20., coordinate 20.)
            let direction =
                if index % 2 = 0 then centers.[index % centers.Length] - origin
                else Vector(coordinate 1., coordinate 1., coordinate 1.)
            Ray(origin, direction.Normalise))
    compareScene "random-normalized-rays" shapes rays [| 0., infinity; 0., 7.; 3., 24.; 10., 10.; 20., 2. |]
    let actual = Array.zeroCreate<HitPoint> rays.Length
    let tree = buildWith FlatBVH shapes
    Parallel.For(0, rays.Length, fun index -> actual.[index] <- traverseClosest tree rays.[index] 0. infinity) |> ignore
    Assert.True(Array.forall2 (fun ray hit -> sameHit (brute shapes ray 0. infinity) hit) rays actual, "flat-bvh-parallel-query-isolation")

let degenerateScenes () =
    let forward = Ray(Point(0., 0., -3.), Vector(0., 0., 1.))
    compareScene "empty" [||] [| forward |] [| 0., infinity; 0., 10. |]
    for algorithm in algorithms do Assert.True((buildWith algorithm [||]).IsEmpty, sprintf "%A-empty-state" algorithm)

    let coincident = Array.init 257 (fun index ->
        Probe((fun () -> BBox(Point(-1., -1., 4.), Point(1., 1., 6.))),
              (fun ray -> if index = 256 then planeTime 5. ray.GetOrigin.Z ray.GetDirection.Z else None)) :> Shape)
    let ray = Ray(Point.Zero, Vector(0., 0., 1.))
    compareScene "degenerate-centroids-last-primitive" coincident [| ray |] [| 0., infinity; 0., 4.; 5., 10. |]
    let packed = (buildWith FlatBVH coincident).FlatBVH.Value
    Assert.Equal(257, packed.PrimitiveCount, "flat-degenerate-leaf-retains-all-indices")
    Assert.Equal(1, packed.LeafCount, "flat-degenerate-centroids-terminate")

    let line = Array.init 32 (fun index -> pointShape (float index))
    compareScene "zero-area-line-grid" line
        [| Ray(Point(-1., 0., 0.), Vector(1., 0., 0.)); Ray(Point(33., 0., 0.), Vector(-1., 0., 0.)) |]
        [| 0., infinity; 3., 12.; 0., 0.5 |]
    let plane = [| for x = -3 to 3 do for y = -3 to 3 do sheet (float x) (float y) |]
    compareScene "zero-thickness-grid-and-boundaries" plane
        [| forward
           Ray(Point(-5., 5., 0.), Vector(1., -1., 1.).Normalise)
           Ray(Point(0.5, 0.5, 8.), Vector(-0., 0., -1.))
           Ray(Point(3.5, 3.5, 8.), Vector(0., -0., -1.))
           Ray(Point(9., 9., 8.), Vector(0., 0., -1.)) |]
        [| 0., infinity; 0., 2.; 0., 20. |]

    let center = Point(0., 0., 3.)
    let finite = sphere center 1.
    let unbounded = unboundedSphere center 1.
    let other = unboundedSphere (Point(0., 0., 8.)) 1.
    compareScene "explicit-unbounded-bounds" [| finite; unbounded; other |] [| ray; forward |] [| 0., infinity; 0., 2.; 2., 10. |]
    compareScene "unbounded-input-order-tie" [| unbounded; finite |] [| ray |] [| 0., infinity |]
    Assert.Equal(2, (buildWith FlatBVH [| finite; unbounded; other |]).UnboundedPrimitiveCount, "unbounded-primitives-retained-without-calling-bounding-box-getters")
    compareScene "empty-bounds" [| Shape.None; finite; Shape.None |] [| ray; forward |] [| 0., infinity; 2., 10. |]
    for algorithm in [| KDTree; BVH; RegularGrid; FlatBVH |] do
        let empty = buildWith algorithm [| Shape.None; Shape.None |]
        Assert.True(empty.IsEmpty && empty.EmptyPrimitiveCount = 2 && empty.ActivePrimitiveCount = 0, sprintf "%A-explicit-empty-bounds-have-no-active-primitives" algorithm)

let boundedAndLegacyTraversal () =
    let calls = ref 0
    let box () = BBox(Point(-1., -1., -1.), Point(10., 10., 10.))
    let shapes =
        [| Probe(box, (fun ray -> planeTime 4. ray.GetOrigin.Y ray.GetDirection.Y), onHit = (fun () -> calls.Value <- calls.Value + 1)) :> Shape
           Probe(box, (fun ray -> planeTime 1.5 ray.GetOrigin.Y ray.GetDirection.Y), onHit = (fun () -> calls.Value <- calls.Value + 1)) :> Shape |]
    let ray = Ray(Point.Zero, Vector(sqrt 0.5, 0.5, 0.5))
    Assert.True(BVH.getRayDirectionValue ray 0 > 0. && BVH.getRayDirectionValue ray 0 < 1., "normalized-direction-not-truncated")
    let manual = BVH.Node(BVH.Leaf([0], box ()), BVH.Leaf([1], box ()), box (), 0)
    let result = BVH.traverse manual ray shapes
    Assert.True(Object.ReferenceEquals(shapes.[1], result.Shape) && result.Time = 3., "bvh-searches-only-sibling-after-first-hit")
    Assert.Equal(2, calls.Value, "bvh-never-revisits-current-parent")

    for algorithm in algorithms do
        let tree = buildWith algorithm shapes
        Assert.True(not (anyHit tree ray 0. 3.), sprintf "%A-open-upper-bound" algorithm)
        Assert.True(not (anyHit tree ray 8. infinity), sprintf "%A-open-lower-bound" algorithm)
        Assert.True(Object.ReferenceEquals(shapes.[0], (traverseClosest tree ray 3. 9.).Shape), sprintf "%A-lower-bound-does-not-hide-other-primitives" algorithm)
        let tight = traverseClosest tree ray (Math.BitDecrement 3.) (Math.BitIncrement 3.)
        Assert.True(Object.ReferenceEquals(shapes.[1], tight.Shape), sprintf "%A-one-ulp-open-interval" algorithm)

    calls.Value <- 0
    let anyTree = buildWith FlatBVH shapes
    Assert.True(anyHit anyTree ray 0. 10., "flat-opaque-any-hit-finds-blocker")
    Assert.Equal(1, calls.Value, "flat-opaque-any-hit-stops-at-first-blocker")

    let distance = sphere (Point(0., 0., 4.)) 1.
    let standaloneShapes = [| distance |]
    let zRay = Ray(Point.Zero, Vector(0., 0., 1.))
    let expected = brute standaloneShapes zRay 0. infinity
    Assert.True(sameHit expected (KD_tree.traverseKDTree (KD_tree.buildKDTree standaloneShapes) zRay standaloneShapes), "legacy-kdtree-closest-oracle")
    Assert.True(sameHit expected (RegularGrids.traverse (RegularGrids.build standaloneShapes) zRay standaloneShapes), "legacy-grid-closest-oracle")
    Assert.True(not (BVH.traverse (BVH.build [||]) zRay [||]).DidHit, "legacy-bvh-empty")
    Assert.True(not (KD_tree.traverseKDTree (KD_tree.buildKDTree [||]) zRay [||]).DidHit, "legacy-kdtree-empty")
    Assert.True(not (RegularGrids.traverse (RegularGrids.build [||]) zRay [||]).DidHit, "legacy-grid-empty")

let depthAndOwnership () =
    let points = Array.init 400 (fun index -> pointShape (Math.Pow(2., float index)))
    let tree = buildFlatWithOptions { FlatBVH.defaultOptions with LeafSize = 1; MaxDepth = 128 } points
    Assert.True(tree.FlatBVH.Value.MaxDepth > 64, "flat-stress-scene-exceeds-initial-traversal-stack")
    let ray = Ray(Point.Zero, Vector(1., 0., 0.))
    Assert.True(sameHit (brute points ray 0. infinity) (traverseClosest tree ray 0. infinity), "flat-traversal-stack-growth-keeps-pending-nodes")
    Assert.True(anyHit tree ray 0. infinity, "flat-deep-stack-any-hit")
    let shallow = buildFlatWithOptions { FlatBVH.defaultOptions with LeafSize = 1; MaxDepth = 1 } points
    Assert.True(shallow.FlatBVH.Value.MaxDepth <= 1, "flat-build-depth-budget-is-bounded")
    Assert.True(sameHit (brute points ray 0. infinity) (traverseClosest shallow ray 0. infinity), "flat-depth-limit-keeps-entire-leaf")

    let reads = ref 0
    let bounds () =
        reads.Value <- reads.Value + 1
        BBox(Point(1., -1., -1.), Point(3., 1., 1.))
    let shapes = Array.init 24 (fun _ -> Probe(bounds, (fun ray -> planeTime 2. ray.GetOrigin.X ray.GetDirection.X)) :> Shape)
    for algorithm in algorithms do
        reads.Value <- 0
        let prepared = buildWith algorithm shapes
        Assert.Equal((if algorithm = BruteForce then 0 else shapes.Length), reads.Value, sprintf "%A-caches-each-bounding-box-once" algorithm)
        let before = reads.Value
        traverseClosest prepared ray 0. infinity |> ignore
        anyHit prepared ray 0. infinity |> ignore
        Assert.Equal(before, reads.Value, sprintf "%A-traversal-never-recomputes-shape-bounds" algorithm)

    let savedSelection, savedRegistry = acceleration, listOfAccel
    try
        listOfAccel <- []
        setAcceleration KDTree
        let firstShapes = [| sphere (Point(0., 0., 4.)) 1. |]
        let secondShapes = [| sphere (Point(0., 0., 8.)) 1. |]
        let first = createAcceleration (shapeArray(17, firstShapes, None))
        Assert.Equal(KDTree, first.Kind, "explicit-selection-overrides-flat-default")
        createAcceleration (shapeArray(1, secondShapes, None)) |> ignore
        let again = createAcceleration (shapeArray(17, firstShapes, None))
        Assert.True(Object.ReferenceEquals(first, again), "legacy-cache-uses-id-not-prepended-list-index")
        let collision = createAcceleration (shapeArray(17, secondShapes, None))
        Assert.True(not (Object.ReferenceEquals(first, collision)), "legacy-id-collision-never-substitutes-other-geometry")
        let original = firstShapes.[0]
        let modern = buildWith FlatBVH firstShapes
        firstShapes.[0] <- secondShapes.[0]
        setAcceleration RegularGrid
        let selected = createAcceleration (shapeArray(17, secondShapes, None))
        Assert.Equal(RegularGrid, selected.Kind, "changed-selection-replaces-incompatible-cached-kind")
        Assert.Equal(KDTree, first.Kind, "existing-acceleration-retains-captured-selection")
        listOfAccel <- []
        let hit = traverseClosest modern (Ray(Point.Zero, Vector(0., 0., 1.))) 0. infinity
        Assert.True(Object.ReferenceEquals(original, hit.Shape), "prepared-scene-owns-stable-shape-array-snapshot")
        Assert.Equal(FlatBVH, modern.Kind, "prepared-scene-ignores-global-selection-and-cache-resets")
        Assert.Equal(0, listOfAccel.Length, "modern-build-and-query-do-not-populate-legacy-registry")
    finally
        acceleration <- savedSelection
        listOfAccel <- savedRegistry

let invalidQueries () =
    let tree = buildWith FlatBVH [| pointShape 1. |]
    let rejects name action =
        let mutable rejected = false
        try action ()
        with :? ArgumentException -> rejected <- true
        Assert.True(rejected, name)
    rejects "zero-direction-reported-not-silently-dropped" (fun () -> anyHit tree (Ray(Point.Zero, Vector.Zero)) 0. infinity |> ignore)
    rejects "nan-direction-reported-not-silently-dropped" (fun () -> anyHit tree (Ray(Point.Zero, Vector(Double.NaN, 0., 1.))) 0. infinity |> ignore)
    rejects "nan-interval-rejected" (fun () -> anyHit tree (Ray(Point.Zero, Vector(1., 0., 0.))) Double.NaN infinity |> ignore)
    rejects "invalid-bin-count-rejected" (fun () -> buildFlatWithOptions { FlatBVH.defaultOptions with BinCount = 1 } [||] |> ignore)
    rejects "invalid-depth-rejected" (fun () -> buildFlatWithOptions { FlatBVH.defaultOptions with MaxDepth = 0 } [||] |> ignore)
    let badBounds =
        [| "null", (fun () -> Unchecked.defaultof<BBox>)
           "nan", (fun () -> BBox(Point(Double.NaN, 0., 0.), Point(1., 1., 1.)))
           "infinite", (fun () -> BBox(Point(-infinity, -infinity, -infinity), Point(infinity, infinity, infinity))) |]
    rejects "bounds-only-null-array-rejected" (fun () -> FlatBVH.buildFromBounds null |> ignore)
    rejects "bounds-only-invalid-options-rejected" (fun () ->
        FlatBVH.buildFromBoundsWithOptions { FlatBVH.defaultOptions with BinCount = 1 } [||] |> ignore)
    for name, bounds in badBounds do
        rejects ("bounds-only-" + name + "-rejected") (fun () -> FlatBVH.buildFromBounds [| bounds () |] |> ignore)
    for algorithm in [| KDTree; BVH; RegularGrid; FlatBVH |] do
        for name, bounds in badBounds do
            rejects (sprintf "%A-%s-bounds-rejected-explicitly" algorithm name)
                (fun () -> buildWith algorithm [| sphereWithBounds (Point(0., 0., 3.)) 1. bounds |] |> ignore)
        let failure = InvalidOperationException("A bounding-box failure must not be disguised as unbounded geometry.")
        let shape = sphereWithBounds (Point(0., 0., 3.)) 1. (fun () -> raise failure)
        let mutable propagated = false
        try buildWith algorithm [| shape |] |> ignore
        with :? InvalidOperationException as error -> propagated <- Object.ReferenceEquals(failure, error)
        Assert.True(propagated, sprintf "%A-bounding-box-errors-propagate" algorithm)

let extremeFiniteRays () =
    let cases =
        [| "subnormal-direction", Ray(Point.Zero, Vector(Double.Epsilon, 0., 0.)),
           BBox(Point(Double.Epsilon, 0., 0.), Point(Double.Epsilon, 0., 0.)), 1.
           "overflowing-reciprocal", Ray(Point.Zero, Vector(1e-320, -0., 0.)),
           BBox(Point(1e-320, 0., 0.), Point(2e-320, 0., 0.)), 1.
           "overflowing-coordinate-subtraction", Ray(Point(-1e308, 0., 0.), Vector(1e308, 0., 0.)),
           BBox(Point(1e308, -1., -1.), Point(1.5e308, 1., 1.)), 2.25
           "overflowing-box-extent", Ray(Point.Zero, Vector(-1e308, 0., 0.)),
           BBox(Point(-1e308, -1., -1.), Point(1e308, 1., 1.)), 0.75 |]
    for name, ray, bounds, time in cases do
        let coordinate = Math.FusedMultiplyAdd(time, ray.GetDirection.X, ray.GetOrigin.X)
        let shapes = Array.init 12 (fun index ->
            Probe((fun () -> bounds),
                  (fun ray -> if index = 11 then planeTime coordinate ray.GetOrigin.X ray.GetDirection.X else None)) :> Shape)
        compareScene name shapes [| ray |] [| 0., infinity; 0., time; time, infinity |]

let fartherRootsWithinBounds () =
    let shape = SphereShape(Point(0., 0., 5.), 2., Textures.mkMatTexture Material.None) :> Shape
    let ray = Ray(Point.Zero, Vector(0., 0., 1.))
    let reference = shape.hitFunction (Ray(Point(0., 0., 4.), ray.GetDirection))
    for algorithm in algorithms do
        let tree = buildWith algorithm [| shape |]
        let hit = traverseClosest tree ray 4. infinity
        Assert.True(hit.DidHit && hit.Time = 7. && Object.ReferenceEquals(shape, hit.Shape), sprintf "%A-recovers-exit-after-excluded-entry" algorithm)
        Assert.True(Object.ReferenceEquals(ray, hit.Ray), sprintf "%A-bounded-hit-retains-original-ray" algorithm)
        Assert.True(not hit.FrontFace && hit.GeometricNormal = reference.GeometricNormal && hit.ShadingNormal = reference.ShadingNormal, sprintf "%A-bounded-hit-retains-normal-metadata" algorithm)
        Assert.Equal(reference.UV, hit.UV, sprintf "%A-bounded-hit-retains-texture-coordinates" algorithm)
        Assert.True(anyHit tree ray 3. 8. && not (anyHit tree ray 4. 7.), sprintf "%A-exit-root-obeys-open-query-bounds" algorithm)
    let random = Random 2718
    let rays =
        Array.init 128 (fun _ ->
            let origin = Point(random.NextDouble() - 0.5, random.NextDouble() - 0.5, -2.)
            let target = Point(random.NextDouble() - 0.5, random.NextDouble() - 0.5, 5.)
            Ray(origin, (target - origin).Normalise))
    for algorithm in algorithms do
        let tree = buildWith algorithm [| shape |]
        let mutable recovered = true
        for ray in rays do
            let entry = shape.hitFunction ray
            let exit = traverseClosest tree ray entry.Time infinity
            recovered <- recovered && exit.DidHit && not exit.FrontFace && exit.Time > entry.Time + 1.
        Assert.True(recovered, sprintf "%A-excluded-nonaxial-entry-does-not-repeat-or-hide-exit" algorithm)

let flatExport () =
    let shapes =
        Array.init 41 (fun index ->
            let center = Point(float index * 3., 0., 0.)
            if index % 10 = 0 then unboundedSphere center 1.
            else sphere center 1.)
    let acceleration = buildWith FlatBVH shapes
    let data = (tryExportFlat acceleration).Value
    let nodes = data.Nodes.ToArray()
    let indices = data.PrimitiveIndices.ToArray()
    let fallback = data.UnboundedPrimitiveIndices.ToArray()
    Assert.Equal(shapes.Length, data.PrimitiveCount, "flat-export-retains-finite-and-unbounded-primitives")
    Assert.Equal([| 0; 10; 20; 30; 40 |], fallback, "flat-export-explicit-original-unbounded-indices")
    Assert.True(not data.IsFullyBounded, "flat-export-never-hides-unbounded-fallback")
    Assert.Equal([| 0 .. shapes.Length - 1 |], Array.append indices fallback |> Array.sort, "flat-export-index-permutation-preserves-source-identity")
    Assert.Equal(0, data.RootIndex, "flat-export-root-is-first-packed-node")
    let covered = Array.zeroCreate<int> indices.Length
    let mutable layoutValid = true
    let rec visit index depth =
        if index < 0 || index >= nodes.Length || depth > data.MaxDepth then layoutValid <- false
        else
            let node = nodes.[index]
            layoutValid <- layoutValid && node.MinX <= node.MaxX && node.MinY <= node.MaxY && node.MinZ <= node.MaxZ
            if node.IsLeaf then
                layoutValid <- layoutValid && node.First >= 0 && node.First + node.Count <= indices.Length && node.Right = -1
                if layoutValid then
                    for offset = node.First to node.First + node.Count - 1 do covered.[offset] <- covered.[offset] + 1
            else
                layoutValid <- layoutValid && node.Count = 0 && node.First = index + 1 && node.Right > node.First
                visit node.First (depth + 1)
                visit node.Right (depth + 1)
    visit data.RootIndex 1
    Assert.True(layoutValid && (covered |> Array.forall ((=) 1)), "flat-export-child-links-and-leaf-index-spans-cover-every-primitive-once")

    let mutable exportedNodes = Unchecked.defaultof<ArraySegment<FlatBVH.NodeData>>
    let mutable exportedIndices = Unchecked.defaultof<ArraySegment<int>>
    Assert.True(MemoryMarshal.TryGetArray(data.Nodes, &exportedNodes) && MemoryMarshal.TryGetArray(data.PrimitiveIndices, &exportedIndices), "flat-export-provides-contiguous-blittable-data")
    exportedNodes.Array.[exportedNodes.Offset] <- Unchecked.defaultof<FlatBVH.NodeData>
    exportedIndices.Array.[exportedIndices.Offset] <- -1
    let fresh = (tryExportFlat acceleration).Value
    Assert.Equal(nodes, fresh.Nodes.ToArray(), "flat-export-node-snapshot-does-not-alias-live-traversal")
    Assert.Equal(indices, fresh.PrimitiveIndices.ToArray(), "flat-export-index-snapshot-does-not-alias-live-traversal")
    let ray = Ray(Point(-3., 0., 0.), Vector(1., 0., 0.))
    Assert.True(sameHit (brute shapes ray 0. infinity) (traverseClosest acceleration ray 0. infinity), "flat-export-consumer-cannot-corrupt-scene-traversal")

    let empty = (tryExportFlat (buildWith FlatBVH [||])).Value
    Assert.True(empty.RootIndex = -1 && empty.Nodes.IsEmpty && empty.PrimitiveCount = 0 && empty.IsFullyBounded, "flat-export-empty-scene-is-explicit")
    let unbounded = (tryExportFlat (buildWith FlatBVH [| shapes.[0] |])).Value
    Assert.True(unbounded.RootIndex = -1 && unbounded.Nodes.IsEmpty && unbounded.PrimitiveCount = 1 && not unbounded.IsFullyBounded, "flat-export-unbounded-only-scene-is-not-an-empty-scene")
    let sparse = (tryExportFlat (buildWith FlatBVH [| Shape.None; shapes.[1]; Shape.None; shapes.[0] |])).Value
    Assert.Equal(4, sparse.PrimitiveCount, "flat-export-source-count-includes-empty-shape-slots")
    Assert.Equal(2, sparse.ActivePrimitiveCount, "flat-export-active-count-excludes-empty-shape-slots")
    Assert.Equal([| 0; 2 |], sparse.EmptyPrimitiveIndices.ToArray(), "flat-export-empty-shape-indices-are-explicit")
    Assert.Equal([| 1 |], sparse.PrimitiveIndices.ToArray(), "flat-export-retains-sparse-original-finite-index")
    Assert.Equal([| 3 |], sparse.UnboundedPrimitiveIndices.ToArray(), "flat-export-retains-sparse-original-unbounded-index")
    let emptyShape = (tryExportFlat (buildWith FlatBVH [| Shape.None |])).Value
    Assert.True(emptyShape.RootIndex = -1 && emptyShape.PrimitiveCount = 1 && emptyShape.ActivePrimitiveCount = 0
                && emptyShape.EmptyPrimitiveIndices.ToArray() = [| 0 |], "flat-export-explicit-empty-geometry-never-becomes-unbounded")
    for algorithm in [| KDTree; BVH; RegularGrid; BruteForce |] do
        Assert.True((tryExportFlat (buildWith algorithm shapes)).IsNone, sprintf "%A-export-does-not-silently-rebuild-flat-bvh" algorithm)

let boundsOnlyBuild () =
    let boxes =
        Array.init 33 (fun index ->
            if index % 11 = 0 then BBox.Empty
            else BBox(Point(float index * 2., -1., 0.), Point(float index * 2. + 1., 1., 0.)))
    let shapes = boxes |> Array.map (fun box -> Probe((fun () -> box), (fun _ -> None)) :> Shape)
    let options = { FlatBVH.defaultOptions with LeafSize = 2; BinCount = 8; MaxDepth = 12 }
    let expected = (tryExportFlat (buildFlatWithOptions options shapes)).Value
    let actual = FlatBVH.buildFromBoundsWithOptions options boxes
    Assert.Equal(expected.Nodes.ToArray(), actual.Nodes.ToArray(), "bounds-only-builder-reuses-identical-sah-node-layout")
    Assert.Equal(expected.PrimitiveIndices.ToArray(), actual.PrimitiveIndices.ToArray(), "bounds-only-builder-preserves-identical-primitive-order")
    Assert.Equal(expected.MaxDepth, actual.MaxDepth, "bounds-only-builder-reports-identical-depth")
    Assert.Equal([| 0; 11; 22 |], actual.EmptyPrimitiveIndices.ToArray(), "bounds-only-builder-retains-empty-source-slots")
    Assert.True(actual.PrimitiveCount = 33 && actual.ActivePrimitiveCount = 30 && actual.IsFullyBounded, "bounds-only-builder-source-accounting")
    let defaultExpected = (tryExportFlat (buildWith FlatBVH shapes)).Value
    Assert.Equal(defaultExpected.Nodes.ToArray(), (FlatBVH.buildFromBounds boxes).Nodes.ToArray(), "bounds-only-builder-shares-default-options")
    let empty = FlatBVH.buildFromBounds [||]
    Assert.True(empty.RootIndex = -1 && empty.Nodes.IsEmpty && empty.PrimitiveIndices.IsEmpty && empty.MaxDepth = 0, "bounds-only-builder-empty-input")

let actualUnboundedGeometry () =
    let texture = Textures.mkMatTexture Material.None
    let plane = InfinitePlane(texture) :> Shape
    let horizontal = Transform.transform plane (Transformation.rotateX (Math.PI / 2.))
    let floor = Transform.transform horizontal (Transformation.translate 0. -2. 0.)
    let implicitSurface = (Tracer.ImplicitSurfaces.mkImplicit "x^2+y^2+z^2-1").toShape texture
    let shiftedImplicit = Transform.transform implicitSurface (Transformation.translate 5. 0. 0.)
    for name, shape in [| "plane", plane; "rotated-plane", horizontal; "floor", floor
                          "implicit", implicitSurface; "transformed-implicit", shiftedImplicit |] do
        Assert.True(shape.Bounds.IsNone, name + "-explicit-unbounded-contract")
    let downward = Ray(Point(0., 2., 0.), Vector(0., -1., 0.))
    compareScene "actual-transformed-infinite-floor"
        [| floor; sphere (Point(0., 1., 0.)) 0.25; Shape.None |]
        [| downward
           Ray(Point(1000., 2., 1000.), Vector(0., -1., 0.))
           Ray(Point(0., -3., 0.), Vector(0., 1., 0.))
           Ray(Point.Zero, Vector(1., 0., 0.)) |]
        [| 0., infinity; 0., 0.5; 2.5, 8. |]
    let hit = traverseClosest (buildWith FlatBVH [| floor |]) downward 0. infinity
    Assert.True(hit.DidHit && abs (hit.Time - 4.) <= 1e-12 && Object.ReferenceEquals(floor, hit.Shape), "infinite-floor-build-never-forces-throwing-bounding-box-getter")
    compareScene "actual-unbounded-transformed-implicit"
        [| shiftedImplicit; sphere (Point(2., 0., 0.)) 0.25 |]
        [| Ray(Point.Zero, Vector(1., 0., 0.)); Ray(Point(10., 0., 0.), Vector(-1., 0., 0.)) |]
        [| 0., infinity; 3., infinity; 4., 7. |]

let allTest () =
    closestAgainstBruteForce ()
    degenerateScenes ()
    boundedAndLegacyTraversal ()
    depthAndOwnership ()
    invalidQueries ()
    extremeFiniteRays ()
    fartherRootsWithinBounds ()
    flatExport ()
    boundsOnlyBuild ()
    actualUnboundedGeometry ()
