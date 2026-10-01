namespace Tracer.Basics

open System

/// Picks which local light next-event estimation samples at a path vertex.
///
/// Lights split into two groups:
/// * Global lights (directional, environment and anything unrecognised) are always sampled, every
///   vertex, exactly as before. A sun and a sky never compete with lamps for samples.
/// * Local lights (point, sphere and area lights) are sampled exhaustively while there are at most
///   `LightSelection.ExhaustiveLimit` of them, which keeps small scenes bit-identical. Above that, one
///   local light is chosen per vertex by descending a light BVH, taking each child with probability
///   proportional to an importance estimate: emitted power over squared distance (clamped to the
///   node's own extent so a receiver inside a cluster does not blow up), zero for a node entirely
///   behind an opaque surface. Its contribution is divided by the selection probability, which
///   `Probability` reproduces exactly for MIS when a BSDF ray lands on an area light.
///
/// Importance is only a heuristic; the estimator is unbiased as long as every light that can
/// contribute has a positive probability, which holds because only lights that are entirely behind
/// the shading plane of an opaque surface (and therefore contribute nothing) get zero.
[<AllowNullLiteral>]
type private LightNode(min: Point, max: Point, power: float, left: LightNode, right: LightNode, light: int) =
    member _.Min = min
    member _.Max = max
    member _.Power = power
    member _.Left = left
    member _.Right = right
    /// Index into the scene's light array for a leaf; -1 for an inner node.
    member _.Light = light
    member val Parent: LightNode = null with get, set

type LightSelection(lights: Light[]) =
    static let luminance (c: Colour) = 0.2126 * c.R + 0.7152 * c.G + 0.0722 * c.B

    /// Bounds and power of a local light, or None for a global one.
    static let describe (light: Light) =
        match light with
        | :? PointLight as point ->
            let p = point.Position
            Some (p, p, 4. * Math.PI * luminance (point.GetColour Unchecked.defaultof<HitPoint>))
        | :? SphereLight as sphere ->
            let r = Vector(sphere.Radius, sphere.Radius, sphere.Radius)
            Some (sphere.Position - r, sphere.Position + r, sphere.Power)
        | :? AreaLight as area ->
            let box = area.Shape.getBoundingBox ()
            // Uniform area sampling: the density is one over the area. A Lambertian emitter of
            // radiance L and area A emits pi L A.
            let surface = area.SampleSurface(Point.Zero, 0.5, 0.5)
            let areaSize = if surface.AreaPdf > 0. && Double.IsFinite surface.AreaPdf then 1. / surface.AreaPdf else 1.
            Some (box.lowPoint, box.highPoint, Math.PI * areaSize * luminance (area.GetColour Unchecked.defaultof<HitPoint>))
        | _ -> None

    let described = lights |> Array.map describe
    let globals = [| for i in 0 .. lights.Length - 1 do if described.[i].IsNone then yield i |]
    let locals = [| for i in 0 .. lights.Length - 1 do if described.[i].IsSome then yield i |]
    let exhaustive = locals.Length <= LightSelection.ExhaustiveLimit
    let leaves : LightNode[] = Array.zeroCreate lights.Length

    let root : LightNode =
        if exhaustive then null
        else
            let candidates =
                locals |> Array.choose (fun i ->
                    let lo, hi, power = described.[i].Value
                    if Double.IsFinite power && power > 0. then Some (i, lo, hi, power) else None)
            let rec build (items: (int * Point * Point * float)[]) : LightNode =
                if items.Length = 0 then null
                elif items.Length = 1 then
                    let i, lo, hi, power = items.[0]
                    let node = LightNode(lo, hi, power, null, null, i)
                    leaves.[i] <- node
                    node
                else
                    let centre (_, lo: Point, hi: Point, _) = Point((lo.X + hi.X) / 2., (lo.Y + hi.Y) / 2., (lo.Z + hi.Z) / 2.)
                    let cs = items |> Array.map centre
                    let extent axis =
                        let values = cs |> Array.map (fun (c: Point) -> match axis with 0 -> c.X | 1 -> c.Y | _ -> c.Z)
                        Array.max values - Array.min values
                    let axis = [| 0; 1; 2 |] |> Array.maxBy extent
                    let key (c: Point) = match axis with 0 -> c.X | 1 -> c.Y | _ -> c.Z
                    let sorted = Array.zip items cs |> Array.sortBy (fun (_, c) -> key c) |> Array.map fst
                    let half = sorted.Length / 2
                    let left = build sorted.[.. half - 1]
                    let right = build sorted.[half ..]
                    let lo = Point(min left.Min.X right.Min.X, min left.Min.Y right.Min.Y, min left.Min.Z right.Min.Z)
                    let hi = Point(max left.Max.X right.Max.X, max left.Max.Y right.Max.Y, max left.Max.Z right.Max.Z)
                    let node = LightNode(lo, hi, left.Power + right.Power, left, right, -1)
                    left.Parent <- node
                    right.Parent <- node
                    node
            build candidates

    /// Importance of `node` for a receiver at `p` with shading normal `n`.
    let importance (node: LightNode) (p: Point) (n: Vector) (translucent: bool) =
        let lo, hi = node.Min, node.Max
        let facing =
            if translucent then true
            else
                // Positive if any corner of the box lies in front of the shading plane (the box
                // is convex, so otherwise all of it is behind and the light contributes nothing).
                let mutable any = false
                for corner in 0 .. 7 do
                    let x = if corner &&& 1 = 0 then lo.X else hi.X
                    let y = if corner &&& 2 = 0 then lo.Y else hi.Y
                    let z = if corner &&& 4 = 0 then lo.Z else hi.Z
                    if (x - p.X) * n.X + (y - p.Y) * n.Y + (z - p.Z) * n.Z > 0. then any <- true
                any
        if not facing then 0.
        else
            let cx, cy, cz = (lo.X + hi.X) / 2., (lo.Y + hi.Y) / 2., (lo.Z + hi.Z) / 2.
            let dx, dy, dz = cx - p.X, cy - p.Y, cz - p.Z
            let distanceSquared = dx * dx + dy * dy + dz * dz
            let ex, ey, ez = (hi.X - lo.X) / 2., (hi.Y - lo.Y) / 2., (hi.Z - lo.Z) / 2.
            let extentSquared = ex * ex + ey * ey + ez * ez
            let distanceTerm = max distanceSquared (max extentSquared 1e-12)
            if node.Light >= 0 && not translucent && extentSquared = 0. then
                // A point lamp: the exact cosine at the receiver.
                let cosine = (dx * n.X + dy * n.Y + dz * n.Z) / sqrt (max distanceSquared 1e-300)
                node.Power * max 0. cosine / distanceTerm
            else node.Power / distanceTerm

    /// Local lights that are sampled exhaustively above this count are instead selected one per
    /// vertex. Mutable so tests and experiments can force either behaviour.
    static member val ExhaustiveLimit = 8 with get, set

    member _.Lights = lights
    /// Indices of lights that are always sampled (every light when exhaustive).
    member _.Always = if exhaustive then [| 0 .. lights.Length - 1 |] else globals
    member _.Globals = globals
    member _.Locals = locals
    member _.IsExhaustive = exhaustive

    /// Chooses a local light for a receiver: (light index, probability), or (-1, 0) when no local
    /// light can contribute. Only meaningful when not exhaustive.
    member _.Select(p: Point, n: Vector, translucent: bool, u: float) : struct (int * float) =
        if isNull root then struct (-1, 0.)
        else
            let mutable node = root
            let mutable probability = 1.
            let mutable u = u
            let mutable failed = false
            while not failed && node.Light < 0 do
                let a = importance node.Left p n translucent
                let b = importance node.Right p n translucent
                let total = a + b
                if not (total > 0.) || not (Double.IsFinite total) then failed <- true
                else
                    let pa = a / total
                    if u < pa then
                        node <- node.Left
                        probability <- probability * pa
                        u <- min 0.99999999999 (u / pa)
                    else
                        node <- node.Right
                        probability <- probability * (1. - pa)
                        u <- min 0.99999999999 ((u - pa) / (1. - pa))
            if failed || not (probability > 0.) then struct (-1, 0.)
            // A single-light tree still needs its importance to be positive.
            elif obj.ReferenceEquals(node, root) && importance root p n translucent <= 0. then struct (-1, 0.)
            else struct (node.Light, probability)

    /// Probability that `Select` returns `light` for this receiver; 1 for always-sampled lights.
    member _.Probability(p: Point, n: Vector, translucent: bool, light: int) =
        if exhaustive || described.[light].IsNone then 1.
        else
            let leaf = leaves.[light]
            if isNull leaf then 0.
            elif obj.ReferenceEquals(leaf, root) then
                if importance root p n translucent > 0. then 1. else 0.
            else
                let mutable node = leaf
                let mutable probability = 1.
                while not (isNull node.Parent) do
                    let parent = node.Parent
                    let a = importance parent.Left p n translucent
                    let b = importance parent.Right p n translucent
                    let total = a + b
                    let mine = if obj.ReferenceEquals(node, parent.Left) then a else b
                    probability <- if total > 0. && Double.IsFinite total then probability * (mine / total) else 0.
                    node <- parent
                probability
