namespace Tracer.Basics

open System
open System.Collections.Generic
open System.Threading
open Tracer.Basics.Sampling
open Tracer.Basics.PathTracing

/// Unidirectional path tracer with next-event estimation, multiple importance
/// sampling and Russian roulette.
///
/// The structural difference from `ClassicIntegrator` is that this traces ONE
/// path per camera sample instead of branching. The classic integrator spawns
/// `glossySamples` children at every bounce and recurses into all of them, so
/// its cost is glossySamples^depth - 1024 rays per camera sample at 4 samples
/// and depth 5. Here the cost is 2*depth: one continuation ray and one shadow
/// ray per vertex. Quality comes from tracing more paths, not from branching,
/// which is both cheaper per unit of noise and makes depth 10-20 affordable.
type PathIntegrator
    (scene: Scene, query: IRayQuery, allOpaque: bool, cancellation: CancellationToken,
     maxDepth: int, rouletteDepth: int) =

    let lights = List.toArray scene.Lights
    let environments =
        lights |> Array.choose (function :? EnvironmentLight as light -> Some light | _ -> None)

    /// Area lights are geometry, so a BSDF ray can land on one directly. MIS
    /// needs to recognize that and ask what the light-sampling density would
    /// have been, which means mapping the hit shape back to its light.
    let areaLightByShape =
        let map = Dictionary<Shape, AreaLight>(HashIdentity.Reference)
        for light in lights do
            match light with
            | :? AreaLight as area -> map.[area.Shape] <- area
            | _ -> ()
        map

    let isDeltaLight (light: Light) =
        match light with
        | :? PointLight | :? DirectionalLight -> true
        | _ -> false

    /// Beer-Lambert absorption over a segment inside a filtering medium.
    let attenuation (filter: Colour) distance =
        if filter.R = 1. && filter.G = 1. && filter.B = 1. then Colour.White
        elif Double.IsPositiveInfinity distance then
            Colour((if filter.R = 1. then 1. else 0.),
                   (if filter.G = 1. then 1. else 0.),
                   (if filter.B = 1. then 1. else 0.))
        else MaterialTransport.absorption filter distance

    let background (direction: Vector) =
        if environments.Length = 0 then scene.BackgroundColour
        else
            let mutable colour = Colour.Black
            for light in environments do colour <- colour + light.Radiance direction
            colour

    /// Power heuristic with beta = 2 (Veach & Guibas 1995). Squaring suppresses
    /// the low-density strategy harder than the balance heuristic, which is what
    /// keeps glossy-under-area-light from fireflying.
    let powerHeuristic (pdfA: float) (pdfB: float) =
        if not (Double.IsFinite pdfA) || pdfA <= 0. then 0.
        elif not (Double.IsFinite pdfB) || pdfB <= 0. then 1.
        else
            let a = pdfA * pdfA
            let b = pdfB * pdfB
            let total = a + b
            if total <= 0. || not (Double.IsFinite total) then 0. else a / total

    let clampColour (c: Colour) =
        let fix v = if Double.IsFinite v && v > 0. then v else 0.
        Colour(fix c.R, fix c.G, fix c.B)

    let mulColour (a: Colour) (b: Colour) =
        clampColour (Colour(a.R * b.R, a.G * b.G, a.B * b.B))

    let addColour (a: Colour) (b: Colour) =
        clampColour (Colour(a.R + b.R, a.G + b.G, a.B + b.B))

    let scaleColour (c: Colour) (s: float) =
        if not (Double.IsFinite s) || s <= 0. then Colour.Black
        else clampColour (Colour(c.R * s, c.G * s, c.B * s))

    let maxComponent (c: Colour) = max c.R (max c.G c.B)

    /// Shadow-ray transmittance. Opaque scenes get a cheap any-hit test; with
    /// any transparent geometry present we have to walk the segment and
    /// accumulate Fresnel and absorption, exactly as the classic integrator does.
    let visibility (ray: Ray) (distance: float) (filter: Colour) =
        if distance <= 0. then Colour.White
        elif allOpaque then
            if query.Any(ray, 0., distance) then Colour.Black else Colour.White
        else
            let mutable currentRay = ray
            let mutable remaining = distance
            let mutable transmittance = Colour.White
            let mutable finished = false
            let mutable crossings = 0
            while not finished do
                cancellation.ThrowIfCancellationRequested()
                let hit = query.Closest(currentRay, 0., remaining)
                if not hit.DidHit then
                    transmittance <- mulColour transmittance (attenuation filter remaining)
                    finished <- true
                else
                    crossings <- crossings + 1
                    if crossings > 2048 then
                        invalidOp "Shadow transmission exceeded the supported surface-crossing limit."
                    let travelled = hit.Time * currentRay.GetDirection.Magnitude
                    transmittance <- mulColour transmittance (attenuation filter travelled)
                    match hit.Material with
                    | :? TransparentMaterial as material ->
                        let etaT, etaI = material.Indices hit
                        let normal = if hit.FrontFace then hit.GeometricNormal else -hit.GeometricNormal
                        let cosine = max 0. (min 1. (-currentRay.GetDirection.Normalise * normal))
                        let fresnel = Fresnel.dielectric cosine etaI etaT
                        transmittance <- scaleColour transmittance (1. - fresnel)
                        let nextRay = hit.SpawnRay currentRay.GetDirection
                        let advanced = (nextRay.GetOrigin - currentRay.GetOrigin) * currentRay.GetDirection.Normalise
                        if advanced <= 0. || not (Double.IsFinite advanced) then
                            invalidOp "A transparent shadow ray failed to advance."
                        remaining <- max 0. (remaining - advanced)
                        currentRay <- nextRay
                        if transmittance.IsBlack || remaining = 0. then finished <- true
                    | _ ->
                        transmittance <- Colour.Black
                        finished <- true
            transmittance

    /// Solid-angle density that light sampling would have used for `direction`
    /// arriving at `hit` from area light `light`.
    let areaLightPdf (light: AreaLight) (origin: Point) (lightHit: HitPoint) =
        let difference = lightHit.Point - origin
        let distanceSquared = difference.MagnitudeSquared
        if distanceSquared <= 0. then 0.
        else
            let direction = difference.Normalise
            let cosine = abs (lightHit.Normal.Normalise * direction)
            if cosine <= 1e-9 then 0.
            else
                let areaPdf = light.GetProbabilityDensity lightHit
                if not (Double.IsFinite areaPdf) || areaPdf <= 0. then 0.
                else areaPdf * distanceSquared / cosine

    /// Draw a light sample from explicitly supplied uniform numbers.
    ///
    /// This deliberately bypasses each light's own `Sampler`. Those samplers
    /// hold a fixed table indexed by a key, and the `regular` sampler has a
    /// single set - so every path vertex would reuse the same few directions,
    /// turning next-event estimation into a deterministic quadrature. Under MIS
    /// that is biased, and measurably so: the furnace test reads 1-3% high with
    /// a regular light sampler and converges to 0.1% once the samples are
    /// decorrelated per vertex, as they are here.
    let sampleLight (light: Light) (hit: HitPoint) (u1: float) (u2: float) =
        match light with
        | :? PointLight as point ->
            let difference = point.Position - hit.Point
            let distance = difference.Magnitude
            ValueSome { Direction = difference.Normalise; Distance = distance
                        Radiance = point.GetColour hit; Weight = if distance > 0. then 1. else 0. }
        | :? DirectionalLight as directional ->
            ValueSome { Direction = directional.Direction; Distance = infinity
                        Radiance = directional.GetColour hit; Weight = 1. }
        | :? AreaLight as area ->
            let surface = area.SampleSurface(hit.Point, u1, u2)
            if not (Double.IsFinite surface.AreaPdf) || surface.AreaPdf <= 0. then ValueNone
            else
                let difference = surface.Point - hit.Point
                let distance = difference.Magnitude
                if distance <= 0. then ValueNone
                else
                    let direction = difference.Normalise
                    let cosine = max 0. (surface.Normal.Normalise * -direction)
                    ValueSome { Direction = direction; Distance = distance; Radiance = area.GetColour hit
                                Weight = cosine / (distance * distance * surface.AreaPdf) }
        | :? EnvironmentLight as environment ->
            let x, y, z = mapToHemisphere (u1, u2) 1.
            let frame = ShadingFrame.ofNormal hit.Normal
            let direction = (frame.ToWorld(Vector(x, y, z))).Normalise
            let cosine = max 0. (direction * hit.Normal)
            if cosine <= 0. then ValueNone
            else
                ValueSome { Direction = direction; Distance = infinity
                            Radiance = environment.Radiance direction; Weight = Math.PI / cosine }
        | _ -> ValueNone

    /// Next-event estimation: sample every light directly and MIS the result
    /// against what BSDF sampling would have produced for the same direction.
    let directLighting (hit: HitPoint) (surface: SurfaceParams) (frame: ShadingFrame)
                       (wo: Vector) (key: uint64) (mediumFilter: Colour) =
        let mutable total = Colour.Black
        for lightIndex = 0 to lights.Length - 1 do
            let light = lights.[lightIndex]
            let sampleCount = LightSampling.sampleCount light
            let lightKey = mixKey (key ^^^ (uint64 lightIndex + 0x51ED2701UL))
            let mutable contribution = Colour.Black
            for sampleIndex = 0 to sampleCount - 1 do
                let u1, u2 = sample2D lightKey (sampleIndex + 3)
                match sampleLight light hit u1 u2 with
                | ValueNone -> ()
                | ValueSome sample ->
                if sample.Weight > 0. && not sample.Radiance.IsBlack then
                    let wi = frame.ToLocal sample.Direction
                    if wi.Z > 0. then
                        let f = Bsdf.evalLocal surface wo wi
                        if not f.IsBlack then
                            let weight =
                                if isDeltaLight light then 1.
                                else
                                    // Weight is cos/(d^2 * areaPdf), so its
                                    // reciprocal is the solid-angle density.
                                    let lightPdf = 1. / sample.Weight
                                    let bsdfPdf = Bsdf.pdfLocal surface wo wi
                                    powerHeuristic lightPdf bsdfPdf
                            if weight > 0. then
                                let origin = hit.ShadowOrigin sample.Direction
                                let maximum =
                                    if Double.IsPositiveInfinity sample.Distance then infinity
                                    else
                                        let target = hit.Point + sample.Distance * sample.Direction
                                        let delta = target - origin
                                        let scale =
                                            max (max (abs origin.X) (max (abs origin.Y) (abs origin.Z)))
                                                (max (abs target.X) (max (abs target.Y) (abs target.Z)))
                                        let margin = 32. * 2.220446049250313e-16 * scale
                                        max 0. (Math.BitDecrement(delta.Magnitude - margin))
                                let shadowRay = Ray(origin, sample.Direction, hit.Ray.ShutterTime)
                                let transmittance = visibility shadowRay maximum mediumFilter
                                if not transmittance.IsBlack then
                                    let radiance = mulColour sample.Radiance transmittance
                                    contribution <-
                                        addColour contribution
                                            (scaleColour (mulColour f radiance) (sample.Weight * weight))
            if sampleCount > 0 then
                total <- addColour total (scaleColour contribution (1. / float sampleCount))
        total

    member _.MaxDepth = maxDepth

    /// Trace one path, returning radiance plus the denoiser guide buffers.
    ///
    /// Albedo and normal are captured at the first NON-SPECULAR hit rather than
    /// the first hit outright. Taking them at a mirror or a glass surface would
    /// describe the interface instead of what is seen through it, and the
    /// denoiser would smooth reflections and refractions into mush.
    member _.TraceWithGuides(ray: Ray, key: uint64) =
        let mutable radiance = Colour.Black
        let mutable throughput = Colour.White
        let mutable currentRay = ray
        // A camera ray, and any ray leaving a delta lobe, cannot have been
        // produced by light sampling, so emission it lands on counts in full.
        let mutable previousWasSpecular = true
        let mutable previousBsdfPdf = 0.
        let mutable previousPoint = ray.GetOrigin
        // Shading normal of the vertex the current ray left, needed to evaluate
        // the environment light's density for MIS when the ray escapes.
        let mutable previousNormal = Vector(0., 0., 1.)
        // Stack of transparent materials the ray is currently inside, innermost
        // first. Drives both the relative IOR at the next boundary and the
        // absorption applied along each segment.
        let mutable media : TransparentMaterial list = []
        let mutable depth = 0
        let mutable alive = true
        let mutable guideAlbedo = Colour.Black
        let mutable guideNormal = Vector.Zero
        let mutable guidesCaptured = false

        while alive && depth <= maxDepth do
            cancellation.ThrowIfCancellationRequested()
            let hit = query.Closest(currentRay, 0., infinity)
            let mediumFilter =
                match media with
                | material :: _ -> material.InnerFilterColour
                | [] -> Colour.White

            if not hit.DidHit then
                let travelled = infinity
                let escaped = mulColour throughput (attenuation mediumFilter travelled)
                let sky = background currentRay.GetDirection
                if not sky.IsBlack then
                    let weight =
                        if previousWasSpecular || environments.Length = 0 then 1.
                        else
                            // EnvironmentLight samples the cosine hemisphere
                            // about the shading normal of the vertex the ray
                            // left, so its density is cos/pi measured against
                            // THAT normal - not a constant. Every environment
                            // light shares this density, so one weight covers
                            // however many of them the scene has.
                            let cosine = max 0. (currentRay.GetDirection.Normalise * previousNormal)
                            let envPdf = cosine / Math.PI
                            powerHeuristic previousBsdfPdf envPdf
                    radiance <- addColour radiance (scaleColour (mulColour escaped sky) weight)
                alive <- false
            else
                let segment = hit.Time * currentRay.GetDirection.Magnitude
                throughput <- mulColour throughput (attenuation mediumFilter segment)

                // Emission. When the previous bounce was sampled from a
                // non-delta lobe, NEE already had a chance at this light, so MIS
                // decides how much of it this path may claim.
                let emission = MaterialAdapter.emissionAt hit
                if not emission.IsBlack then
                    let weight =
                        if previousWasSpecular then 1.
                        else
                            match areaLightByShape.TryGetValue hit.Shape with
                            | true, light ->
                                let lightPdf = areaLightPdf light previousPoint hit
                                powerHeuristic previousBsdfPdf lightPdf
                            | _ -> 1.
                    radiance <- addColour radiance (scaleColour (mulColour throughput emission) weight)

                if MaterialAdapter.isPurelyEmissive hit.Material then alive <- false
                else
                    let exteriorIor =
                        match media with
                        | material :: _ -> material.InnerRefractionIndex
                        | [] -> 1.
                    let surface = MaterialAdapter.surfaceAt hit exteriorIor
                    let frame = ShadingFrame.ofNormal hit.Normal
                    let wo = frame.ToLocal(-currentRay.GetDirection.Normalise)

                    if wo.Z <= 0. then alive <- false
                    else
                        if not guidesCaptured && not (Bsdf.isSmooth surface) && surface.Transmission < 0.5 then
                            guideAlbedo <- surface.BaseColour
                            guideNormal <- frame.Normal
                            guidesCaptured <- true
                        let vertexKey = mixKey (key ^^^ (uint64 depth * 0x9E3779B1UL + 0x2545F491UL))

                        radiance <-
                            addColour radiance
                                (mulColour throughput (directLighting hit surface frame wo vertexKey mediumFilter))

                        let uLobe, _ = sample2D vertexKey 0
                        let u1, u2 = sample2D vertexKey 1
                        let bsdf = Bsdf.sampleLocal surface wo uLobe u1 u2

                        if bsdf.Weight.IsBlack || (bsdf.Pdf <= 0. && not bsdf.IsSpecular) then alive <- false
                        else
                            throughput <- mulColour throughput bsdf.Weight
                            if throughput.IsBlack then alive <- false
                            else
                                let worldDirection = (frame.ToWorld bsdf.Direction).Normalise
                                if bsdf.IsTransmitted then
                                    match hit.Material with
                                    | :? TransparentMaterial as material ->
                                        media <-
                                            if hit.FrontFace then material :: media
                                            else
                                                match media with
                                                | _ :: rest -> rest
                                                | [] -> []
                                    | _ -> ()

                                previousWasSpecular <- bsdf.IsSpecular
                                previousBsdfPdf <- bsdf.Pdf
                                previousPoint <- hit.Point
                                previousNormal <- frame.Normal
                                currentRay <- hit.SpawnRay worldDirection
                                depth <- depth + 1

                                // Russian roulette. It raises variance but cuts
                                // cost faster, so efficiency improves; starting
                                // it only after a few bounces keeps the
                                // low-order, high-energy vertices intact.
                                if alive && depth >= rouletteDepth then
                                    let survival = max 0.05 (min 0.95 (maxComponent throughput))
                                    let q, _ = sample2D vertexKey 2
                                    if q > survival then alive <- false
                                    else throughput <- scaleColour throughput (1. / survival)
        struct (radiance, guideAlbedo, guideNormal)

    /// Trace one path and return the radiance it carries back to the camera.
    member this.Trace(ray: Ray, key: uint64) =
        let struct (radiance, _, _) = this.TraceWithGuides(ray, key)
        radiance

    interface IIntegrator with
        member this.Trace(ray, key) = this.Trace(ray, key)
        member this.TraceWithGuides(ray, key) = this.TraceWithGuides(ray, key)
