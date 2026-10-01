namespace Tracer.Basics

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Threading
open Tracer.Basics.Sampling

type IRayQuery =
    abstract member Closest: Ray * float * float -> HitPoint
    abstract member Any: Ray * float * float -> bool

/// Lets the renderer drive either the classic Whitted integrator or the path
/// tracer without knowing which it holds.
type IIntegrator =
    abstract member Trace: Ray * uint64 -> Colour

    /// Radiance plus the guide buffers a denoiser needs: surface albedo and
    /// shading normal at the first non-specular hit. Integrators that do not
    /// support denoising return black and zero, which the renderer treats as
    /// "no guide available".
    abstract member TraceWithGuides: Ray * uint64 -> struct (Colour * Colour * Vector)

type private MediumEntry =
    { Shape: Shape
      Material: TransparentMaterial }

type private MediumState =
    { Stack: MediumEntry list
      ExteriorIor: float
      ExteriorFilter: Colour }

type ClassicIntegrator(scene: Scene, query: IRayQuery, allOpaque: bool, cancellation: CancellationToken) =
    let lights = List.toArray scene.Lights
    /// Every light while there are few local ones; otherwise global lights plus one selected
    /// local light per hit, divided by its selection probability (see LightSelection).
    let selection = LightSelection(lights)
    let environments =
        lights |> Array.choose (function :? EnvironmentLight as light -> Some light | _ -> None)
    /// Height fog over each camera and secondary ray segment outside transparent media (shadow rays skip it).
    let fog = scene.Fog
    let air = { Stack = []; ExteriorIor = 1.; ExteriorFilter = Colour.White }
    let originMedia = ConcurrentDictionary<struct (float * float * float), Lazy<MediumState>>()

    let mediumProperties medium =
        match medium.Stack with
        | current :: _ -> current.Material.InnerRefractionIndex, current.Material.InnerFilterColour
        | [] -> medium.ExteriorIor, medium.ExteriorFilter

    let incomingMedium medium (hit: HitPoint) =
        match hit.Material, medium.Stack with
        | (:? TransparentMaterial as material), [] when hit.FrontFace ->
            { medium with ExteriorIor = material.OuterRefractionIndex; ExteriorFilter = material.OuterFilterColour }
        | _ -> medium

    let attenuation (filter: Colour) distance =
        if filter.R = 1. && filter.G = 1. && filter.B = 1. then Colour.White
        elif Double.IsPositiveInfinity distance then
            Colour((if filter.R = 1. then 1. else 0.),
                   (if filter.G = 1. then 1. else 0.),
                   (if filter.B = 1. then 1. else 0.))
        else MaterialTransport.absorption filter distance

    let crossBoundary medium (hit: HitPoint) (material: TransparentMaterial) =
        let etaI, _ = mediumProperties medium
        if hit.FrontFace then
            etaI, material.InnerRefractionIndex,
            { medium with Stack = { Shape = hit.Shape; Material = material } :: medium.Stack }
        else
            let remaining = medium.Stack |> List.filter (fun entry -> not (obj.ReferenceEquals(entry.Shape, hit.Shape)))
            let next =
                { Stack = remaining
                  ExteriorIor = if List.isEmpty remaining then material.OuterRefractionIndex else medium.ExteriorIor
                  ExteriorFilter = if List.isEmpty remaining then material.OuterFilterColour else medium.ExteriorFilter }
            let etaT, _ = mediumProperties next
            (if List.isEmpty medium.Stack then material.InnerRefractionIndex else etaI), etaT, next

    /// Escaped-ray radiance. Camera rays see each environment's `Visible` radiance (a sky's sun
    /// disc included); secondary rays see only its lighting `Radiance`, because a sun drawn in the
    /// sky is also a DirectionalLight and would otherwise be counted twice (and as fireflies).
    let background (direction: Vector) (cameraRay: bool) =
        if environments.Length = 0 then scene.BackgroundColour
        else
            let mutable colour = Colour.Black
            for light in environments do
                colour <- colour + (if cameraRay then light.Visible direction else light.Radiance direction)
            colour

    let initialMedium (origin: Point) =
        if allOpaque then air
        else
            let seen = HashSet<Shape>(HashIdentity.Reference)
            let inside = ResizeArray<MediumEntry>()
            let direction = Vector(0.371, 0.529, 0.764).Normalise
            let mutable ray = Ray(origin, direction)
            let mutable finished = false
            let mutable crossings = 0
            while not finished do
                cancellation.ThrowIfCancellationRequested()
                let hit = query.Closest(ray, 0., infinity)
                if not hit.DidHit then finished <- true
                else
                    crossings <- crossings + 1
                    if crossings > 2048 then invalidOp "Could not classify the camera medium: too many surface crossings."
                    match hit.Material with
                    | :? TransparentMaterial as material when seen.Add hit.Shape ->
                        if not hit.FrontFace then inside.Add { Shape = hit.Shape; Material = material }
                    | _ -> ()
                    ray <- hit.SpawnRay direction
            if inside.Count = 0 then air
            else
                let outer = inside.[inside.Count - 1].Material
                { Stack = List.ofSeq inside; ExteriorIor = outer.OuterRefractionIndex; ExteriorFilter = outer.OuterFilterColour }

    let mediumFactory =
        Func<struct (float * float * float), Lazy<MediumState>>(fun struct (x, y, z) ->
            lazy (initialMedium (Point(x, y, z))))

    let mediumAtOrigin (origin: Point) =
        if allOpaque then air
        else
            let key = struct (origin.X, origin.Y, origin.Z)
            match originMedia.TryGetValue key with
            | true, medium -> medium.Value
            | _ ->
                // Bound storage for cameras that generate a different aperture origin for every ray.
                if originMedia.Count < 16384 then originMedia.GetOrAdd(key, mediumFactory).Value
                else initialMedium origin

    let shadowSegment (hit: HitPoint) (sample: LightSample) =
        let origin = hit.ShadowOrigin sample.Direction
        if Double.IsPositiveInfinity sample.Distance then Ray(origin, sample.Direction, hit.Ray.ShutterTime), infinity
        else
            let target = hit.Point + sample.Distance * sample.Direction
            let delta = target - origin
            let distance = delta.Magnitude
            let scale =
                max (max (abs origin.X) (max (abs origin.Y) (abs origin.Z)))
                    (max (abs target.X) (max (abs target.Y) (abs target.Z)))
            let endpointMargin = 32. * 2.220446049250313e-16 * scale
            Ray(origin, delta.Normalise, hit.Ray.ShutterTime), max 0. (Math.BitDecrement(distance - endpointMargin))

    let visibility (ray: Ray) distance medium =
        if distance <= 0. then Colour.White
        elif allOpaque then
            if query.Any(ray, 0., distance) then Colour.Black else Colour.White
        else
            let mutable currentRay = ray
            let mutable remaining = distance
            let mutable currentMedium = medium
            let mutable transmittance = Colour.White
            let mutable finished = false
            let mutable crossings = 0
            while not finished do
                cancellation.ThrowIfCancellationRequested()
                let hit = query.Closest(currentRay, 0., remaining)
                if hit.DidHit then currentMedium <- incomingMedium currentMedium hit
                let _, filter = mediumProperties currentMedium
                if not hit.DidHit then
                    transmittance <- transmittance * attenuation filter remaining
                    finished <- true
                else
                    crossings <- crossings + 1
                    if crossings > 2048 then invalidOp "Shadow transmission exceeded the supported surface-crossing limit."
                    let travelled = hit.Time * currentRay.GetDirection.Magnitude
                    transmittance <- transmittance * attenuation filter travelled
                    match hit.Material with
                    | :? TransparentMaterial as material ->
                        let etaI, etaT, nextMedium = crossBoundary currentMedium hit material
                        let normal = if hit.FrontFace then hit.GeometricNormal else -hit.GeometricNormal
                        let fresnel, _ = SurfaceSampling.dielectric currentRay.GetDirection normal etaI etaT
                        transmittance <- transmittance * (1. - fresnel)
                        let nextRay = hit.SpawnRay currentRay.GetDirection
                        let advanced = (nextRay.GetOrigin - currentRay.GetOrigin) * currentRay.GetDirection.Normalise
                        if advanced <= 0. || not (Double.IsFinite advanced) then
                            invalidOp "A transparent shadow ray failed to advance."
                        remaining <- max 0. (remaining - advanced)
                        currentRay <- nextRay
                        currentMedium <- nextMedium
                        if transmittance.IsBlack || remaining = 0. then finished <- true
                    | _ ->
                        transmittance <- Colour.Black
                        finished <- true
            transmittance

    /// Shadow rays cast from inside the bounded volume, which lies outside any transparent medium.
    let volumeVisibility = fun (ray: Ray) (distance: float) -> visibility ray distance air

    let ambient (hit: HitPoint) key medium =
        let colour = hit.Material.AmbientColour(hit, scene.Ambient)
        if colour.IsBlack then colour
        else
            match scene.Ambient with
            | :? AmbientOccluder as occluder ->
                let u, v, w = SurfaceSampling.frame hit.Normal
                let mutable sum = 0.
                for index = 0 to occluder.Sampler.SampleCount - 1 do
                    let x, y, z = mapToHemisphere (occluder.Sampler.SampleAt(key, index)) 1.
                    let direction = (x * u + y * v + z * w).Normalise
                    let visible = visibility (hit.SpawnRay direction) infinity medium
                    sum <- sum + occluder.MinIntensity + (1. - occluder.MinIntensity) * visible.Average
                colour * (sum / float occluder.Sampler.SampleCount)
            | _ -> colour

    let rec trace (ray: Ray) depth medium key =
        cancellation.ThrowIfCancellationRequested()
        let hit = query.Closest(ray, 0., infinity)
        if not hit.DidHit then
            let _, filter = mediumProperties medium
            let escaped = background ray.GetDirection (depth = scene.MaxBounces) * attenuation filter infinity
            match fog with
            | Some fog when List.isEmpty medium.Stack ->
                let fogged = fog.ApplyEscaped(ray.GetOrigin, ray.GetDirection.Normalise, escaped)
                if fog.HasVolume then
                    fog.ApplyVolume(ray.GetOrigin, ray.GetDirection.Normalise, infinity, ray.ShutterTime, key,
                                    scene.MaxBounces - depth, volumeVisibility, fogged)
                else fogged
            | _ -> escaped
        else
            let medium = incomingMedium medium hit
            let _, filter = mediumProperties medium
            let segmentAttenuation = attenuation filter (hit.Time * ray.GetDirection.Magnitude)
            let mutable local = MaterialTransport.emission hit + ambient hit (mixKey (key ^^^ 0xa0UL)) medium
            if not (hit.Material :? TransparentMaterial || hit.Material :? EmissiveMaterial) then
                let lightContribution lightIndex =
                    let light = lights.[lightIndex]
                    let count = LightSampling.sampleCount light
                    let lightKey = mixKey (key ^^^ (uint64 lightIndex + 0x10000UL))
                    let mutable contribution = Colour.Black
                    for sampleIndex = 0 to count - 1 do
                        let sample = LightSampling.sampleAt light hit lightKey sampleIndex
                        let unoccluded = MaterialTransport.direct hit sample.Direction sample.Radiance sample.Weight
                        if not unoccluded.IsBlack then
                            let shadowRay, maximum = shadowSegment hit sample
                            contribution <- contribution + unoccluded * visibility shadowRay maximum medium
                    contribution / float count
                let always = selection.Always
                for k = 0 to always.Length - 1 do
                    local <- local + lightContribution always.[k]
                if not selection.IsExhaustive then
                    let u, _ = sample2D (mixKey (key ^^^ 0x5E1EC7EDUL)) 0
                    let struct (index, probability) = selection.Select(hit.Point, hit.Normal, false, u)
                    if index >= 0 && probability > 0. then
                        local <- local + lightContribution index * (1. / probability)
            if depth > 0 && hit.Material.IsRecursive then
                let etaI, etaT, transmittedMedium =
                    match hit.Material with
                    | :? TransparentMaterial as material -> crossBoundary medium hit material
                    | _ ->
                        let eta, _ = mediumProperties medium
                        eta, eta, medium
                let children = MaterialTransport.scatter hit (mixKey (key ^^^ 0xb0UL)) etaI etaT
                for index = 0 to children.Length - 1 do
                    let child = children.[index]
                    if not child.Weight.IsBlack then
                        let nextMedium = if child.Transmitted then transmittedMedium else medium
                        let childKey = mixKey (key ^^^ (uint64 index + 0xc0000UL))
                        local <- local + child.Weight * trace child.Ray (depth - 1) nextMedium childKey
            let shaded = local * segmentAttenuation
            match fog with
            | Some fog when List.isEmpty medium.Stack ->
                let distance = hit.Time * ray.GetDirection.Magnitude
                let fogged = fog.Apply(ray.GetOrigin, ray.GetDirection.Normalise, distance, shaded)
                if fog.HasVolume then
                    fog.ApplyVolume(ray.GetOrigin, ray.GetDirection.Normalise, distance, ray.ShutterTime, key,
                                    scene.MaxBounces - depth, volumeVisibility, fogged)
                else fogged
            | _ -> shaded

    member _.Trace(ray: Ray, key: uint64) =
        trace ray scene.MaxBounces (mediumAtOrigin ray.GetOrigin) key

    interface IIntegrator with
        member this.Trace(ray, key) = this.Trace(ray, key)
        // The Whitted integrator has no denoising path: it is deterministic, so
        // there is no Monte Carlo noise for a denoiser to remove.
        member this.TraceWithGuides(ray, key) = struct (this.Trace(ray, key), Colour.Black, Vector.Zero)

    member _.Visibility(ray: Ray, maximum: float) =
        visibility ray maximum (mediumAtOrigin ray.GetOrigin)
