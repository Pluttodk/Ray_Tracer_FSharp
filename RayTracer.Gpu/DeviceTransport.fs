namespace Tracer.Gpu

open System
open ILGPU.Algorithms
open DeviceMath
open DeviceGeometry

module DeviceTransport =
    let mediumIor (scene: DeviceScene) (workspace: DeviceWorkspace) (mediaBase: int) (count: int) =
        if count = 0 then 1.f
        else scene.Materials.[scene.ObjectMaterials.[workspace.Media.[mediaBase]]].Ior

    let mediumFilter (scene: DeviceScene) (workspace: DeviceWorkspace) (mediaBase: int) (count: int) =
        if count = 0 then one ()
        else scene.Materials.[scene.ObjectMaterials.[workspace.Media.[mediaBase]]].Filter

    let copyMedium (workspace: DeviceWorkspace) (source: int) (target: int) (count: int) =
        for index = 0 to count - 1 do workspace.Media.[target + index] <- workspace.Media.[source + index]

    let crossBoundary (scene: DeviceScene) (settings: DeviceSettings) (workspace: DeviceWorkspace)
                      (lane: int) (surface: DeviceSurface) (source: int) (target: int) (count: int) =
        let material = scene.Materials.[surface.Material]
        let etaI = mediumIor scene workspace source count
        if surface.FrontFace <> 0 then
            if count >= settings.MediumCapacity then
                workspace.Errors.[lane] <- 2
                { EtaI = etaI; EtaT = material.Ior; Count = count }
            else
                for index = count downto 1 do workspace.Media.[target + index] <- workspace.Media.[source + index - 1]
                workspace.Media.[target] <- surface.Object
                { EtaI = etaI; EtaT = material.Ior; Count = count + 1 }
        else
            let mutable remaining = 0
            for index = 0 to count - 1 do
                let objectId = workspace.Media.[source + index]
                if objectId <> surface.Object then
                    workspace.Media.[target + remaining] <- objectId
                    remaining <- remaining + 1
            { EtaI = if count = 0 then material.Ior else etaI
              EtaT = mediumIor scene workspace target remaining
              Count = remaining }

    let textureValue (scene: DeviceScene) (material: DeviceMaterial) (uv: V2) =
        if material.TextureWidth = 0 then one ()
        else
            let u = clamp01 uv.X
            let v = clamp01 uv.Y
            let x = XMath.Min(material.TextureWidth - 1, int (u * float32 material.TextureWidth))
            let y = XMath.Min(material.TextureHeight - 1, int ((1.f - v) * float32 material.TextureHeight))
            scene.TexturePixels.[material.TextureOffset + y * material.TextureWidth + x]

    let direct (material: DeviceMaterial) (colour: V3) (surface: DeviceSurface)
               (incoming: V3) (sample: DeviceLightSample) =
        let cosine = XMath.Max(0.f, dot surface.Normal sample.Direction)
        if cosine = 0.f || sample.Weight = 0.f || material.Kind = 4 || material.Kind = 5 then zero ()
        else
            let diffuse = scale colour (material.Diffuse / float32 Math.PI)
            let mutable specular = zero ()
            if material.Kind <> 0 && material.Specular > 0.f then
                let reflection = add (neg sample.Direction) (scale surface.Normal (2.f * cosine))
                let specularCosine = XMath.Max(0.f, dot reflection (neg incoming))
                specular <- scale material.SpecularColour (material.Specular * powi specularCosine material.Exponent)
            scale (mul (add diffuse specular) sample.Radiance) (cosine * sample.Weight)

    let lightSample (scene: DeviceScene) (light: DeviceLight) (surface: DeviceSurface) key index =
        let mutable result =
            { Direction = light.Direction; Radiance = light.Radiance
              Distance = Single.PositiveInfinity; Weight = 1.f }
        if light.Kind = 0 then
            let difference = sub light.Position surface.Point
            let distance = magnitude difference
            result <-
                { result with Direction = normalize difference; Distance = distance
                              Weight = if distance > 0.f then 1.f else 0.f }
        elif light.Kind = 2 then
            let sample = sample scene light.SampleOffset light.SampleCount light.SampleSets key index
            let point = add light.Position (add (scale light.EdgeU sample.X) (scale light.EdgeV sample.Y))
            let difference = sub point surface.Point
            let distance = magnitude difference
            let direction = normalize difference
            let cosine = XMath.Max(0.f, dot light.Direction (neg direction))
            result <-
                { result with Direction = direction; Distance = distance
                              Weight = if distance > 0.f then cosine * light.Area / (distance * distance) else 0.f }
        elif light.Kind = 3 then
            let random = sample scene light.SampleOffset light.SampleCount light.SampleSets key index
            let h = hemisphere random 1.f
            let v = frameV surface.Normal
            let u = cross surface.Normal v
            let direction = normalize (add (add (scale u h.X) (scale v h.Y)) (scale surface.Normal h.Z))
            let cosine = XMath.Max(0.f, dot direction surface.Normal)
            result <- { result with Direction = direction; Weight = if cosine > 0.f then float32 Math.PI / cosine else 0.f }
        result

    let visibility (scene: DeviceScene) (settings: DeviceSettings) (workspace: DeviceWorkspace)
                   (lane: int) (ray: DeviceRay) maximum (activeBase: int) (shadowBase: int) (mediumCount: int) =
        if maximum <= 0.f then one ()
        elif settings.AllOpaque <> 0 then
            let hit = closest scene settings workspace lane ray 0.f maximum 1
            if hit.Triangle >= 0 then zero () else one ()
        else
            copyMedium workspace activeBase shadowBase mediumCount
            let mutable currentRay = ray
            let mutable remaining = maximum
            let mutable count = mediumCount
            let mutable transmittance = one ()
            let mutable finished = false
            let mutable crossings = 0
            while not finished && workspace.Errors.[lane] = 0 do
                let hit = closest scene settings workspace lane currentRay 0.f remaining 0
                let filter = mediumFilter scene workspace shadowBase count
                if hit.Triangle < 0 then
                    transmittance <- mul transmittance (attenuation filter remaining)
                    finished <- true
                else
                    crossings <- crossings + 1
                    if crossings > 2048 then workspace.Errors.[lane] <- 3
                    let travelled = hit.Time * magnitude currentRay.Direction
                    transmittance <- mul transmittance (attenuation filter travelled)
                    let surface = surface scene currentRay hit
                    let material = scene.Materials.[surface.Material]
                    if material.Kind <> 4 then
                        transmittance <- zero ()
                        finished <- true
                    else
                        let boundary = crossBoundary scene settings workspace lane surface shadowBase shadowBase count
                        let normal = if surface.FrontFace <> 0 then surface.Geometric else neg surface.Geometric
                        let transport = dielectric (normalize currentRay.Direction) normal boundary.EtaI boundary.EtaT
                        transmittance <- scale transmittance (1.f - transport.Fresnel)
                        let nextRay = spawn surface currentRay.Direction
                        let advanced = dot (sub nextRay.Origin currentRay.Origin) (normalize currentRay.Direction)
                        if advanced <= 0.f || not (finite advanced) then workspace.Errors.[lane] <- 4
                        remaining <- XMath.Max(0.f, remaining - advanced)
                        currentRay <- nextRay
                        count <- boundary.Count
                        if isBlack transmittance || remaining = 0.f then finished <- true
            transmittance

    let sampleVisibility (scene: DeviceScene) (settings: DeviceSettings) (workspace: DeviceWorkspace)
                         (lane: int) (surface: DeviceSurface) (sample: DeviceLightSample)
                         (activeBase: int) (shadowBase: int) (mediumCount: int) =
        let origin = offset surface sample.Direction
        let mutable ray = { Origin = origin; Direction = sample.Direction }
        let mutable maximum = sample.Distance
        if maximum <> Single.PositiveInfinity then
            let target = add surface.Point (scale sample.Direction sample.Distance)
            let delta = sub target origin
            let distance = magnitude delta
            let positionScale = XMath.Max(maxComponentAbs origin, maxComponentAbs target)
            let endpointMargin = 32.f * DeviceMath.Epsilon * positionScale
            ray <- { Origin = origin; Direction = normalize delta }
            // Match the CPU's one-ULP inward endpoint after its scale-aware margin.
            maximum <- XMath.Max(0.f, moveOffset (distance - endpointMargin) 0.f -1.f)
        visibility scene settings workspace lane ray maximum activeBase shadowBase mediumCount

    let localRadiance (scene: DeviceScene) (settings: DeviceSettings) (workspace: DeviceWorkspace)
                      (lane: int) (surface: DeviceSurface) (work: DeviceWork) (activeBase: int) (shadowBase: int) =
        let material = scene.Materials.[surface.Material]
        let texture = textureValue scene material surface.Uv
        let colour = mul material.Colour texture
        let mutable result = zero ()
        if material.Kind = 5 then
            if surface.FrontFace <> 0 then result <- scale colour material.Emission
        elif material.Kind <> 4 then
            let ambientColour = mul material.AmbientColour texture
            result <- scale (mul ambientColour settings.Ambient) material.Ambient
            for lightIndex = 0 to settings.LightCount - 1 do
                let light = scene.Lights.[lightIndex]
                let lightKey = mixKey (work.Key ^^^ (uint64 lightIndex + 0x10000UL))
                let mutable contribution = zero ()
                for sampleIndex = 0 to light.SampleCount - 1 do
                    let sample = lightSample scene light surface lightKey sampleIndex
                    if not (finite3 sample.Direction && finite3 sample.Radiance && finite sample.Weight) ||
                       sample.Distance <> sample.Distance || sample.Distance < 0.f then workspace.Errors.[lane] <- 5
                    let unoccluded = direct material colour surface (normalize work.Ray.Direction) sample
                    if not (isBlack unoccluded) then
                        let visible =
                            sampleVisibility scene settings workspace lane surface sample activeBase shadowBase work.MediumCount
                        contribution <- add contribution (mul unoccluded visible)
                result <- add result (scale contribution (1.f / float32 light.SampleCount))
        result

    let push (settings: DeviceSettings) (workspace: DeviceWorkspace) (lane: int) (rayBase: int) (mediaBase: int) (stackCount: int)
             (ray: DeviceRay) weight depth key (sourceMedium: int) (mediumCount: int) =
        if not (isBlack weight) && workspace.Errors.[lane] = 0 then
            if stackCount >= settings.RayCapacity then
                workspace.Errors.[lane] <- 7
                stackCount
            else
                workspace.Rays.[rayBase + stackCount] <-
                    { Ray = ray; Weight = weight; Key = key; Depth = depth; MediumCount = mediumCount }
                copyMedium workspace sourceMedium (mediaBase + stackCount * settings.MediumCapacity) mediumCount
                stackCount + 1
        else stackCount

    let trace (scene: DeviceScene) (settings: DeviceSettings) (workspace: DeviceWorkspace)
              lane (ray: DeviceRay) key =
        let rayBase = lane * settings.RayCapacity
        let mediaBase = lane * (settings.RayCapacity + 2) * settings.MediumCapacity
        let activeBase = mediaBase + settings.RayCapacity * settings.MediumCapacity
        let shadowBase = activeBase + settings.MediumCapacity
        let initialCount = workspace.InitialMediumCount.[0]
        for index = 0 to initialCount - 1 do workspace.Media.[mediaBase + index] <- scene.InitialMedia.[index]
        workspace.Rays.[rayBase] <-
            { Ray = ray; Weight = one (); Key = key; Depth = settings.MaxBounces; MediumCount = initialCount }
        let mutable stackCount = 1
        let mutable result = zero ()
        let mutable workCount = 0
        while stackCount > 0 && workspace.Errors.[lane] = 0 do
            stackCount <- stackCount - 1
            workCount <- workCount + 1
            if workCount > settings.MaximumRayWork then workspace.Errors.[lane] <- 6
            let work = workspace.Rays.[rayBase + stackCount]
            copyMedium workspace (mediaBase + stackCount * settings.MediumCapacity) activeBase work.MediumCount
            let hit = closest scene settings workspace lane work.Ray 0.f Single.PositiveInfinity 0
            let filter = mediumFilter scene workspace activeBase work.MediumCount
            if hit.Triangle < 0 then
                result <- add result (mul (mul work.Weight (attenuation filter Single.PositiveInfinity)) settings.Background)
            else
                let surface = surface scene work.Ray hit
                let segmentWeight = mul work.Weight (attenuation filter (hit.Time * magnitude work.Ray.Direction))
                let local = localRadiance scene settings workspace lane surface work activeBase shadowBase
                result <- add result (mul segmentWeight local)
                let material = scene.Materials.[surface.Material]
                if work.Depth > 0 && (material.Kind = 2 || material.Kind = 3 || material.Kind = 4) then
                    let incoming = normalize work.Ray.Direction
                    let childDepth = work.Depth - 1
                    if material.Kind = 4 then
                        let boundary = crossBoundary scene settings workspace lane surface activeBase shadowBase work.MediumCount
                        let normal = if surface.FrontFace <> 0 then surface.Geometric else neg surface.Geometric
                        let transport = dielectric incoming normal boundary.EtaI boundary.EtaT
                        if transport.Fresnel < 1.f then
                            let ratio = boundary.EtaI / boundary.EtaT
                            let weight = scale segmentWeight ((1.f - transport.Fresnel) * ratio * ratio)
                            let childIndex = if transport.Fresnel > 0.f then 1UL else 0UL
                            let childKey = mixKey (work.Key ^^^ (childIndex + 0xc0000UL))
                            stackCount <-
                                push settings workspace lane rayBase mediaBase stackCount (spawn surface transport.Direction)
                                     weight childDepth childKey shadowBase boundary.Count
                        if transport.Fresnel > 0.f then
                            let weight = scale segmentWeight transport.Fresnel
                            let childKey = mixKey (work.Key ^^^ 0xc0000UL)
                            stackCount <-
                                push settings workspace lane rayBase mediaBase stackCount (spawn surface (reflect incoming normal))
                                     weight childDepth childKey activeBase work.MediumCount
                    else
                        let reflected = reflect incoming surface.Normal
                        let count = if material.Kind = 3 then settings.GlossySamples else 1
                        let scatterKey = mixKey (work.Key ^^^ 0xb0UL)
                        let weight = scale (mul segmentWeight material.ReflectionColour) (material.Reflectivity / float32 count)
                        for childIndex = count - 1 downto 0 do
                            let mutable direction = reflected
                            if material.Kind = 3 then
                                let random =
                                    sample scene material.GlossySampleOffset count material.GlossySampleSets scatterKey childIndex
                                let h = hemisphere random (float32 material.GlossExponent)
                                let v = frameV reflected
                                let u = cross reflected v
                                let candidate = add (add (scale u h.X) (scale v h.Y)) (scale reflected h.Z)
                                direction <-
                                    if dot candidate surface.Normal > 0.f then candidate
                                    else add (add (scale u -h.X) (scale v -h.Y)) (scale reflected h.Z)
                            let childKey = mixKey (work.Key ^^^ (uint64 childIndex + 0xc0000UL))
                            stackCount <-
                                push settings workspace lane rayBase mediaBase stackCount (spawn surface direction)
                                     weight childDepth childKey activeBase work.MediumCount
        result
