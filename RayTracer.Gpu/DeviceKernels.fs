namespace Tracer.Gpu

open System
open ILGPU
open DeviceMath
open DeviceGeometry
open DeviceTransport

module DeviceKernels =
    let classifyCamera (index: Index1D) (scene: DeviceScene) (settings: DeviceSettings) (workspace: DeviceWorkspace) =
        if index.X = 0 then
            workspace.Errors.[0] <- 0
            workspace.InitialMediumCount.[0] <- 0
            for objectId = 0 to settings.ObjectCount - 1 do workspace.SeenObjects.[objectId] <- 0
            if settings.AllOpaque = 0 then
                let direction = normalize (v3 0.371f 0.529f 0.764f)
                let mutable ray = { Origin = settings.Camera.Position; Direction = direction }
                let mutable finished = false
                let mutable crossings = 0
                let mutable count = 0
                while not finished && workspace.Errors.[0] = 0 do
                    let hit = closest scene settings workspace 0 ray 0.f Single.PositiveInfinity 0
                    if hit.Triangle < 0 then finished <- true
                    else
                        crossings <- crossings + 1
                        if crossings > 2048 then workspace.Errors.[0] <- 8
                        let surface = surface scene ray hit
                        let material = scene.Materials.[surface.Material]
                        if material.Kind = 4 && workspace.SeenObjects.[surface.Object] = 0 then
                            workspace.SeenObjects.[surface.Object] <- 1
                            if surface.FrontFace = 0 then
                                if count >= settings.MediumCapacity then workspace.Errors.[0] <- 2
                                else
                                    scene.InitialMedia.[count] <- surface.Object
                                    count <- count + 1
                        ray <- spawn surface direction
                workspace.InitialMediumCount.[0] <- count

    let render (index: Index1D) (scene: DeviceScene) (settings: DeviceSettings)
               (workspace: DeviceWorkspace) (output: ArrayView<V3>) =
        let lane = index.X
        if lane < settings.BatchCount then
            workspace.Errors.[lane] <- 0
            let pixel = settings.BatchStart + lane
            let x = pixel % settings.Width
            let y = settings.Height - 1 - pixel / settings.Width
            let camera = settings.Camera
            let cameraKey = mixKey (uint64 (uint32 settings.Seed) ^^^ uint64 (y * settings.Width + x))
            let mutable colour = zero ()
            let mutable sampleIndex = 0
            while sampleIndex < settings.CameraSamples && workspace.Errors.[lane] = 0 do
                let random =
                    sample scene settings.CameraSampleOffset settings.CameraSamples settings.CameraSampleSets cameraKey sampleIndex
                let px = camera.PixelWidth * (float32 x - float32 settings.Width / 2.f + random.X)
                let py = camera.PixelHeight * (float32 y - float32 settings.Height / 2.f + random.Y)
                let direction =
                    normalize (sub (add (scale camera.V px) (scale camera.U py)) (scale camera.W camera.ViewDistance))
                let ray = { Origin = camera.Position; Direction = direction }
                let key = sampleKey settings.Seed (y * settings.Width + x) sampleIndex
                colour <- add colour (trace scene settings workspace lane ray key)
                sampleIndex <- sampleIndex + 1
            let averaged = scale colour (1.f / float32 settings.CameraSamples)
            if not (finite3 averaged) || averaged.X < 0.f || averaged.Y < 0.f || averaged.Z < 0.f then
                workspace.Errors.[lane] <- 5
            output.[lane] <- averaged
