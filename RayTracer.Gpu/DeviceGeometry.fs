namespace Tracer.Gpu

open System
open ILGPU.Algorithms
open DeviceMath

module DeviceGeometry =
    let boxTime (node: DeviceNode) (ray: DeviceRay) minimum maximum =
        let mutable near = minimum
        let mutable far = maximum
        let mutable valid = true
        for axis = 0 to 2 do
            let origin = axisValue ray.Origin axis
            let direction = axisValue ray.Direction axis
            let low = axisValue node.Low axis
            let high = axisValue node.High axis
            if direction = 0.f then
                if origin < low || origin > high then valid <- false
            else
                let a = (low - origin) / direction
                let b = (high - origin) / direction
                near <- XMath.Max(near, XMath.Min(a, b))
                far <- XMath.Min(far, XMath.Max(a, b))
        if valid && near <= far then near else Single.PositiveInfinity

    let shearVertex (point: V3) (ray: DeviceRay) kx ky kz sx sy sz =
        let depth = axisValue point kz - axisValue ray.Origin kz
        v3 (axisValue point kx - axisValue ray.Origin kx + sx * depth)
           (axisValue point ky - axisValue ray.Origin ky + sy * depth)
           (depth * sz)

    let edge (a: V3) (b: V3) =
        // Evaluate either orientation with identical operands before applying its sign.
        // GPU multiply/subtract contraction must not make both adjacent faces reject an edge.
        let forward = a.X < b.X || (a.X = b.X && a.Y < b.Y)
        let first = if forward then a else b
        let second = if forward then b else a
        let determinant = first.X * second.Y - first.Y * second.X
        if a.X = b.X && a.Y = b.Y then 0.f
        elif forward then determinant else -determinant

    let triangleHit (triangle: DeviceTriangle) triangleIndex (ray: DeviceRay) minimum maximum =
        let direction = ray.Direction
        let kz =
            if XMath.Abs direction.X >= XMath.Abs direction.Y && XMath.Abs direction.X >= XMath.Abs direction.Z then 0
            elif XMath.Abs direction.Y >= XMath.Abs direction.Z then 1 else 2
        let kx0 = (kz + 1) % 3
        let ky0 = (kz + 2) % 3
        let kx = if axisValue direction kz < 0.f then ky0 else kx0
        let ky = if axisValue direction kz < 0.f then kx0 else ky0
        let sx = -axisValue direction kx / axisValue direction kz
        let sy = -axisValue direction ky / axisValue direction kz
        let sz = 1.f / axisValue direction kz
        let a = shearVertex triangle.A ray kx ky kz sx sy sz
        let b = shearVertex triangle.B ray kx ky kz sx sy sz
        let c = shearVertex triangle.C ray kx ky kz sx sy sz
        let ea = edge b c
        let eb = edge c a
        let ec = edge a b
        let mutable result = { Time = maximum; Beta = 0.f; Gamma = 0.f; Triangle = -1 }
        if not ((ea < 0.f || eb < 0.f || ec < 0.f) && (ea > 0.f || eb > 0.f || ec > 0.f)) then
            let determinant = ea + eb + ec
            if determinant <> 0.f then
                let time = (ea * a.Z + eb * b.Z + ec * c.Z) / determinant
                if time > minimum && time <= maximum && finite time then
                    result <- { Time = time; Beta = eb / determinant; Gamma = ec / determinant; Triangle = triangleIndex }
        result

    let closest (scene: DeviceScene) (settings: DeviceSettings) (workspace: DeviceWorkspace)
                lane (ray: DeviceRay) minimum maximum anyHit =
        let mutable result = { Time = maximum; Beta = 0.f; Gamma = 0.f; Triangle = -1 }
        let stackBase = lane * settings.TraversalCapacity
        let mutable count = 0
        let mutable finished = false
        let mutable localObject = -1
        let mutable localRay = ray
        if not (finite3 ray.Origin && finite3 ray.Direction) || isBlack ray.Direction then workspace.Errors.[lane] <- 5
        if settings.NodeCount > 0 then
            workspace.Traversal.[stackBase] <- 0
            count <- 1
        while count > 0 && not finished && workspace.Errors.[lane] = 0 do
            count <- count - 1
            let node = scene.Nodes.[workspace.Traversal.[stackBase + count]]
            if boxTime node ray minimum result.Time < Single.PositiveInfinity then
                if node.Count > 0 then
                    let mutable leafIndex = 0
                    while leafIndex < node.Count && not finished do
                        let triangleIndex = scene.PrimitiveIndices.[node.Start + leafIndex]
                        let triangle = scene.Triangles.[triangleIndex]
                        if triangle.Object <> localObject then
                            let inverse = scene.WorldToObject.[triangle.Object]
                            localRay <-
                                { Origin = transformPoint inverse ray.Origin
                                  Direction = transformVector inverse ray.Direction }
                            localObject <- triangle.Object
                            if not (finite3 localRay.Origin && finite3 localRay.Direction) || isBlack localRay.Direction then
                                workspace.Errors.[lane] <- 5
                        // Keep the transformed direction unnormalized so t remains in world-ray units.
                        let hit = triangleHit triangle triangleIndex localRay minimum result.Time
                        if hit.Triangle >= 0 && hit.Time < maximum &&
                           (hit.Time < result.Time || result.Triangle < 0 || hit.Triangle < result.Triangle) then
                            result <- hit
                            if anyHit <> 0 then finished <- true
                        leafIndex <- leafIndex + 1
                else
                    let leftTime = boxTime scene.Nodes.[node.Left] ray minimum result.Time
                    let rightTime = boxTime scene.Nodes.[node.Right] ray minimum result.Time
                    let leftValid = leftTime < Single.PositiveInfinity
                    let rightValid = rightTime < Single.PositiveInfinity
                    if leftValid && rightValid then
                        if count + 2 > settings.TraversalCapacity then workspace.Errors.[lane] <- 1
                        else
                            let leftFirst = leftTime <= rightTime
                            workspace.Traversal.[stackBase + count] <- if leftFirst then node.Right else node.Left
                            workspace.Traversal.[stackBase + count + 1] <- if leftFirst then node.Left else node.Right
                            count <- count + 2
                    elif leftValid || rightValid then
                        if count + 1 > settings.TraversalCapacity then workspace.Errors.[lane] <- 1
                        else
                            workspace.Traversal.[stackBase + count] <- if leftValid then node.Left else node.Right
                            count <- count + 1
        result

    let surface (scene: DeviceScene) (ray: DeviceRay) (hit: DeviceHit) =
        let triangle = scene.Triangles.[hit.Triangle]
        let alpha = 1.f - hit.Beta - hit.Gamma
        let point = add ray.Origin (scale ray.Direction hit.Time)
        let geometric = triangle.GeometricNormal
        let interpolated =
            normalize (add (add (scale triangle.NormalA alpha) (scale triangle.NormalB hit.Beta))
                           (scale triangle.NormalC hit.Gamma))
        let shading = if isBlack interpolated then geometric else interpolated
        let aligned = if dot shading geometric < 0.f then neg shading else shading
        let normal = if dot ray.Direction aligned > 0.f then neg aligned else aligned
        let errorScale =
            XMath.Max(maxComponentAbs point, maxComponentAbs ray.Origin)
        { Point = point; Geometric = geometric; Normal = normal
          Uv = interpolateUv triangle.UvA triangle.UvB triangle.UvC hit.Beta hit.Gamma
          // 128 ULP of FP32, not of FP64 - see Epsilon in DeviceMath.
          OffsetDistance = 128.f * DeviceMath.Epsilon * errorScale
          Material = triangle.Material; Object = triangle.Object
          FrontFace = if dot ray.Direction geometric < 0.f then 1 else 0 }
