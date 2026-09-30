namespace Tracer.Basics

module RegularGrids =
    open System
    open AccelerationCommon

    let debugBuildCounts = false
    type RGStructure = int list[,,] * int * int * int * BBox
    let mutable gridSize = (0, 0, 0)

    let clamp (value: float, maximum: int) =
        if Double.IsNaN value || value <= 0. || maximum <= 0 then 0.
        elif value >= float maximum then float maximum
        else floor value

    let calcEdgeLength wx wy wz n =
        if wx <= 0. || wy <= 0. || wz <= 0. || n <= 0. then 0.
        else exp ((log wx + log wy + log wz - log n) / 3.)

    let calcAxisCell m w s =
        if w <= 0. || s <= 0. || not (Double.IsFinite s) then 1.
        else floor (m * w / s) + 1.

    let calcAxisCells wx wy wz m n =
        let widths = [| wx; wy; wz |]
        let maximum = widths |> Array.fold (fun acc w -> if Double.IsFinite w then max acc w else acc) 0.
        let normalized = widths |> Array.map (fun w -> if w > 0. && maximum > 0. && Double.IsFinite w then w / maximum else 0.)
        let active = normalized |> Array.filter (fun w -> w > 0.)
        let cells = [| 1; 1; 1 |]
        if n > 0 && active.Length > 0 then
            let multiplier = if Double.IsFinite m && m > 0. then m else 2.
            let target = min 262144. (max 1. (float n * pown multiplier active.Length))
            let density = (log target - Array.sumBy log active) / float active.Length
            for axis = 0 to 2 do
                if normalized.[axis] > 0. then
                    cells.[axis] <- max 1 (int (min 128. (floor (exp (log normalized.[axis] + density)) + 1.)))
            let budget = min 262144L (max 8L (int64 n * 8L))
            while int64 cells.[0] * int64 cells.[1] * int64 cells.[2] > budget do
                let axis = if cells.[0] >= cells.[1] && cells.[0] >= cells.[2] then 0 elif cells.[1] >= cells.[2] then 1 else 2
                cells.[axis] <- max 1 ((cells.[axis] + 1) / 2)
        cells.[0], cells.[1], cells.[2]

    let findOuterBoundingBoxLowHighPoints boxes = BVH.findOuterBoundingBoxLowHighPoints boxes
    let convertShapesToBBoxes shapes = BVH.convertShapesToBBoxes shapes
    let calcBbox n w = if w > 0. && Double.IsFinite w then float n / w else 0.

    let private scaledCell coordinate low high count =
        let width = high * 0.5 - low * 0.5
        if count <= 1 || width <= 0. then 0.
        else (coordinate * 0.5 - low * 0.5) / width * float count

    let private singleCell indices (bounds: Bounds) : RGStructure =
        let grid = Array3D.create 1 1 1 (Array.toList indices)
        grid, 1, 1, 1, bounds.ToBBox()

    let internal buildBounded (boxes: Bounds array) (indices: int array) : RGStructure =
        let bounds = if indices.Length = 0 then ofBBox (BBox(Point.Zero, Point.Zero)) else rangeBounds boxes indices 0 indices.Length
        let wx, wy, wz = bounds.MaxX - bounds.MinX, bounds.MaxY - bounds.MinY, bounds.MaxZ - bounds.MinZ
        if indices.Length < 10 || not (Double.IsFinite wx && Double.IsFinite wy && Double.IsFinite wz) then singleCell indices bounds
        else
            let nx, ny, nz = calcAxisCells wx wy wz 2. indices.Length
            let ranges =
                indices |> Array.map (fun index ->
                    let box = boxes.[index]
                    let low value minimum maximum count = int (clamp(Math.BitDecrement(scaledCell value minimum maximum count), count - 1))
                    let high value minimum maximum count = int (clamp(Math.BitIncrement(scaledCell value minimum maximum count), count - 1))
                    struct (low box.MinX bounds.MinX bounds.MaxX nx, low box.MinY bounds.MinY bounds.MaxY ny, low box.MinZ bounds.MinZ bounds.MaxZ nz,
                            high box.MaxX bounds.MinX bounds.MaxX nx, high box.MaxY bounds.MinY bounds.MaxY ny, high box.MaxZ bounds.MinZ bounds.MaxZ nz))
            let references =
                ranges |> Array.sumBy (fun struct (lx, ly, lz, hx, hy, hz) -> int64 (hx - lx + 1) * int64 (hy - ly + 1) * int64 (hz - lz + 1))
            let budget = min 16777216L (max (int64 indices.Length * 32L) (int64 nx * int64 ny * int64 nz * 4L))
            if references > budget then singleCell indices bounds
            else
                let grid = Array3D.create nx ny nz []
                for position = 0 to indices.Length - 1 do
                    let struct (lx, ly, lz, hx, hy, hz) = ranges.[position]
                    for z = lz to hz do
                        for y = ly to hy do
                            for x = lx to hx do
                                grid.[x, y, z] <- indices.[position] :: grid.[x, y, z]
                if debugBuildCounts then gridSize <- nx, ny, nz
                grid, nx, ny, nz, bounds.ToBBox()

    let buildStructure (shapes: Shape array) =
        let boxes = cacheBounds shapes
        let finite, fallback = partitionBounds boxes
        if fallback.Length > 0 then singleCell [| 0 .. shapes.Length - 1 |] unbounded
        else buildBounded boxes finite

    let build shapes = buildStructure shapes

    let calcIxIyIz (point: Point) (box: BBox) nx ny nz =
        int (clamp(scaledCell point.X box.lowPoint.X box.highPoint.X nx, nx - 1)),
        int (clamp(scaledCell point.Y box.lowPoint.Y box.highPoint.Y ny, ny - 1)),
        int (clamp(scaledCell point.Z box.lowPoint.Z box.highPoint.Z nz, nz - 1))

    let calcNextStepStop direction start index delta count =
        if direction < 0. then start + float (count - index) * delta, -1, -1
        elif direction > 0. then start + float (index + 1) * delta, 1, count
        else infinity, 0, -1

    let closestHit indices ray shapes =
        let result = indices |> List.fold (fun acc index -> consider shapes index ray 0. infinity acc) (noCandidate infinity)
        if result.Found then Some result.Hit else None

    let internal query (structure: RGStructure) ray data shapes minimum maximum initial stopAtFirst =
        let grid, nx, ny, nz, box = structure
        let bounds = ofBBox box
        let mutable result = initial
        let mutable stopped = false
        let visit indices =
            let mutable remaining = indices
            while not stopped && not (List.isEmpty remaining) do
                result <- consider shapes (List.head remaining) ray minimum maximum result
                stopped <- stopAtFirst && result.Found
                remaining <- List.tail remaining
        if minimum < maximum then
            match intersect bounds data minimum result.Distance with
            | ValueNone -> ()
            | ValueSome(struct (entry, exit)) ->
                if nx = 1 && ny = 1 && nz = 1 then visit grid.[0, 0, 0]
                else
                    let x = Math.FusedMultiplyAdd(entry, data.DX, data.X)
                    let y = Math.FusedMultiplyAdd(entry, data.DY, data.Y)
                    let z = Math.FusedMultiplyAdd(entry, data.DZ, data.Z)
                    if not (Double.IsFinite x && Double.IsFinite y && Double.IsFinite z) then
                        for x = 0 to nx - 1 do
                            for y = 0 to ny - 1 do
                                for z = 0 to nz - 1 do
                                    if not stopped then visit grid.[x, y, z]
                    else
                        let firstX, firstY, firstZ = calcIxIyIz (Point(x, y, z)) box nx ny nz
                        let axisStep low high count index origin direction =
                            if direction = 0. || high = low then infinity, infinity, 0
                            else
                                let width = (high - low) / float count
                                let boundary =
                                    if direction > 0. then
                                        if index + 1 = count then high else low + float (index + 1) * width
                                    elif index = 0 then low
                                    else low + float index * width
                                max entry (boundaryTime boundary origin direction), width / abs direction, (if direction > 0. then 1 else -1)
                        let tx, dx, sx = axisStep bounds.MinX bounds.MaxX nx firstX data.X data.DX
                        let ty, dy, sy = axisStep bounds.MinY bounds.MaxY ny firstY data.Y data.DY
                        let tz, dz, sz = axisStep bounds.MinZ bounds.MaxZ nz firstZ data.Z data.DZ
                        let mutable ix, iy, iz = firstX, firstY, firstZ
                        let mutable nextX, nextY, nextZ = tx, ty, tz
                        while not stopped && ix >= 0 && ix < nx && iy >= 0 && iy < ny && iz >= 0 && iz < nz do
                            visit grid.[ix, iy, iz]
                            let next = min nextX (min nextY nextZ)
                            if next > result.Distance || next > exit || next >= maximum || Double.IsPositiveInfinity next then stopped <- true
                            elif nextX <= nextY && nextX <= nextZ then
                                ix <- ix + sx
                                nextX <- nextX + dx
                            elif nextY <= nextZ then
                                iy <- iy + sy
                                nextY <- nextY + dy
                            else
                                iz <- iz + sz
                                nextZ <- nextZ + dz
        result

    let searchStructure structure shapes ray =
        let data = validateQuery ray 0. infinity
        let result = query structure ray data shapes 0. infinity (noCandidate infinity) false
        if result.Found then Some result.Hit else None

    let traverse structure (ray: Ray) shapes =
        match searchStructure structure shapes ray with
        | Some hit -> hit
        | None -> HitPoint ray
