namespace Tracer.SceneAssets

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Text

[<Struct>]
type Vertex =
    { Position: V3
      Normal: V3
      U: float
      V: float }

type Mesh =
    { Id: string
      Recipe: string
      Smooth: bool
      Vertices: Vertex array
      Triangles: (int * int * int) array }

[<CLIMutable>]
type Topology =
    { Vertices: int
      Triangles: int
      Edges: int
      Components: int
      EulerCharacteristic: int
      Closed: bool
      ConsistentWinding: bool
      SignedVolume: float
      BoundsMin: float array
      BoundsMax: float array }

module Meshes =
    let private tau = 2. * Math.PI
    let private require condition message =
        if not condition then raise (InvalidDataException message)

    let private volume (positions: V3 array) (triangles: (int * int * int) array) =
        triangles
        |> Array.sumBy (fun (a, b, c) -> V3.dot positions.[a] (V3.cross positions.[b] positions.[c]) / 6.)

    let private finish id recipe smooth (positions: V3 array) (uv: (float * float) array) triangles =
        let faces =
            if volume positions triangles < 0. then
                triangles |> Array.map (fun (a, b, c) -> a, c, b)
            else triangles
        let normals = Array.create positions.Length V3.zero
        for a, b, c in faces do
            let normal = V3.cross (V3.sub positions.[b] positions.[a]) (V3.sub positions.[c] positions.[a])
            for index in [| a; b; c |] do
                normals.[index] <- V3.add normals.[index] normal
        { Id = id; Recipe = recipe; Smooth = smooth
          Vertices =
            Array.mapi (fun i position ->
                let u, v = uv.[i]
                { Position = position; Normal = V3.unit normals.[i]; U = u; V = v }) positions
          Triangles = faces }

    let validate (mesh: Mesh) =
        let fail message = $"{mesh.Id}: {message}"
        require (mesh.Vertices.Length >= 4 && mesh.Triangles.Length >= 4) (fail "mesh is empty or not a solid.")
        let edgeFaces = Dictionary<struct (int * int), ResizeArray<struct (int * int)>>()
        let incidentFaces = Array.init mesh.Vertices.Length (fun _ -> ResizeArray<int>())
        let positions = mesh.Vertices |> Array.map _.Position
        for vertex in mesh.Vertices do
            require (V3.finite vertex.Position && V3.finite vertex.Normal) (fail "non-finite vertex/normal.")
            require (abs (V3.length vertex.Normal - 1.) < 1e-6) (fail "non-unit normal.")
            require (Double.IsFinite vertex.U && Double.IsFinite vertex.V) (fail "non-finite texture coordinate.")
            require (vertex.U >= 0. && vertex.U <= 1. && vertex.V >= 0. && vertex.V <= 1.) (fail "UV outside [0,1].")
        for faceIndex in 0 .. mesh.Triangles.Length - 1 do
            let a, b, c = mesh.Triangles.[faceIndex]
            require ([| a; b; c |] |> Array.forall (fun i -> i >= 0 && i < positions.Length)) (fail "index outside vertex array.")
            require (a <> b && b <> c && c <> a) (fail "repeated triangle index.")
            let normal = V3.cross (V3.sub positions.[b] positions.[a]) (V3.sub positions.[c] positions.[a])
            require (V3.length normal > 1e-12) (fail $"degenerate triangle {faceIndex}.")
            let shadingNormal = V3.add mesh.Vertices.[a].Normal (V3.add mesh.Vertices.[b].Normal mesh.Vertices.[c].Normal)
            require (V3.dot normal shadingNormal > 0.) (fail $"inverted shading normals on face {faceIndex}.")
            for index in [| a; b; c |] do incidentFaces.[index].Add faceIndex
            for first, second in [| a, b; b, c; c, a |] do
                let edge, direction = if first < second then struct (first, second), 1 else struct (second, first), -1
                let faces =
                    match edgeFaces.TryGetValue edge with
                    | true, value -> value
                    | _ ->
                        let value = ResizeArray()
                        edgeFaces.Add(edge, value)
                        value
                faces.Add(struct (faceIndex, direction))
        let neighbours = Array.init mesh.Triangles.Length (fun _ -> ResizeArray<int>())
        for KeyValue(edge, faces) in edgeFaces do
            require (faces.Count = 2) (fail $"edge {edge} has {faces.Count} incident faces, expected 2.")
            let struct (first, firstDirection), struct (second, secondDirection) = faces.[0], faces.[1]
            require (firstDirection + secondDirection = 0) (fail $"inconsistent winding at edge {edge}.")
            neighbours.[first].Add second
            neighbours.[second].Add first
        // A closed edge test alone misses two disjoint surface fans touching at one vertex.
        for vertex in 0 .. incidentFaces.Length - 1 do
            let fan = incidentFaces.[vertex]
            require (fan.Count > 0) (fail $"unused vertex {vertex}.")
            let allowed = HashSet<int>(fan)
            let visited = HashSet<int>()
            let pending = Stack<int>()
            pending.Push fan.[0]
            while pending.Count > 0 do
                let current = pending.Pop()
                if visited.Add current then
                    for neighbour in neighbours.[current] do
                        if allowed.Contains neighbour then pending.Push neighbour
            require (visited.Count = fan.Count) (fail $"non-manifold vertex fan {vertex}.")
        let visited = HashSet<int>()
        let mutable components = 0
        for face in 0 .. mesh.Triangles.Length - 1 do
            if not (visited.Contains face) then
                components <- components + 1
                let pending = Stack<int>()
                pending.Push face
                while pending.Count > 0 do
                    let current = pending.Pop()
                    if visited.Add current then
                        for neighbour in neighbours.[current] do pending.Push neighbour
        require (components = 1) (fail "each reusable mesh must be a single connected solid.")
        let signedVolume = volume positions mesh.Triangles
        require (Double.IsFinite signedVolume && signedVolume > 1e-10) (fail "solid is not outward-oriented or has zero volume.")
        let coordinateMin selector = positions |> Array.minBy selector |> selector
        let coordinateMax selector = positions |> Array.maxBy selector |> selector
        { Vertices = positions.Length; Triangles = mesh.Triangles.Length; Edges = edgeFaces.Count
          Components = components; EulerCharacteristic = positions.Length - edgeFaces.Count + mesh.Triangles.Length
          Closed = true; ConsistentWinding = true; SignedVolume = signedVolume
          BoundsMin = [| coordinateMin _.X; coordinateMin _.Y; coordinateMin _.Z |]
          BoundsMax = [| coordinateMax _.X; coordinateMax _.Y; coordinateMax _.Z |] }

    let private number (value: float) =
        let rounded = Math.Round(value, 9, MidpointRounding.ToEven)
        if rounded = 0. then "0" else rounded.ToString("0.#########", CultureInfo.InvariantCulture)

    let plyTextWithProvenance provenance mesh =
        require (not (String.IsNullOrWhiteSpace provenance) && not (provenance.Contains '\n') && not (provenance.Contains '\r'))
            "PLY provenance reference must be a nonempty single line."
        validate mesh |> ignore
        let text = StringBuilder()
        let line (value: string) = text.Append(value).Append('\n') |> ignore
        line "ply"
        line "format ascii 1.0"
        line $"comment Original procedural asset; GPL-2.0-only; see {provenance}"
        line $"element vertex {mesh.Vertices.Length}"
        for property in [ "x"; "y"; "z"; "nx"; "ny"; "nz"; "u"; "v" ] do
            line $"property float {property}"
        line $"element face {mesh.Triangles.Length}"
        line "property list uchar int vertex_indices"
        line "end_header"
        for v in mesh.Vertices do
            [| v.Position.X; v.Position.Y; v.Position.Z; v.Normal.X; v.Normal.Y; v.Normal.Z; v.U; v.V |]
            |> Array.map number |> String.concat " " |> line
        for a, b, c in mesh.Triangles do line $"3 {a} {b} {c}"
        text.ToString()

    let plyText mesh = plyTextWithProvenance "benchmarks/scenes/provenance.json" mesh

    let readPly id recipe smooth (path: string) =
        let lines = File.ReadAllLines path
        require (File.ReadAllBytes(path) |> Array.forall ((<>) 13uy)) $"{path}: PLY must use LF, not CRLF."
        require (lines.Length > 14 && lines.[0] = "ply" && lines.[1] = "format ascii 1.0") $"{path}: unexpected PLY format."
        let headerEnd = Array.findIndex ((=) "end_header") lines
        let element name =
            let prefix = $"element {name} "
            let line = lines.[0 .. headerEnd] |> Array.find (fun line -> line.StartsWith(prefix, StringComparison.Ordinal))
            Int32.Parse(line.Substring prefix.Length, CultureInfo.InvariantCulture)
        let vertexCount, faceCount = element "vertex", element "face"
        let propertyLines = lines.[0 .. headerEnd] |> Array.filter (fun line -> line.StartsWith("property ", StringComparison.Ordinal))
        let expected =
            [| for name in [ "x"; "y"; "z"; "nx"; "ny"; "nz"; "u"; "v" ] do yield $"property float {name}"
               yield "property list uchar int vertex_indices" |]
        require (propertyLines = expected) $"{path}: PLY property contract changed."
        require (lines.Length = headerEnd + 1 + vertexCount + faceCount) $"{path}: incorrect element counts."
        let parseFloat (value: string) = Double.Parse(value, CultureInfo.InvariantCulture)
        let vertices =
            Array.init vertexCount (fun i ->
                let values = lines.[headerEnd + 1 + i].Split ' ' |> Array.map parseFloat
                require (values.Length = 8) $"{path}: expected eight vertex components."
                { Position = V3.create values.[0] values.[1] values.[2]; Normal = V3.create values.[3] values.[4] values.[5]
                  U = values.[6]; V = values.[7] })
        let triangles =
            Array.init faceCount (fun i ->
                let values = lines.[headerEnd + 1 + vertexCount + i].Split ' ' |> Array.map (fun value -> Int32.Parse(value, CultureInfo.InvariantCulture))
                require (values.Length = 4 && values.[0] = 3) $"{path}: expected triangulated faces."
                values.[1], values.[2], values.[3])
        { Id = id; Recipe = recipe; Smooth = smooth; Vertices = vertices; Triangles = triangles }

    let sphere id segments latitudeCount mapper =
        let positions, uv, triangles = ResizeArray<V3>(), ResizeArray<float * float>(), ResizeArray<int * int * int>()
        let add p u v = positions.Add(mapper p); uv.Add(u, v)
        add (V3.create 0. 1. 0.) 0.5 1.
        for row in 1 .. latitudeCount - 1 do
            let theta = Math.PI * float row / float latitudeCount
            for column in 0 .. segments - 1 do
                let phi = tau * float column / float segments
                add (V3.create (sin theta * sin phi) (cos theta) (sin theta * cos phi)) (float column / float segments) (1. - float row / float latitudeCount)
        let bottom = positions.Count
        add (V3.create 0. -1. 0.) 0.5 0.
        let index row column = 1 + row * segments + (column % segments)
        for column in 0 .. segments - 1 do triangles.Add(0, index 0 column, index 0 (column + 1))
        for row in 0 .. latitudeCount - 3 do
            for column in 0 .. segments - 1 do
                let a, b, c, d = index row column, index row (column + 1), index (row + 1) (column + 1), index (row + 1) column
                triangles.Add(a, d, b)
                triangles.Add(b, d, c)
        for column in 0 .. segments - 1 do
            triangles.Add(index (latitudeCount - 2) column, bottom, index (latitudeCount - 2) (column + 1))
        finish id $"UV sphere deformation; {segments} meridians, {latitudeCount} latitudes; shared seam and pole indices" true (positions.ToArray()) (uv.ToArray()) (triangles.ToArray())

    let loft id recipe smooth segments (profile: (float * float * float * float * float) array) =
        let positions, uv, triangles = ResizeArray<V3>(), ResizeArray<float * float>(), ResizeArray<int * int * int>()
        let rings =
            profile |> Array.mapi (fun row (y, rx, rz, cx, cz) ->
                let count = if rx = 0. && rz = 0. then 1 else segments
                Array.init count (fun column ->
                    let phi = tau * float column / float segments
                    let index = positions.Count
                    positions.Add(V3.create (cx + rx * sin phi) y (cz + rz * cos phi))
                    uv.Add(float column / float segments, float row / float (profile.Length - 1))
                    index))
        for row in 0 .. rings.Length - 2 do
            let lower, upper = rings.[row], rings.[row + 1]
            for column in 0 .. segments - 1 do
                let next = (column + 1) % segments
                if lower.Length = 1 then triangles.Add(lower.[0], upper.[next], upper.[column])
                elif upper.Length = 1 then triangles.Add(lower.[column], lower.[next], upper.[0])
                else
                    triangles.Add(lower.[column], lower.[next], upper.[column])
                    triangles.Add(upper.[column], lower.[next], upper.[next])
        finish id recipe smooth (positions.ToArray()) (uv.ToArray()) (triangles.ToArray())

    let lathe id segments profile =
        profile
        |> Array.map (fun (y, r) -> y, r, r, 0., 0.)
        |> loft id $"Capped surface of revolution; {segments} sectors; authored rounded profile" true segments

    let roundedBox id mapper =
        let grid = [| -1.; -0.92; -0.8; 0.; 0.8; 0.92; 1. |]
        let positions, uv, triangles = ResizeArray<V3>(), ResizeArray<float * float>(), ResizeArray<int * int * int>()
        let indices = Dictionary<struct (int64 * int64 * int64), int>()
        let project p =
            let clamp v = max -0.8 (min 0.8 v)
            let center = V3.create (clamp p.X) (clamp p.Y) (clamp p.Z)
            V3.add center (V3.sub p center |> V3.unit |> V3.scale 0.2)
        let vertex p =
            let key = struct (int64 (Math.Round(p.X * 1e9)), int64 (Math.Round(p.Y * 1e9)), int64 (Math.Round(p.Z * 1e9)))
            match indices.TryGetValue key with
            | true, index -> index
            | _ ->
                let index = positions.Count
                indices.Add(key, index)
                positions.Add(project p |> mapper)
                uv.Add((p.X + p.Z + 2.) / 4., (p.Y + 1.) / 2.)
                index
        let surfaces =
            [| (fun s t -> V3.create 1. t -s); (fun s t -> V3.create -1. t s)
               (fun s t -> V3.create s 1. -t); (fun s t -> V3.create s -1. t)
               (fun s t -> V3.create s t 1.); (fun s t -> V3.create -s t -1.) |]
        for surface in surfaces do
            let face = Array2D.init grid.Length grid.Length (fun i j -> vertex (surface grid.[i] grid.[j]))
            for i in 0 .. grid.Length - 2 do
                for j in 0 .. grid.Length - 2 do
                    let a, b, c, d = face.[i, j], face.[i + 1, j], face.[i + 1, j + 1], face.[i, j + 1]
                    triangles.Add(a, b, c)
                    triangles.Add(a, c, d)
        finish id "Six beveled cube grids with welded boundary indices; radius 0.2; optional authored deformation" true (positions.ToArray()) (uv.ToArray()) (triangles.ToArray())

    let extrusion id depth (outline: (float * float) array) =
        let cross (ax, ay) (bx, by) (cx, cy) = (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)
        let area =
            outline |> Array.mapi (fun i (x, y) ->
                let nx, ny = outline.[(i + 1) % outline.Length]
                x * ny - y * nx) |> Array.sum
        let outline = if area < 0. then Array.rev outline else outline
        let remaining = ResizeArray<int>([| 0 .. outline.Length - 1 |])
        let cap = ResizeArray<int * int * int>()
        while remaining.Count > 3 do
            let mutable found = false
            let mutable i = 0
            while not found && i < remaining.Count do
                let a, b, c = remaining.[(i + remaining.Count - 1) % remaining.Count], remaining.[i], remaining.[(i + 1) % remaining.Count]
                let inside p = cross outline.[a] outline.[b] p >= -1e-10 && cross outline.[b] outline.[c] p >= -1e-10 && cross outline.[c] outline.[a] p >= -1e-10
                if cross outline.[a] outline.[b] outline.[c] > 1e-10
                   && not (remaining |> Seq.exists (fun k -> k <> a && k <> b && k <> c && inside outline.[k])) then
                    cap.Add(a, b, c)
                    remaining.RemoveAt i
                    found <- true
                i <- i + 1
            require found $"{id}: extrusion outline is not a simple triangulable polygon."
        cap.Add(remaining.[0], remaining.[1], remaining.[2])
        let count = outline.Length
        let positions =
            [| for z in [ depth / 2.; -depth / 2. ] do
                   for x, y in outline do yield V3.create x y z |]
        let xmin, xmax = outline |> Array.map fst |> Array.min, outline |> Array.map fst |> Array.max
        let ymin, ymax = outline |> Array.map snd |> Array.min, outline |> Array.map snd |> Array.max
        let uv = positions |> Array.map (fun p -> (p.X - xmin) / (xmax - xmin), (p.Y - ymin) / (ymax - ymin))
        let triangles = ResizeArray<int * int * int>()
        for a, b, c in cap do
            triangles.Add(a, b, c)
            triangles.Add(a + count, c + count, b + count)
        for a in 0 .. count - 1 do
            let b = (a + 1) % count
            triangles.Add(a, a + count, b + count)
            triangles.Add(a, b + count, b)
        finish id "Original polygon, ear-clipped front/back caps and consistently wound closed side walls" false positions uv (triangles.ToArray())

    let solidGrid id columns rows thickness mapper =
        let width = columns + 1
        let perSide = width * (rows + 1)
        let positions =
            Array.init (perSide * 2) (fun index ->
                let side, local = index / perSide, index % perSide
                let u, v = float (local % width) / float columns, float (local / width) / float rows
                let p = mapper u v
                { p with Z = p.Z + (if side = 0 then thickness / 2. else -thickness / 2.) })
        let uv =
            Array.init positions.Length (fun index ->
                let local = index % perSide
                float (local % width) / float columns, float (local / width) / float rows)
        let triangles = ResizeArray<int * int * int>()
        for row in 0 .. rows - 1 do
            for column in 0 .. columns - 1 do
                let a = row * width + column
                let b, c, d = a + 1, a + width + 1, a + width
                triangles.Add(a, b, c)
                triangles.Add(a, c, d)
                triangles.Add(a + perSide, c + perSide, b + perSide)
                triangles.Add(a + perSide, d + perSide, c + perSide)
        let boundary =
            [| for column in 0 .. columns do yield column
               for row in 1 .. rows do yield row * width + columns
               for column in columns - 1 .. -1 .. 0 do yield rows * width + column
               for row in rows - 1 .. -1 .. 1 do yield row * width |]
        for index in 0 .. boundary.Length - 1 do
            let a, b = boundary.[index], boundary.[(index + 1) % boundary.Length]
            triangles.Add(a, a + perSide, b + perSide)
            triangles.Add(a, b + perSide, b)
        finish id $"Original thick draped surface; {columns} x {rows} cells on each side; sealed perimeter; thickness {thickness}" true positions uv (triangles.ToArray())

    let tube id closed sectors reference (points: V3 array) (radii: (float * float) array) =
        let positions, uv, triangles = ResizeArray<V3>(), ResizeArray<float * float>(), ResizeArray<int * int * int>()
        let count = points.Length
        for row in 0 .. count - 1 do
            let previous = if row > 0 then points.[row - 1] elif closed then points.[count - 1] else points.[0]
            let next = if row + 1 < count then points.[row + 1] elif closed then points.[0] else points.[count - 1]
            let tangent = V3.sub next previous |> V3.unit
            let normal = V3.sub reference (V3.scale (V3.dot reference tangent) tangent) |> V3.unit
            let binormal = V3.cross tangent normal |> V3.unit
            let rx, ry = radii.[row]
            for column in 0 .. sectors - 1 do
                let phi = tau * float column / float sectors
                let offset = V3.add (V3.scale (rx * cos phi) normal) (V3.scale (ry * sin phi) binormal)
                positions.Add(V3.add points.[row] offset)
                uv.Add(float column / float sectors, float row / float (if closed then count else count - 1))
        let index row column = row * sectors + column % sectors
        for row in 0 .. (if closed then count - 1 else count - 2) do
            let next = (row + 1) % count
            for column in 0 .. sectors - 1 do
                let a, b, c, d = index row column, index row (column + 1), index next (column + 1), index next column
                triangles.Add(a, b, d)
                triangles.Add(d, b, c)
        if not closed then
            let first, last = positions.Count, positions.Count + 1
            positions.Add points.[0]
            positions.Add points.[count - 1]
            uv.Add(0.5, 0.)
            uv.Add(0.5, 1.)
            for column in 0 .. sectors - 1 do
                triangles.Add(first, index 0 (column + 1), index 0 column)
                triangles.Add(last, index (count - 1) column, index (count - 1) (column + 1))
        let form = if closed then "closed-loop" else "capped"
        finish id $"Original {form} tube sweep; {count} sections, {sectors} radial samples" true (positions.ToArray()) (uv.ToArray()) (triangles.ToArray())

    let private sample count f = Array.init count (fun i -> f (float i / float (count - 1)))
    let private constantRadii count x y = Array.create count (x, y)

    let library () =
        let unitSphere = sphere "sphere" 32 20 id
        let smallSphere = sphere "small-sphere" 20 12 id
        let capsule =
            lathe "rounded-cylinder" 24
                [| -1., 0.; -0.99, 0.38; -0.96, 0.71; -0.91, 0.91; -0.83, 1.
                   0.83, 1.; 0.91, 0.91; 0.96, 0.71; 0.99, 0.38; 1., 0. |]
        let leg =
            lathe "tapered-leg" 24
                [| -1., 0.; -0.995, 0.50; -0.97, 0.75; -0.87, 0.77; 0.78, 0.99
                   0.93, 1.; 0.98, 0.84; 1., 0. |]
        let column =
            lathe "moulded-pedestal" 40
                [| -1., 0.; -1., 1.; -0.94, 1.; -0.90, 0.9; -0.80, 0.90; -0.72, 0.70
                   0.63, 0.66; 0.74, 0.76; 0.81, 0.92; 0.89, 0.92; 0.93, 1.; 1., 1.; 1., 0. |]
        let disc =
            lathe "disc" 48 [| -1., 0.; -1., 0.94; -0.88, 1.; 0.88, 1.; 1., 0.94; 1., 0. |]
        let torusPoints = Array.init 64 (fun i -> let t = tau * float i / 64. in V3.create (cos t) 0. (sin t))
        let torus = tube "ring" true 10 (V3.create 0. 1. 0.) torusPoints (constantRadii torusPoints.Length 0.07 0.07)
        let archPoints = sample 25 (fun t -> let angle = Math.PI * (0.08 + 0.84 * t) in V3.create (cos angle) (sin angle) 0.)
        let arch = tube "arch" false 12 (V3.create 0. 0. 1.) archPoints (constantRadii archPoints.Length 0.09 0.09)
        let curlPoints =
            sample 36 (fun t ->
                let angle, radius = tau * 1.15 * t, 0.95 - 0.60 * t
                V3.create (radius * cos angle) (radius * sin angle) (0.08 * t))
        let curl = tube "sculpted-curl" false 10 (V3.create 0. 0. 1.) curlPoints (sample 36 (fun t -> let r = 0.16 - 0.03 * t in r, r))
        let lidPoints = sample 17 (fun t -> V3.create (2. * t - 1.) (0.68 * sin (Math.PI * t)) (0.10 * sin (Math.PI * t)))
        let eyelid = tube "eyelid" false 8 (V3.create 0. 0. 1.) lidPoints (constantRadii lidPoints.Length 0.16 0.16)
        let railPoints = sample 25 (fun t -> let x = 2. * t - 1. in V3.create x (0.10 * (1. - x * x)) (-0.17 * (1. - x * x)))
        let rail = tube "chair-curved-rail" false 12 (V3.create 0. 1. 0.) railPoints (constantRadii railPoints.Length 0.095 0.095)
        let pipingPoints =
            Array.init 80 (fun i ->
                let t = tau * float i / 80.
                let power x = Math.CopySign(abs x ** 0.40, x)
                V3.create (0.69 * power (cos t)) 0. (0.60 * power (sin t)))
        let piping = tube "seat-piping" true 8 (V3.create 0. 1. 0.) pipingPoints (constantRadii pipingPoints.Length 0.012 0.012)
        let slat =
            roundedBox "chair-curved-slat" (fun p ->
                V3.create (p.X * 0.062) (p.Y * 0.50) (p.Z * 0.028 + 0.045 * (1. - p.Y * p.Y)))
        let head =
            sphere "classical-head" 56 40 (fun p ->
                let width = 0.86 - 0.15 * max 0. (-p.Y) + 0.02 * cos (p.Y * 5.)
                let x = p.X * width
                let front = max 0. p.Z
                let gaussian cx cy sx sy =
                    let dx, dy = (x - cx) / sx, (p.Y - cy) / sy
                    exp (-(dx * dx + dy * dy))
                let sockets = 0.15 * (gaussian -0.36 0.10 0.20 0.17 + gaussian 0.36 0.10 0.20 0.17)
                let cheeks = 0.09 * (gaussian -0.47 -0.18 0.24 0.21 + gaussian 0.47 -0.18 0.24 0.21)
                let muzzle = 0.08 * gaussian 0. -0.48 0.36 0.22
                let chin = 0.10 * gaussian 0. -0.76 0.30 0.19
                let brow = 0.07 * gaussian 0. 0.35 0.65 0.16
                V3.create x p.Y (p.Z * 0.76 + front * (cheeks + muzzle + chin + brow - sockets)))
        let nose =
            loft "classical-nose" "Original anatomical nasal bridge, alae and tip; elliptical closed loft" true 28
                [| -0.37, 0., 0., 0., 0.74; -0.33, 0.08, 0.10, 0., 0.82
                   -0.24, 0.16, 0.16, 0., 0.87; -0.14, 0.145, 0.20, 0., 0.88
                   0.03, 0.09, 0.15, 0., 0.83; 0.23, 0.067, 0.10, 0., 0.77
                   0.44, 0.04, 0.06, 0., 0.72; 0.50, 0., 0., 0., 0.68 |]
        let torso =
            loft "classical-shoulders" "Original tapered chest, deltoid shoulder silhouette and neck seat" true 48
                [| 0., 0., 0., 0., 0.; 0., 0.47, 0.28, 0., 0.; 0.12, 0.66, 0.35, 0., 0.
                   0.34, 0.91, 0.42, 0., 0.; 0.64, 1.12, 0.44, 0., 0.
                   0.78, 1.11, 0.43, 0., 0.; 0.94, 0.88, 0.37, 0., 0.
                   1.12, 0.49, 0.31, 0., 0.; 1.18, 0.28, 0.25, 0., 0.; 1.20, 0., 0., 0., 0. |]
        let toga =
            solidGrid "toga-drape" 40 24 0.045 (fun u v ->
                let x = 2. * u - 1.
                V3.create
                    (x * (0.78 + 0.25 * v))
                    (-0.55 + 0.92 * v - 0.22 * x + 0.10 * x * x)
                    (0.34 + 0.17 * (1. - x * x) + 0.064 * sin (x * 10. + v * 10.) * (1. - 0.32 * v)))
        let cape =
            solidGrid "sentinel-cape" 36 28 0.06 (fun u v ->
                let x = 2. * u - 1.
                V3.create
                    (x * (1.63 - 0.61 * v))
                    (0.45 + 2.47 * v + 0.11 * cos (x * 5.) * (1. - v) + 0.11 * x)
                    (-0.66 - 0.32 * (1. - v) + 0.14 * cos (x * 15.) * (1. - 0.45 * v)))
        let helmet =
            loft "sentinel-helmet" "Original elongated twelve-sector crown; no licensed or traced character reference" false 12
                [| -1., 0., 0., 0., 0.10; -0.93, 0.28, 0.34, 0., 0.08
                   -0.66, 0.65, 0.61, 0., 0.; -0.14, 0.91, 0.78, 0., 0.
                   0.42, 0.92, 0.78, 0., -0.02; 0.77, 0.69, 0.67, 0., -0.08
                   1.02, 0.34, 0.41, 0., -0.12; 1.15, 0., 0., 0., -0.17 |]
        let armorTorso =
            loft "armor-torso" "Original angular cuirass, broad shoulders and tapered waist" false 12
                [| 0., 0., 0., 0., 0.; 0., 0.48, 0.32, 0., 0.; 0.30, 0.63, 0.42, 0., 0.
                   0.80, 0.93, 0.51, 0., 0.; 1.23, 1.08, 0.48, 0., 0.
                   1.48, 0.73, 0.36, 0., 0.; 1.61, 0.30, 0.27, 0., 0.; 1.63, 0., 0., 0., 0. |]
        let visor =
            extrusion "sentinel-visor" 0.12
                [| -0.92, 0.21; -0.18, 0.13; 0., 0.19; 0.90, 0.30
                   0.76, -0.01; 0.15, -0.08; -0.10, -0.02; -0.86, 0. |]
        let facePlate =
            extrusion "sentinel-faceplate" 0.36
                [| -0.66, 0.50; 0.66, 0.50; 0.57, -0.21; 0.18, -0.70
                   -0.27, -0.64; -0.58, -0.16 |]
        let pectoral =
            extrusion "armor-panel" 0.22
                [| -0.86, 0.42; -0.44, 0.78; 0.52, 0.53; 0.90, 0.11; 0.24, -0.66; -0.58, -0.36 |]
        let fin =
            extrusion "sentinel-fin" 0.18
                [| -0.40, -0.85; 0.22, -0.80; 0.40, 0.75; 0.12, 1.17; -0.19, 0.63 |]
        let leaf =
            sphere "leaf" 20 14 (fun p ->
                V3.create (p.X * 0.43 * (1. - 0.25 * p.Y)) p.Y (p.Z * 0.16 + 0.14 * (1. - p.Y * p.Y)))
        let hairPoints = sample 20 (fun t -> V3.create (0.76 * t) (0.56 * sin (Math.PI * 0.70 * t)) (-0.27 * t))
        let hair =
            tube "swept-hair-lock" false 12 (V3.create 0. 0. 1.) hairPoints
                (sample 20 (fun t -> let r = 0.22 * (1. - t * t) + 0.008 in r * 0.70, r))
        let scarf =
            solidGrid "hero-scarf" 8 32 0.028 (fun u v ->
                V3.create
                    (2.05 * v)
                    ((u - 0.5) * (0.33 - 0.08 * v) + 0.20 * sin (tau * 0.72 * v))
                    (0.12 * sin (tau * 1.15 * v) + 0.05 * sin (u * tau + v * 6.)))
        let tunic =
            extrusion "tunic-panel" 0.10
                [| -0.78, 0.80; 0.24, 0.84; 0.73, 0.15; 0.64, -0.84; -0.30, -1.02; -0.81, -0.60 |]
        let rock =
            let sectors = 13
            let profile =
                [| -1.12, 0., 0., -0.19, 0.06; -0.86, 0.23, 0.28, -0.10, 0.02
                   -0.25, 0.57, 0.62, -0.04, -0.09; 0.32, 0.89, 0.91, 0.03, 0.
                   0.72, 1.07, 0.99, 0., 0.; 0.90, 0.97, 0.93, 0., 0.
                   0.92, 0., 0., 0., 0. |]
            let raw = loft "floating-rock" "Original faceted floating rock; deterministic angular displacement, seed 2026" false sectors profile
            let positions =
                raw.Vertices |> Array.mapi (fun i v ->
                    if abs v.Position.X < 1e-10 && abs v.Position.Z < 1e-10 then v.Position
                    else
                        let angle = atan2 v.Position.X v.Position.Z
                        let noise = 1. + 0.11 * sin (angle * 3. + 2.026) + 0.06 * cos (angle * 5. - v.Position.Y)
                        V3.create (v.Position.X * noise) v.Position.Y (v.Position.Z * noise))
            finish raw.Id raw.Recipe false positions (raw.Vertices |> Array.map (fun v -> v.U, v.V)) raw.Triangles
        let octagon =
            let mesh = lathe "octagonal-stone" 8 [| -1., 0.; -1., 0.92; -0.82, 1.; 0.82, 1.; 1., 0.92; 1., 0. |]
            { mesh with Smooth = false }
        [| unitSphere; smallSphere; capsule; leg; column; disc; torus; arch; curl; eyelid; rail; piping; slat
           roundedBox "rounded-box" id; head; nose; torso; toga; cape; helmet; armorTorso; visor; facePlate
           pectoral; fin; leaf; hair; scarf; tunic; rock; octagon |]
