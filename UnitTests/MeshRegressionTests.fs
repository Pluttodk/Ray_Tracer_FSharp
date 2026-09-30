module MeshRegressionTests

open System
open System.Globalization
open System.IO
open System.Text
open System.Threading.Tasks
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open Assert
open Tracer.Basics
open Tracer.Basics.PLYParser
open Tracer.Basics.TriangleMesh
open Tracer.Basics.Transformation

let private near expected actual name =
    Assert.True(Double.IsFinite actual && abs(expected-actual) <= 1e-10 * max 1. (abs expected), name)

let private rejects action name =
    let rejected =
        try action(); false
        with :? InvalidDataException as error -> error.Message.StartsWith("PLY", StringComparison.Ordinal)
    Assert.True(rejected, name)

let private withFile (bytes: byte array) action =
    let filename = ".geometry-mesh-regression-" + Guid.NewGuid().ToString("N") + ".ply"
    try
        File.WriteAllBytes(filename, bytes)
        action filename
    finally
        if File.Exists filename then File.Delete filename

type private NonSeekableStream(inner: Stream) =
    inherit Stream()
    override _.CanRead = true
    override _.CanSeek = false
    override _.CanWrite = false
    override _.Length = raise (NotSupportedException())
    override _.Position with get() = raise (NotSupportedException()) and set _ = raise (NotSupportedException())
    override _.Read(buffer, offset, count) = inner.Read(buffer, offset, min 1 count)
    override _.Seek(_, _) = raise (NotSupportedException())
    override _.SetLength _ = raise (NotSupportedException())
    override _.Write(_, _, _) = raise (NotSupportedException())
    override _.Flush() = ()

let private asciiQuad =
    String.concat "\n"
        [ "ply"; "format ascii 1.0"; "comment géométrie with reordered elements"
          "element face 1"; "property uchar material"
          "property list uchar int vertex_indices"; "property list uchar float extras"
          "element unknown 1"; "property uchar flags"; "property list char uint values"
          "element vertex 4"; "property float v"; "property double z"; "property float x"
          "property list uchar short tags"; "property float y"; "property float u"
          "property float nx"; "property float ny"; "property float nz"; "end_header"
          "7 4 0 1 2 3 2 0.25 0.75"; "255 2 4294967295 200"
          "0 0 0 2 -2 3 0 0 1 0 1"
          "0 0 1 0 0 1 1 0 1"
          "1 0 1 1 32767 1 1 1 0 1"
          "1 0 0 0 1 0 1 0 1"; "" ]

let private binaryFixture bigEndian newline =
    use stream = new MemoryStream()
    let bytes (values: byte array) =
        let values = Array.copy values
        if bigEndian then Array.Reverse values
        stream.Write(values, 0, values.Length)
    let header =
        String.concat newline
            [ "ply"; (if bigEndian then "format binary_big_endian 1.0" else "format binary_little_endian 1.0")
              "comment géométrie"; "element vertex 3"; "property uchar x"; "property ushort y"
              "property int z"; "property char nx"; "property float ny"; "property double nz"
              "property float u"; "property double v"; "property uint ignored_uint"
              "property short ignored_short"; "property list uchar short tags"
              "element face 1"; "property uint material"; "property float opacity"
              "property list char uint vertex_indices"; "property list uchar double extras"; "end_header"; "" ]
        |> Encoding.UTF8.GetBytes
    stream.Write(header, 0, header.Length)
    for i = 0 to 2 do
        stream.WriteByte(byte (10+i))
        bytes (BitConverter.GetBytes(65000us))
        bytes (BitConverter.GetBytes(-2000000000+i))
        stream.WriteByte(255uy)
        bytes (BitConverter.GetBytes(0.5f))
        bytes (BitConverter.GetBytes(1.))
        bytes (BitConverter.GetBytes(0.25f))
        bytes (BitConverter.GetBytes(0.75))
        bytes (BitConverter.GetBytes(UInt32.MaxValue))
        bytes (BitConverter.GetBytes(-32000s))
        stream.WriteByte(2uy)
        bytes (BitConverter.GetBytes(Int16.MinValue))
        bytes (BitConverter.GetBytes(Int16.MaxValue))
    bytes (BitConverter.GetBytes(UInt32.MaxValue))
    bytes (BitConverter.GetBytes(0.5f))
    stream.WriteByte(3uy)
    for i = 0 to 2 do bytes (BitConverter.GetBytes(uint32 i))
    stream.WriteByte(1uy)
    bytes (BitConverter.GetBytes(0.125))
    stream.ToArray()

let private minimal vertices face =
    String.concat "\n"
        [ "ply"; "format ascii 1.0"; sprintf "element vertex %d" (List.length vertices)
          "property float x"; "property float y"; "property float z"
          "element face 1"; "property list uchar int vertex_indices"; "end_header"
          String.concat "\n" vertices; face; "" ]
    |> Encoding.UTF8.GetBytes

let allTest() =
    Assert.Equal(64,Marshal.SizeOf<MeshVertex>(),"mesh-export-vertex-layout-is-eight-fp64-values")
    Assert.Equal(12,Marshal.SizeOf<TriangleIndices>(),"mesh-export-triangle-layout-is-three-int32-values")
    Assert.True(not (RuntimeHelpers.IsReferenceOrContainsReferences<MeshVertex>()),"mesh-export-vertices-are-blittable")
    Assert.True(not (RuntimeHelpers.IsReferenceOrContainsReferences<TriangleIndices>()),"mesh-export-indices-are-blittable")
    let originalCulture = CultureInfo.CurrentCulture
    try
        CultureInfo.CurrentCulture <- CultureInfo.GetCultureInfo("fr-FR")
        for newline in ["\n"; "\r\n"; "\r"] do
            let source = asciiQuad.Replace("\n", newline) |> Encoding.UTF8.GetBytes
            use stream = new MemoryStream(source)
            let vertices, faces = parseIndexedPLYStream stream
            Assert.Equal(4, vertices.Length, "ascii-ply-reordered-elements")
            Assert.Equal([|[|0;1;2;3|]|], faces, "ascii-ply-keeps-complete-polygons")
            near 1. vertices.[2].u.Value "ascii-ply-invariant-u"
            near 1. vertices.[2].v.Value "ascii-ply-invariant-v"
        for bigEndian in [false; true] do
            for newline in ["\n"; "\r\n"; "\r"] do
                let source = binaryFixture bigEndian newline
                use data = new MemoryStream(source)
                use stream = new NonSeekableStream(data)
                let vertices, faces = parseIndexedPLYStream stream
                near 10. vertices.[0].x "binary-ply-header-keeps-first-0x0a"
                near 65000. vertices.[0].y "binary-ply-unsigned-short"
                near -2000000000. vertices.[0].z "binary-ply-signed-int"
                near -1. vertices.[0].nx.Value "binary-ply-signed-char"
                near 0.5 vertices.[0].ny.Value "binary-ply-float"
                near 1. vertices.[0].nz.Value "binary-ply-double"
                near 0.25 vertices.[0].u.Value "binary-ply-u"
                near 0.75 vertices.[0].v.Value "binary-ply-v"
                Assert.Equal([|[|0;1;2|]|],faces,"binary-ply-skips-unknown-properties-in-order")
                rejects (fun () ->
                    use short = new MemoryStream(source.[0..source.Length-2])
                    parseIndexedPLYStream short |> ignore) "binary-ply-short-read-rejected"
    finally CultureInfo.CurrentCulture <- originalCulture

    rejects (fun () ->
        use stream = new MemoryStream(Encoding.UTF8.GetBytes("ply\nformat ascii 1.0\n"))
        parseIndexedPLYStream stream |> ignore) "ply-truncated-header"
    rejects (fun () ->
        let header = "ply\nformat ascii 1.0\nelement vertex 2147483647\nproperty float x\nproperty float y\nproperty float z\nend_header\n"
        use data = new MemoryStream(Encoding.UTF8.GetBytes header)
        use stream = new NonSeekableStream(data)
        parseIndexedPLYStream stream |> ignore) "ply-short-body-does-not-preallocate-untrusted-count"
    rejects (fun () ->
        use stream = new MemoryStream(minimal ["0 0 0";"1 0 0";"0 1 0"] "3 0 1 3")
        parseIndexedPLYStream stream |> ignore) "ply-index-out-of-range"
    rejects (fun () ->
        use stream = new MemoryStream(minimal ["NaN 0 0";"1 0 0";"0 1 0"] "3 0 1 2")
        parseIndexedPLYStream stream |> ignore) "ply-nonfinite-coordinate"
    rejects (fun () ->
        use stream = new MemoryStream(minimal ["0 0 0";"1 0 0";"0 1 0"] "-1")
        parseIndexedPLYStream stream |> ignore) "ply-negative-count"
    let badCountType =
        (Encoding.UTF8.GetString(minimal ["0 0 0";"1 0 0";"0 1 0"] "3 0 1 2"))
            .Replace("list uchar int", "list float int")
    rejects (fun () ->
        use stream = new MemoryStream(Encoding.UTF8.GetBytes badCountType)
        parseIndexedPLYStream stream |> ignore) "ply-floating-list-count-rejected"

    let originalRegistry = Acceleration.listOfAccel
    withFile (Encoding.UTF8.GetBytes asciiQuad) (fun filename ->
        let vertices, faces = parsePLY filename
        Assert.Equal([|[4;0;1;2;3]|], faces, "legacy-ply-parser-return-contract")
        Assert.Equal(4,vertices.Length,"legacy-ply-parser-vertex-contract")
        let geometry = drawTriangles filename true :?> BaseMeshShape
        Assert.Equal(4,geometry.Geometry.VertexCount,"indexed-mesh-stores-shared-vertices")
        Assert.Equal(2,geometry.Geometry.TriangleCount,"convex-polygon-triangulation")
        let exported = geometry.Geometry.Export()
        let exportedVertices, exportedTriangles = exported.Vertices.ToArray(), exported.Triangles.ToArray()
        let exportedNodes, exportedIndices = exported.Blas.Nodes.ToArray(), exported.Blas.PrimitiveIndices.ToArray()
        Assert.Equal(4,exported.VertexCount,"mesh-export-retains-indexed-vertex-count")
        Assert.Equal(2,exported.TriangleCount,"mesh-export-retains-triangle-count")
        Assert.True(exported.SmoothShading,"mesh-export-preserves-shading-mode")
        Assert.Equal(geometry.Geometry.Bounds,exported.Bounds,"mesh-export-preserves-object-space-bounds")
        near (1./sqrt 2.) exportedVertices.[0].Nx "mesh-export-uses-prepared-shading-normals"
        near (1./sqrt 2.) exportedVertices.[0].Nz "mesh-export-prepared-normal-z"
        near 1. exportedVertices.[2].U "mesh-export-preserves-u"
        near 1. exportedVertices.[2].V "mesh-export-preserves-v"
        Assert.Equal([|{ A=0; B=1; C=2 }; { A=0; B=2; C=3 }|],exportedTriangles,"mesh-export-triangulation-indices")
        Assert.True(exported.Blas.IsFullyBounded && exported.Blas.PrimitiveCount = exported.TriangleCount,
                    "mesh-export-blas-addresses-the-exported-triangles")
        Assert.Equal([|0;1|],Array.sort exportedIndices,"mesh-export-blas-indices-retain-original-triangle-ordinals")
        let mutable vertexStorage = Unchecked.defaultof<ArraySegment<MeshVertex>>
        let mutable triangleStorage = Unchecked.defaultof<ArraySegment<TriangleIndices>>
        let mutable nodeStorage = Unchecked.defaultof<ArraySegment<FlatBVH.NodeData>>
        let mutable indexStorage = Unchecked.defaultof<ArraySegment<int>>
        if MemoryMarshal.TryGetArray(exported.Vertices,&vertexStorage)
           && MemoryMarshal.TryGetArray(exported.Triangles,&triangleStorage)
           && MemoryMarshal.TryGetArray(exported.Blas.Nodes,&nodeStorage)
           && MemoryMarshal.TryGetArray(exported.Blas.PrimitiveIndices,&indexStorage) then
            vertexStorage.Array.[vertexStorage.Offset] <- { exportedVertices.[0] with X = 1000. }
            triangleStorage.Array.[triangleStorage.Offset] <- { A=3; B=3; C=3 }
            nodeStorage.Array.[nodeStorage.Offset] <- { exportedNodes.[0] with MinZ = 1000. }
            indexStorage.Array.[indexStorage.Offset] <- -1
        else Assert.Fail("mesh-export-snapshot-storage-is-unavailable")
        let fresh = geometry.Geometry.Export()
        Assert.Equal(exportedVertices,fresh.Vertices.ToArray(),"mesh-export-cannot-mutate-prepared-vertices")
        Assert.Equal(exportedTriangles,fresh.Triangles.ToArray(),"mesh-export-cannot-mutate-prepared-triangles")
        Assert.Equal(exportedNodes,fresh.Blas.Nodes.ToArray(),"mesh-export-cannot-mutate-shared-blas-nodes")
        Assert.Equal(exportedIndices,fresh.Blas.PrimitiveIndices.ToArray(),"mesh-export-cannot-mutate-shared-blas-indices")
        let firstMaterial, secondMaterial = BlankMaterial(), BlankMaterial()
        let mutable textureCalls = 0
        let firstTexture = Textures.mkTexture(fun _ _ -> textureCalls <- textureCalls+1; firstMaterial :> Material)
        let first = geometry.toShape firstTexture :?> MeshShape
        let second = geometry.toShape (Textures.mkMatTexture secondMaterial) :?> MeshShape
        Assert.True(Object.ReferenceEquals(first.Geometry, second.Geometry), "mesh-instances-share-geometry-and-acceleration")
        Assert.True(not first.IsOpaque && textureCalls = 0,"mesh-unknown-texture-opacity-does-not-evaluate-material")
        Assert.True(second.IsOpaque,"mesh-constant-opaque-texture-proves-opacity")
        let glassTexture = Textures.mkMatTexture(TransparentMaterial(Colour.White,Colour.White,1.5,1.))
        let glass = geometry.toShape glassTexture :?> MeshShape
        Assert.True(Object.ReferenceEquals(glass.Geometry,second.Geometry),"glass-and-opaque-mesh-share-the-same-blas")
        Assert.True(not glass.IsOpaque,"mesh-wrapper-does-not-inherit-blank-blas-material-opacity")
        Assert.True(not (Transform.transform glass (translate 1. 2. 3.)).IsOpaque,"transformed-glass-mesh-remains-nonopaque")
        let legacyGlass = PLYTriangle(Point.Zero,Point(1.,0.,0.),Point(0.,1.,0.),glassTexture,false,false,false)
        Assert.True(not legacyGlass.IsOpaque,"legacy-ply-triangle-overrides-blank-material-opacity")
        let miss = first.hitFunction(Ray(Point(2.,2.,1.),Vector(0.,0.,-1.)))
        Assert.True(not miss.DidHit && textureCalls = 0, "mesh-miss-does-not-evaluate-material")
        let ray = Ray(Point(0.25,0.75,1.),Vector(0.,0.,-2.))
        let hit = first.hitFunction ray
        Assert.True(hit.DidHit && textureCalls = 1, "mesh-shades-only-closest-hit")
        near 0.25 hit.U "mesh-u-not-swapped"
        near 0.75 hit.V "mesh-v-not-swapped"
        near 0. hit.GeometricNormal.X "mesh-keeps-geometric-normal"
        near 1. hit.GeometricNormal.Z "mesh-geometric-normal-outward"
        near (1./sqrt 2.) hit.ShadingNormal.X "mesh-interpolated-shading-normal"
        Assert.True(hit.FrontFace && Object.ReferenceEquals(hit.Shape,first), "mesh-hit-uses-wrapper-medium-identity")
        Assert.True(not (((first :> IIntervalShape).HitWithin(ray,hit.Time,infinity)).DidHit),"open-mesh-interval-does-not-repeat-first-surface")
        let secondHit = second.hitFunction ray
        Assert.True(Object.ReferenceEquals(secondHit.Material,secondMaterial), "mesh-instance-material-not-cached-globally")
        Assert.True(Object.ReferenceEquals(secondHit.Shape,second), "mesh-instance-identities-are-distinct")
        let transformed = Transform.transform first (mergeTransformations [scale 2. 3. 0.5; translate 4. 5. 6.])
        let transformedHit = transformed.hitFunction(Ray(Point(4.5,7.25,8.),Vector(0.,0.,-4.)))
        near 0.5 transformedHit.Time "transformed-mesh-time"
        near hit.U transformedHit.U "transformed-mesh-u"
        near hit.V transformedHit.V "transformed-mesh-v"
        near hit.BarycentricBeta transformedHit.BarycentricBeta "transformed-mesh-barycentrics"
        near (1./sqrt 17.) transformedHit.ShadingNormal.X "transformed-mesh-inverse-transpose-normal"
        Assert.True(transformedHit.FrontFace && Object.ReferenceEquals(transformed,transformedHit.Shape), "transformed-mesh-medium-identity")
        let back = transformed.hitFunction(Ray(Point(4.5,7.25,4.),Vector(0.,0.,4.)))
        Assert.True(back.DidHit && not back.FrontFace, "transformed-mesh-exit-frontface")
        let results = Array.zeroCreate<bool> 1000
        Parallel.For(0,results.Length,fun i ->
            let u, v = float(i % 97)/100., float(i % 89)/100.
            let hit = second.hitFunction(Ray(Point(u,v,1.),Vector(0.,0.,-1.)))
            results.[i] <- hit.DidHit && abs(hit.U-u) < 1e-12 && abs(hit.V-v) < 1e-12) |> ignore
        Assert.True(Array.forall id results,"parallel-ply-shading-has-no-shared-barycentric-state"))
    Assert.True(Object.ReferenceEquals(originalRegistry,Acceleration.listOfAccel),"mesh-does-not-use-global-acceleration-cache")

    for u, v in [ 1., 0.25; 0.25, 1.; 0., 0. ] do
        let row x y = String.Join(" ", [| x; y; "0"; u.ToString("R", CultureInfo.InvariantCulture); v.ToString("R", CultureInfo.InvariantCulture) |])
        let constantUv =
            String.concat "\n"
                [ "ply"; "format ascii 1.0"; "element vertex 3"; "property double x"; "property double y"
                  "property double z"; "property double u"; "property double v"; "element face 1"
                  "property list uchar int vertex_indices"; "end_header"
                  row "0" "0"; row "1" "0"; row "0" "1"; "3 0 1 2"; "" ]
            |> Encoding.ASCII.GetBytes
        withFile constantUv (fun filename ->
            let vertices, _ = parsePLY filename
            let texture = Textures.mkMatTexture(BlankMaterial())
            let indexed = (drawTriangles filename false).toShape texture
            let direct =
                PLYTriangle(TriPoint(vertices.[0]), TriPoint(vertices.[1]), TriPoint(vertices.[2]), texture, false, false, true) :> Shape
            for name, shape in [ "indexed", indexed; "direct", direct ] do
                let mutable exact = true
                for x = 1 to 48 do
                    for y = 1 to 48 do
                        let hit = shape.hitFunction(Ray(Point(float x / 103., float y / 107., 1.), Vector(0., 0., -1.)))
                        exact <- exact && hit.DidHit && hit.U = u && hit.V = v
                Assert.True(exact, $"constant-texture-coordinates-remain-exact-{name}-{u}-{v}"))

    let withoutNormals = minimal ["0 0 0";"1 0 0";"0 1 0"] "3 0 1 2"
    withFile withoutNormals (fun filename ->
        let smooth = (drawTriangles filename true :?> BaseMeshShape).Geometry.Export()
        Assert.True(smooth.Vertices.ToArray() |> Array.forall (fun vertex -> vertex.Nx = 0. && vertex.Ny = 0. && vertex.Nz = 1.),
                    "mesh-export-includes-generated-area-weighted-normals")
        let flat = (drawTriangles filename false :?> BaseMeshShape).Geometry.Export()
        Assert.True(not flat.SmoothShading,"mesh-export-explicitly-preserves-flat-shading")
        Assert.True(flat.Vertices.ToArray() |> Array.forall (fun vertex -> vertex.Nx = 0. && vertex.Ny = 0. && vertex.Nz = 0.),
                    "flat-mesh-export-does-not-imply-smooth-normals"))
    let concave = minimal ["0 0 0";"2 0 0";"1 0.5 0";"2 2 0";"0 2 0"] "5 0 1 2 3 4"
    withFile concave (fun filename -> rejects (fun () -> drawTriangles filename false |> ignore) "concave-ply-explicitly-rejected")
    let nonplanar = minimal ["0 0 0";"1 0 0";"1 1 1";"0 1 0"] "4 0 1 2 3"
    withFile nonplanar (fun filename -> rejects (fun () -> drawTriangles filename false |> ignore) "nonplanar-ply-explicitly-rejected")
    let crossing = minimal ["0 0 0";"1 1 0";"0 1 0";"1 0 0"] "4 0 1 2 3"
    withFile crossing (fun filename -> rejects (fun () -> drawTriangles filename false |> ignore) "self-crossing-ply-explicitly-rejected")
    let degenerate = minimal ["0 0 0";"0 0 0";"1 0 0"] "3 0 1 2"
    withFile degenerate (fun filename ->
        let mesh = drawTriangles filename true :?> BaseMeshShape
        Assert.Equal(1,mesh.Geometry.DegenerateTriangleCount,"degenerate-source-triangle-is-reported")
        let exported = mesh.Geometry.Export()
        Assert.Equal(1,exported.DegenerateTriangleCount,"mesh-export-reports-retained-degenerate-triangles")
        Assert.Equal(1,exported.TriangleCount,"mesh-export-does-not-drop-source-triangles")
        let shape = mesh.toShape(Textures.mkMatTexture Material.None)
        Assert.True(not ((shape.hitFunction(Ray(Point(0.,0.,1.),Vector(0.,0.,-1.)))).DidHit),"degenerate-triangle-does-not-hit"))
    let empty = "ply\nformat ascii 1.0\nelement vertex 0\nproperty float x\nproperty float y\nproperty float z\nelement face 0\nproperty list uchar int vertex_indices\nend_header\n"
    withFile (Encoding.UTF8.GetBytes empty) (fun filename ->
        let shape = (drawTriangles filename true).toShape(Textures.mkMatTexture Material.None)
        Assert.True(shape.getBoundingBox().IsEmpty,"empty-mesh-has-empty-bounds")
        let exported = (shape :?> MeshShape).Geometry.Export()
        Assert.True(exported.Vertices.IsEmpty && exported.Triangles.IsEmpty,"empty-mesh-export-has-empty-arrays")
        Assert.Equal(-1,exported.Blas.RootIndex,"empty-mesh-export-has-no-blas-root")
        Assert.True(not((shape.hitFunction(Ray(Point(0.,0.,1.),Vector(0.,0.,-1.)))).DidHit),"empty-mesh-does-not-hit"))
