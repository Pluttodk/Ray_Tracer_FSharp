module GeometryRegressionTests

open System
open System.Threading.Tasks
open Assert
open Tracer
open Tracer.Basics
open Tracer.Basics.Sampling
open Tracer.Basics.Transformation
open Tracer.BaseShape

let private near expected actual name =
    Assert.True(Double.IsFinite actual && abs(expected-actual) <= 1e-10 * max 1. (abs expected), name)

let private vectorNear (expected: Vector) (actual: Vector) name =
    near expected.X actual.X (name + "-x")
    near expected.Y actual.Y (name + "-y")
    near expected.Z actual.Z (name + "-z")

let private throws action name =
    let rejected = try action(); false with :? ArgumentException -> true
    Assert.True(rejected, name)

let allTest() =
    let material = Material.None
    let texture = Textures.mkMatTexture material
    for direction in [Vector(2.,0.,0.); Vector(0.,-3.,0.); Vector(0.3,0.7,-2.); Vector(1e-200,0.,0.)] do
        let ray = Ray(Point.Zero, direction)
        near 2.5 (ray.TimeAtPoint(ray.PointAtTime 2.5)) "ray-time-arbitrary-direction"
        vectorNear (-direction) ray.Invert.GetDirection "ray-invert-preserves-parameter-scale"
    vectorNear (Vector(1.,0.,0.)) (Vector(1e300,0.,0.).Normalise) "normalize-large-vector"
    vectorNear (Vector(0.,1.,0.)) (Vector(0.,1e-300,0.).Normalise) "normalize-small-vector"
    let extremeRay = Ray(Point(1e308,0.,0.),Vector(-1e308,0.,0.))
    near -1e308 (extremeRay.PointAtTime 2.).X "ray-point-fma-avoids-intermediate-overflow"
    near 2. (extremeRay.TimeAtPoint(Point(-1e308,0.,0.))) "ray-time-avoids-difference-overflow"
    near (Math.PI/2.) (Vector.Zero.AngleBetween (Vector(1.,0.,0.)) (Vector(0.,1.,0.))) "vector-angle"

    let bounds = BBox(Point.Zero, Point(1.,1.,1.))
    Assert.Equal(Some(1.,2.), bounds.intersect(Ray(Point(0.,0.5,-1.), Vector(0.,0.,1.))), "slab-zero-on-boundary")
    Assert.Equal(None, bounds.intersect(Ray(Point(-0.1,0.5,-1.), Vector(0.,0.,1.))), "slab-zero-outside")
    Assert.Equal(Some(-0.5,0.5), bounds.intersect(Ray(Point(0.5,0.5,0.5), Vector(1.,0.,0.))), "slab-inside")
    Assert.Equal(Some(1.,1.), bounds.intersect(Ray(Point(-1.,0.,0.5), Vector(1.,1.,0.))), "slab-corner-grazing")
    Assert.Equal(Some(0.25,0.5), bounds.IntersectInterval(Ray(Point.Zero, Vector(1.,0.,0.)),0.25,0.5), "slab-explicit-interval")
    Assert.True(not(bounds.boundingBoxIntersect(BBox(Point(0.5,2.,0.5),Point(1.5,3.,1.5)))), "box-overlap-requires-all-axes")
    Assert.True(bounds.boundingBoxIntersect(BBox(Point(1.,0.,0.),Point(2.,1.,1.))), "box-overlap-touching")
    Assert.True((BBox(Point(2.,0.,0.),Point(1.,1.,1.))).IsEmpty, "inverted-bounds-are-empty")
    Assert.Equal(None, bounds.intersect Ray.None, "zero-ray-does-not-intersect")
    let extremeBounds = BBox(Point(-1e308,-1.,-1.),Point(-0.5e308,1.,1.))
    Assert.Equal(Some(1.5,2.),extremeBounds.intersect extremeRay,"slab-difference-overflow-preserves-finite-interval")

    let center = Point(4.,5.,6.)
    let pointOnly = HitPoint(center)
    Assert.True(not pointOnly.DidHit && pointOnly.Point = center && pointOnly.Normal.IsZero,
                "point-only-hit-placeholder-retains-position-not-a-fake-normal")
    let sphere = SphereShape(center, 2., texture)
    let sphereRay = Ray(Point(8.,5.,6.), Vector(-2.,0.,0.))
    let sphereHit = sphere.hitFunction sphereRay
    near 1. sphereHit.Time "sphere-honors-center"
    vectorNear (Vector(1.,0.,0.)) sphereHit.GeometricNormal "sphere-outward-normal"
    Assert.True(sphere.isInside center, "sphere-center-is-inside")
    for shape in [API.mkSphere center 2. texture; (BaseSphere(center,2.)).toShape texture] do
        near sphereHit.Time (shape.hitFunction sphereRay).Time "sphere-api-base-no-double-translation"
    let insideHit = sphere.hitFunction(Ray(center, Vector(3.,0.,0.)))
    Assert.True(insideHit.DidHit && not insideHit.FrontFace, "inside-sphere-is-back-face")
    vectorNear (Vector(1.,0.,0.)) insideHit.GeometricNormal "inside-sphere-retains-outward-normal"
    vectorNear (Vector(-1.,0.,0.)) insideHit.Normal "inside-sphere-shading-faces-ray"
    Assert.True(insideHit.EscapedPoint.X < insideHit.Point.X, "inside-reflection-offset-stays-inside")
    Assert.True(insideHit.OffsetPoint(Vector(1.,0.,0.)).X > insideHit.Point.X, "outgoing-offset-exits-surface")
    let distantSphere = SphereShape(Point.Zero,1.,texture)
    near 99999999. (distantSphere.hitFunction(Ray(Point(0.,0.,1e8),Vector(0.,0.,-1.)))).Time
        "distant-sphere-discriminant-does-not-collapse"
    let slowSphere = SphereShape(Point.Zero,1e-9,texture)
    near 1e291 (slowSphere.hitFunction(Ray(Point(2e-9,0.,0.),Vector(-1e-300,0.,0.)))).Time
        "sphere-preserves-t-with-tiny-direction"
    let hugeSphere = SphereShape(Point.Zero,1e200,texture)
    Assert.True(hugeSphere.isInside(Point(0.5e200,0.,0.)),"sphere-inside-does-not-square-overflow")
    near 1. (hugeSphere.hitFunction(Ray(Point(2e200,0.,0.),Vector(-1e200,0.,0.)))).Time "large-sphere-intersection"
    let intervalSphere = SphereShape(Point(0.,0.,5.),2.,texture)
    let random = Random 2718
    let mutable intervalsRecoverExits = true
    for _ = 1 to 128 do
        let origin = Point(random.NextDouble()-0.5,random.NextDouble()-0.5,-2.)
        let target = Point(random.NextDouble()-0.5,random.NextDouble()-0.5,5.)
        let ray = Ray(origin,(target-origin).Normalise)
        let entry = intervalSphere.hitFunction ray
        let exit = (intervalSphere :> IIntervalShape).HitWithin(ray,entry.Time,infinity)
        intervalsRecoverExits <- intervalsRecoverExits && exit.DidHit && not exit.FrontFace
                                 && exit.Time > entry.Time+1. && Object.ReferenceEquals(ray,exit.Ray)
    Assert.True(intervalsRecoverExits,"native-sphere-intervals-exclude-nonaxial-entry-without-rebasing")
    for size in [1e-9; 1.; 1e12] do
        let ray = Ray(Point(2.*size,0.,0.), Vector(-size,0.,0.))
        let hit = HitPoint(ray,1.,Vector(7.,0.,0.),material,sphere)
        near 1. hit.Normal.Magnitude "hit-normal-is-unit"
        let outside, inside = hit.OffsetPoint(Vector(1.,0.,0.)), hit.OffsetPoint(Vector(-1.,0.,0.))
        Assert.True(outside.X > hit.Point.X && inside.X < hit.Point.X, "scale-aware-offset-both-sides")
        Assert.True(abs(outside.X-hit.Point.X) < size*1e-10, "offset-does-not-swallow-small-geometry")

    let disc = Disc(center,2.,texture)
    let diskRay = Ray(Point(5.,5.,10.),Vector(0.,0.,-2.))
    near 2. (disc.hitFunction diskRay).Time "disc-honors-center"
    near 0.75 (disc.hitFunction diskRay).U "disc-uv-relative-to-center"
    for shape in [API.mkDisk center 2. texture; (BaseDisc(center,2.)).toShape texture] do
        near 2. (shape.hitFunction diskRay).Time "disk-api-base-no-double-translation"
    let rectangle = Rectangle(Point(4.,5.,6.), Point(4.,7.,6.), Point(7.,5.,6.), texture)
    let rectangleHit = rectangle.hitFunction(Ray(Point(5.5,5.5,10.),Vector(0.,0.,-2.)))
    Assert.True(rectangleHit.DidHit, "translated-rectangle-hit")
    near 0.5 rectangleHit.U "rectangle-u-from-bottom-left"
    near 0.25 rectangleHit.V "rectangle-v-from-bottom-left"
    near 5.999999 (rectangle.getBoundingBox().lowPoint.Z) "rectangle-bounds-honor-depth"
    let tilted = Rectangle(Point.Zero,Point(0.,0.,2.),Point(3.,0.,0.),texture)
    Assert.True((tilted.hitFunction(Ray(Point(1.,2.,1.),Vector(0.,-1.,0.)))).DidHit, "rectangle-arbitrary-plane")

    let triangle = Triangle(Point.Zero,Point(1.,0.,0.),Point(0.,1.,0.),material)
    near 1. (triangle.hitFunction(Ray(Point(0.2,0.2,Double.Epsilon),Vector(0.,0.,-Double.Epsilon)))).Time
        "triangle-subnormal-direction-does-not-overflow-reciprocal"
    let hits = Array.zeroCreate<HitPoint> 1000
    Parallel.For(0,hits.Length,fun i ->
        let u, v = float(i % 97)/220., float(i % 79)/190.
        hits.[i] <- triangle.hitFunction(Ray(Point(u,v,1.),Vector(0.,0.,-2.)))) |> ignore
    Assert.True(hits |> Array.mapi (fun i hit ->
        hit.DidHit && abs(hit.BarycentricBeta-float(i % 97)/220.) < 1e-12
        && abs(hit.BarycentricGamma-float(i % 79)/190.) < 1e-12) |> Array.forall id,
        "parallel-triangle-barycentrics-belong-to-each-hit")
    let adjacent = Triangle(Point(1.,1.,0.),Point(0.,1.,0.),Point(1.,0.,0.),material)
    let edgeRay = Ray(Point(0.4,0.6,1.),Vector(0.,0.,-1.))
    Assert.True((triangle.hitFunction edgeRay).DidHit || (adjacent.hitFunction edgeRay).DidHit, "shared-edge-has-no-crack")

    let transformation = mergeTransformations [scale -2. 3. 0.5; translate 4. 5. 6.]
    let transformed = Transform.transform (SphereShape(Point.Zero,1.,texture)) transformation
    let transformedHit = transformed.hitFunction(Ray(Point(8.,5.,6.),Vector(-2.,0.,0.)))
    near 1. transformedHit.Time "nonuniform-transform-preserves-time"
    vectorNear (Vector(1.,0.,0.)) transformedHit.GeometricNormal "mirrored-solid-outward-normal"
    Assert.True(Object.ReferenceEquals(transformed, transformedHit.Shape), "transform-hit-identity-is-instance")
    let transformExit =
        (box transformed :?> IIntervalShape).HitWithin(Ray(Point(8.,5.,6.),Vector(-2.,0.,0.)),transformedHit.Time,infinity)
    near 3. transformExit.Time "transformed-native-interval-recovers-exit"
    Assert.True(not transformExit.FrontFace && Object.ReferenceEquals(transformed,transformExit.Shape),"transformed-interval-metadata")
    Assert.True(Object.ReferenceEquals(transformed.getBoundingBox(), transformed.getBoundingBox()), "transformed-bounds-are-cached")
    let shear = sheare(0.2,0.3,0.4,-0.2,0.1,0.25)
    let point = Point(1.5,-2.,3.)
    let roundTrip = transformPoint(transformPoint(point,getMatrix shear),getInvMatrix shear)
    vectorNear point.ToVector roundTrip.ToVector "general-shear-inverse"
    let identity = mergeTransformations []
    vectorNear point.ToVector (transformPoint(point,getMatrix identity)).ToVector "empty-transformation-is-identity"
    let affineMatrix =
        { getMatrix identity with
            Pos1x1 = 2.; Pos1x2 = 1.; Pos1x4 = 3.
            Pos2x2 = 2.; Pos2x4 = -2.
            Pos3x3 = 4.; Pos3x4 = 5. }
    let inverseMatrix =
        { getMatrix identity with
            Pos1x1 = 0.5; Pos1x2 = -0.25; Pos1x4 = -2.
            Pos2x2 = 0.5; Pos2x4 = 1.
            Pos3x3 = 0.25; Pos3x4 = -1.25 }
    let affine = mkTransformation(affineMatrix,inverseMatrix)
    Assert.Equal(affineMatrix,getMatrix affine,"public-affine-constructor-preserves-forward-matrix")
    Assert.Equal(inverseMatrix,getInvMatrix affine,"public-affine-constructor-preserves-supplied-inverse")
    let affinePoint = transformPoint(Point(1.,2.,3.),getMatrix affine)
    vectorNear (Vector(7.,2.,17.)) affinePoint.ToVector "public-affine-row-major-point"
    vectorNear (Vector(1.,2.,3.)) (transformPoint(affinePoint,getInvMatrix affine)).ToVector "public-affine-inverse-round-trip"
    vectorNear (Vector(0.5,-0.25,0.)) (Transform.transformNormal (Vector(1.,0.,0.)) affine) "public-affine-inverse-transpose-normal"
    throws (fun () -> mkTransformation({ affineMatrix with Pos1x1 = nan },inverseMatrix) |> ignore) "public-affine-nonfinite-matrix-rejected"
    throws (fun () -> mkTransformation(affineMatrix,{ inverseMatrix with Pos2x2 = infinity }) |> ignore) "public-affine-nonfinite-inverse-rejected"
    throws (fun () -> scale 1. 0. 1. |> ignore) "singular-scale-rejected"

    let cylinder = SolidCylinder(center,2.,4.,texture,texture,texture)
    let capHit = cylinder.hitFunction(Ray(center,Vector(0.,2.,0.)))
    near 1. capHit.Time "cylinder-inside-axis-hit"
    vectorNear (Vector(0.,1.,0.)) capHit.GeometricNormal "cylinder-top-normal-outward"
    Assert.True(not capHit.FrontFace && Object.ReferenceEquals(capHit.Shape,cylinder), "cylinder-medium-identity")
    Assert.True(Object.ReferenceEquals(cylinder.topDisc,cylinder.topDisc), "cylinder-components-cached")
    near 1. (API.mkSolidCylinder center 2. 4. texture texture texture |> fun s -> s.hitFunction(Ray(center,Vector(0.,2.,0.)))).Time
        "cylinder-api-no-double-translation"
    let makeBox low high = Box(low,high,texture,texture,texture,texture,texture,texture)
    let box = makeBox Point.Zero (Point(1.,1.,1.))
    let boxExit = box.hitFunction(Ray(Point(0.5,0.5,0.5),Vector(2.,0.,0.)))
    vectorNear (Vector(1.,0.,0.)) boxExit.GeometricNormal "box-exit-normal-outward"
    Assert.True(not boxExit.FrontFace, "box-exit-frontface-false")
    Assert.True((box.hitFunction(Ray(Point(0.,0.5,-1.),Vector(0.,0.,1.)))).DidHit, "box-on-slab-parallel-ray")

    let touching = makeBox (Point(1.,0.,0.)) (Point(2.,1.,1.))
    let ray = Ray(Point(0.5,0.5,0.5),Vector(2.,0.,0.))
    let union = CSG(box,touching,Union)
    near 0.75 (union.hitFunction ray).Time "touching-union-skips-internal-face"
    Assert.True(not (((CSG(box,touching,Intersection)).hitFunction ray).DidHit), "touching-intersection-has-no-volume")
    let distant = makeBox (Point(4.,0.,0.)) (Point(5.,1.,1.))
    Assert.True(not (((CSG(box,distant,Intersection)).hitFunction ray).DidHit), "disjoint-csg-terminates")
    Assert.True((CSG(box,distant,Intersection)).getBoundingBox().IsEmpty, "disjoint-csg-empty-bounds")
    Assert.True(not (((CSG(box,box,Subtraction)).hitFunction ray).DidHit), "identical-subtraction-empty")
    let cavity = CSG(SphereShape(Point.Zero,2.,texture),SphereShape(Point.Zero,1.,texture),Subtraction)
    let cavityHit = cavity.hitFunction(Ray(Point.Zero,Vector(2.,0.,0.)))
    near 0.5 cavityHit.Time "subtraction-from-inside-cavity"
    vectorNear (Vector(-1.,0.,0.)) cavityHit.GeometricNormal "cavity-normal-reversed-outward"
    Assert.True(cavityHit.FrontFace, "cavity-entry-frontface")
    let cavityExit = (cavity :> IIntervalShape).HitWithin(Ray(Point.Zero,Vector(2.,0.,0.)),cavityHit.Time,infinity)
    near 1. cavityExit.Time "csg-native-interval-recovers-outer-exit"
    Assert.True(not cavityExit.FrontFace,"csg-interval-exit-normal")

    let centerSampler = Sampler([|[|0.5,0.5|]|])
    let pinhole = PinholeCamera(Point(0.,0.,5.),Point.Zero,Vector(0.,1.,0.),1.,2.,2.,3,3,centerSampler)
    vectorNear (Vector(0.,0.,-1.)) (pinhole.CreateRays 1 1).[0].GetDirection "odd-resolution-center-ray"
    let polar = PinholeCamera(Point(0.,5.,0.),Point.Zero,Vector(0.,1.,0.),1.,2.,2.,3,3,centerSampler)
    let polarRay = (polar.CreateRays 1 1).[0]
    vectorNear (Vector(0.,-1.,0.)) polarRay.GetDirection "camera-pole-basis"
    near 0. (polar.U * polar.V) "camera-basis-orthogonal"
    throws (fun () -> PinholeCamera(Point.Zero,Point.Zero,Vector(0.,1.,0.),1.,2.,2.,3,3,centerSampler) |> ignore) "camera-degenerate-view-rejected"
    throws (fun () -> PinholeCamera(Point(0.,0.,5.),Point.Zero,Vector(0.,1.,0.),1.,2.,2.,0,3,centerSampler) |> ignore) "camera-zero-resolution-rejected"
    let lensSampler = Sampler([|[|0.5,0.5; 0.,0.; 1.,1.|]|])
    let lens = ThinLensCamera(Point(0.,0.,5.),Point.Zero,Vector(0.,1.,0.),1.,2.,2.,3,3,0.5,5.,centerSampler,lensSampler)
    let lensRays = lens.CreateRays 1 1
    Assert.Equal(3,lensRays.Length,"unequal-lens-view-sample-counts")
    Assert.True(lensRays |> Array.forall (fun ray -> (ray.GetOrigin-lens.Position).Magnitude <= 0.500000001), "lens-origins-are-on-disc")
    for ray in lensRays do
        let focus = ray.PointAtTime ((-5.) / (ray.GetDirection * lens.W))
        vectorNear Point.Zero.ToVector focus.ToVector "lens-rays-meet-focal-point"
    Assert.Equal(lens.CreateRaysAt 1 1 42UL,lens.CreateRaysAt 1 1 42UL,"camera-keyed-rays-repeat")
    for camera, sampled in [ (pinhole :> Camera), (pinhole :> ISampledCamera)
                             (lens :> Camera), (lens :> ISampledCamera) ] do
        let rays = Array.init sampled.SampleCount (fun index -> sampled.CreateRay(1, 1, 42UL, index))
        Assert.Equal(camera.CreateRaysAt 1 1 42UL, rays, "single-ray-camera-preserves-array-api")
        Assert.Equal(sampled.SampleCount,camera.SampleCount,"camera-exposes-public-sample-count")
        let defaultRays = Array.init camera.SampleCount (fun index -> camera.CreateRay(1,1,index))
        Assert.Equal(camera.CreateRays 1 1,defaultRays,"public-single-ray-camera-preserves-pixel-key")
        let keyedRays = Array.init camera.SampleCount (fun index -> camera.CreateRay(1,1,42UL,index))
        Assert.Equal(rays,keyedRays,"public-keyed-single-ray-camera-preserves-interface")
        throws (fun () -> camera.CreateRay(1,1,-1) |> ignore) "camera-rejects-negative-sample-index"
        throws (fun () -> camera.CreateRay(1,1,camera.SampleCount) |> ignore) "camera-rejects-excess-sample-index"
        throws (fun () -> sampled.CreateRay(1,1,42UL,sampled.SampleCount) |> ignore) "sampled-interface-rejects-excess-sample-index"
    let addressedSampler =
        Sampler([| [|0.25,0.25; 0.75,0.75|]
                   [|0.1,0.2; 0.3,0.4|]
                   [|0.8,0.6; 0.9,0.7|] |])
    let addressedCamera =
        PinholeCamera(Point(0.,0.,5.),Point.Zero,Vector(0.,1.,0.),1.,2.,2.,4,3,addressedSampler)
    let expectedSamples = addressedSampler.SampleSetAt(mixKey (uint64 (1*4+2)))
    for index = 0 to addressedCamera.SampleCount-1 do
        let sx, sy = expectedSamples.[index]
        let expected = Vector(0.5*sx,(2./3.)*(sy-0.5),-1.).Normalise
        vectorNear expected (addressedCamera.CreateRay(2,1,index)).GetDirection "camera-uses-addressed-pixel-sample-set"
    let singleOnly =
        { new Camera(Point(0.,0.,5.),Point.Zero,Vector(0.,1.,0.),1.,2.,2.,4,3) with
            member _.CreateRays _ _ = invalidOp "The sampled camera must not allocate a ray array."
          interface ISampledCamera with
            member _.SampleCount = 2
            member _.CreateRay(_, _, _, _) = Ray(Point(0.,0.,5.),Vector(0.,0.,-1.)) }
    Assert.Equal(2,singleOnly.SampleCount,"public-sample-count-does-not-call-array-api")
    Assert.True((singleOnly.CreateRay(2,1,0)).IsValid,"public-single-ray-does-not-call-array-api")
    let unbounded = Transform.transform (InfinitePlane texture) (rotateX (Math.PI / 2.))
    Assert.True(unbounded.Bounds.IsNone, "transformed-plane-is-explicitly-unbounded")
    Assert.True(unbounded.IsOpaque, "transform-preserves-opaque-metadata")
    Assert.True(Shape.None.Bounds.Value.IsEmpty, "blank-shape-is-explicitly-empty")
    Assert.True((CSG(box,distant,Intersection)).Bounds.Value.IsEmpty, "csg-preserves-empty-bounds")
    let glassTexture = Textures.mkMatTexture(TransparentMaterial(Colour.White, Colour.White, 1.5, 1.))
    Assert.True(not (SphereShape(Point.Zero,1.,glassTexture)).IsOpaque, "glass-does-not-enter-opaque-shadow-path")
    let procedural = Textures.mkTexture (fun _ _ -> material)
    Assert.True(not (Textures.isOpaque procedural), "unknown-texture-opacity-is-conservative")
    Assert.True(Textures.isOpaque (Textures.markOpaque procedural), "prepared-opaque-texture-metadata")
    let unknownTexture = Textures.mkTexture (fun _ _ -> invalidOp "Opacity metadata must not evaluate a texture.")
    let opaqueFactories: (string * (Textures.Texture -> Shape)) list =
        [ "rectangle", (fun t -> Rectangle(Point.Zero,Point(0.,1.,0.),Point(1.,0.,0.),t) :> Shape)
          "disc", (fun t -> Disc(Point.Zero,1.,t) :> Shape)
          "sphere", (fun t -> SphereShape(Point.Zero,1.,t) :> Shape)
          "hollow-cylinder", (fun t -> HollowCylinder(Point.Zero,1.,2.,t) :> Shape)
          "solid-cylinder", (fun t -> SolidCylinder(Point.Zero,1.,2.,t,t,t) :> Shape)
          "box", (fun t -> Box(Point.Zero,Point(1.,1.,1.),t,t,t,t,t,t) :> Shape)
          "plane", (fun t -> InfinitePlane(t) :> Shape)
          "implicit", (fun t -> (ImplicitSurfaces.mkImplicit "x^2+y^2+z^2-1").toShape t)
          "bounded-implicit", (fun t ->
              (ImplicitSurfaces.mkBoundedImplicit "x^2+y^2+z^2-1" (BBox(Point(-1.,-1.,-1.),Point(1.,1.,1.)))).toShape t) ]
    for name, create in opaqueFactories do
        let opaque, glass, unknown = create texture, create glassTexture, create unknownTexture
        Assert.True(opaque.IsOpaque,name + "-constant-texture-proves-opacity")
        Assert.True(not glass.IsOpaque,name + "-glass-is-not-opaque")
        Assert.True(not unknown.IsOpaque,name + "-unknown-texture-stays-conservative")
        Assert.True(not (Transform.transform glass (translate 1. 2. 3.)).IsOpaque,name + "-transform-preserves-glass-opacity")
        Assert.True(not (Transform.transform unknown (translate 1. 2. 3.)).IsOpaque,name + "-transform-preserves-unknown-opacity")
    let unknownShape =
        { new Shape() with
            member _.getBoundingBox() = BBox(Point.Zero,Point(1.,1.,1.))
            member _.isInside _ = false
            member _.hitFunction ray = HitPoint(ray) }
    Assert.True(not unknownShape.IsOpaque,"custom-shape-default-opacity-is-conservative")
    let glassMaterial = TransparentMaterial(Colour.White,Colour.White,1.5,1.)
    Assert.True(triangle.IsOpaque,"triangle-constant-material-is-opaque")
    Assert.True(not (Triangle(Point.Zero,Point(1.,0.,0.),Point(0.,1.,0.),glassMaterial)).IsOpaque,"glass-triangle-is-not-opaque")
    for operation in [Union;Intersection;Subtraction;Grouping] do
        let opaque = SphereShape(Point.Zero,1.,texture)
        let glass = SphereShape(Point.Zero,1.,glassTexture)
        Assert.True((CSG(opaque,opaque,operation)).IsOpaque,sprintf "%A-both-operands-prove-opacity" operation)
        Assert.True(not (CSG(opaque,glass,operation)).IsOpaque,sprintf "%A-glass-second-operand-is-conservative" operation)
        Assert.True(not (CSG(glass,opaque,operation)).IsOpaque,sprintf "%A-glass-first-operand-is-conservative" operation)
        Assert.True(not (CSG(opaque,unknownShape,operation)).IsOpaque,sprintf "%A-unknown-operand-is-conservative" operation)
    for face = 0 to 5 do
        let textures = Array.create 6 texture
        textures.[face] <- glassTexture
        let mixed = Box(Point.Zero,Point(1.,1.,1.),textures.[0],textures.[1],textures.[2],textures.[3],textures.[4],textures.[5])
        Assert.True(not mixed.IsOpaque,"box-requires-all-six-faces-opaque")
    for part = 0 to 2 do
        let textures = Array.create 3 texture
        textures.[part] <- glassTexture
        let mixed = SolidCylinder(Point.Zero,1.,2.,textures.[0],textures.[1],textures.[2])
        Assert.True(not mixed.IsOpaque,"solid-cylinder-requires-side-and-both-caps-opaque")
    near 1. (sphereHit.SpawnRay(Vector(7.,0.,0.)).GetDirection.Magnitude) "spawned-rays-use-physical-distance"
