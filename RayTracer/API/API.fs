namespace Tracer

open Tracer.Basics.Sampling
open Tracer.Basics
open Tracer.Basics.Sampling
open Tracer.Basics.Render
open Tracer.BaseShape
open Tracer.ImplicitSurfaces
open Tracer.Basics.Textures
open Transformation

module API = 

  //////////////////////
  // Type definitions //
  //////////////////////

  type dummy = unit

  type vector = Vector
  type point = Point
  type colour = Colour
  type material = Material
  type shape = Shape
  type baseShape = BaseShape
  type texture = Texture
  type camera = Camera
  type scene = Scene
  type light = Light
  type ambientLight = AmbientLight
  type transformation = Transformation
  type sampler = Sampler

  ////////////
  // Basics //
  ////////////

  let mkVector (x : float) (y : float) (z : float) : vector = 
    new Vector(x, y, z)
  
  let mkPoint (x : float) (y : float) (z : float) : point = 
    new Point(x, y, z)
  
  let fromColor (c : System.Drawing.Color) : colour = 
    new Colour(c)
  
  let mkColour (r : float) (g : float) (b : float) : colour = 
    new Colour(r, g, b)

  //////////////
  // Samplers //
  //////////////

  let mkRegularSampler (n : int) : sampler = 
    regular n
 
  let mkRandomSampler (n : int) (sets : int) : sampler = 
    random n sets
  
  let mkNRooksSampler (n : int) (sets : int) : sampler = 
    nRooks n sets
  
  let mkJitteredSampler (n : int) (sets : int) : sampler = 
    jittered n sets
  
  let mkMultiJitteredSampler (n : int) (sets : int) : sampler = 
    multiJittered n sets

  ///////////////
  // Materials //
  ///////////////

  let mkMatteMaterial (ca : colour) (ka : float) (cd : colour) (kd : float) : material = 
    new MatteMaterial(ca, ka, cd, kd) :> material
  
  let mkPhongMaterial (ca : colour) (ka : float) (cd : colour) (kd : float) (cs : colour) (ks : float) (exp : int) : material = 
    new PhongMaterial(ca, ka, cd, kd, cs, ks, exp) :> material
  
  let mkMatteReflectiveMaterial (ca : colour) (ka : float) (cd : colour) (kd : float) (cr : colour) (kr : float) : material = 
    new MatteReflectiveMaterial(ca, ka, cd, kd, cr, kr) :> material
  
  let mkMatteGlossyReflectiveMaterial (ca : colour) (ka : float) (cd : colour) (kd : float) (cr : colour) (kr : float) (exps : int) (s : sampler) : material = 
    new MatteGlossyReflectiveMaterial(ca, ka, cd, kd, cr, kr, exps, s) :> material
  
  let mkPhongReflectiveMaterial (ca : colour) (ka : float) (cd : colour) (kd : float) (cs : colour) (ks : float) (cr : colour) (kr : float) (exps : int) : material = 
    new PhongReflectiveMaterial(ca, ka, cd, kd, cs, ks, cr, kr, exps) :> material
  
  let mkPhongGlossyReflectiveMaterial (ca : colour) (ka : float) (cd : colour) (kd : float) (cs : colour) (ks : float) (cr : colour) (kr : float) (exps : int) (expr : int) (s : sampler) : material = 
    new PhongGlossyReflectiveMaterial(ca, ka, cd, kd, cs, ks, cr, kr, exps, expr, s) :> material
  
  let mkEmissive (c : colour) (i : float) : material = 
    new EmissiveMaterial(c, i) :> material

  let mkTransparent (cf_in : colour) (cf_out : colour) (eta_in : float) (eta_out : float) : material = 
    new TransparentMaterial(cf_in, cf_out, eta_in, eta_out) :> material

  let mkTexture (f : float -> float -> material) : texture = 
    Textures.mkTexture f
  
  let mkMatTexture (m : material) : texture = 
    Textures.mkMatTexture m

  ////////////
  // Shapes //
  ////////////

  let mkShape (b : baseShape) (t : texture) : shape = 
    b.toShape t
  
  let mkSphere (p : point) (r : float) (m : texture) : shape = 
    let sphere = SphereShape(p, r, m)
    Transform.transform sphere (Transformation.translate p.X p.Y p.Z)
  
  let mkBaseSphere (p : point) (r : float)  : baseShape = 
    new BaseSphere(p, r) :> baseShape
  
  let mkBaseRectangle (bottomLeft : point) (topLeft : point) (bottomRight : point) : baseShape = 
    new BaseRectangle(bottomLeft, topLeft, bottomRight) :> baseShape
  
  let mkRectangle (bottomLeft : point) (topLeft : point) (bottomRight : point) (t : texture) : shape = 
    new Rectangle(bottomLeft, topLeft, bottomRight, t) :> shape
  
  let mkTriangle (a:point) (b:point) (c:point) (m : material) : shape = 
    new Triangle(a, b, c, m) :> shape

  let mkPlane (m : texture) : shape = 
    new InfinitePlane(m) :> shape

  let mkImplicit (s : string) : baseShape = 
    mkImplicit s

  let mkPLY (filename : string) (smooth : bool) : baseShape = 
    TriangleMesh.drawTriangles filename smooth

  let mkHollowCylinder (c : point) (r : float) (h : float) (t : texture) : shape = 
    let s = HollowCylinder(c, r, h, t)
    Transform.transform s (Transformation.translate c.X c.Y c.Z)

  let mkSolidCylinder (c : point) (r : float) (h : float) (t : texture) (top : texture) (bottom : texture) : shape = 
    let s = SolidCylinder(c, r, h, t, top, bottom) :> shape
    Transform.transform s (Transformation.translate c.X c.Y c.Z)

  let mkDisk (c : point) (r : float) (t : texture) : shape = 
    let disc = Disc(Point.Zero, r, t) :> shape
    Transform.transform disc (Transformation.translate c.X c.Y c.Z)

  let mkBaseDisk (c : point) (r : float) : baseShape = 
    new BaseDisc(c, r) :> baseShape
 
  let mkBox (low : point) (high : point) (front : texture) (back : texture) (top : texture) (bottom : texture) (left : texture) (right : texture) : shape =
    new Box(low, high, front, back, top, bottom, left, right) :> shape

  /////////////////////////////////
  // Constructive solid geometry //
  /////////////////////////////////

  let group (s1 : shape) (s2 : shape) : shape = 
    new CSG(s1, s2, CSGOperator.Grouping) :> shape
  
  let union (s1 : shape) (s2 : shape) : shape = 
    new CSG(s1, s2, CSGOperator.Union) :> shape
  
  let intersection (s1 : shape) (s2 : shape) : shape = 
    new CSG(s1, s2, CSGOperator.Intersection) :> shape
  
  let subtraction (s1 : shape) (s2 : shape) : shape = 
    new CSG(s1, s2, CSGOperator.Subtraction) :> shape

  /////////////
  // Cameras //
  /////////////

  let mkPinholeCamera (pos : point) (look : point) (up : vector) (zoom : float) (width : float) (height : float) (pwidth : int) (pheight : int) (s : sampler) : camera = 
    new PinholeCamera(pos, look, up, zoom, width, height, pwidth, pheight, s) :> Camera

  let mkThinLensCamera (pos : point) (look : point) (up : vector) (zoom : float) (width : float) (height : float) (pwidth : int) (pheight : int) (radius : float) (fpDistance : float) (pixel : sampler) (lens : sampler) : camera =
    new ThinLensCamera(pos, look, up, zoom, width, height, pwidth, pheight, radius, fpDistance, pixel, lens) :> Camera
  
  ////////////
  // Lights //
  ////////////

  let mkLight (p : point) (c : colour) (i : float) : light = 
    new PointLight(c, i, p) :> light

  let mkDirectionalLight (d : vector) (c : colour) (i : float) : light = 
    new DirectionalLight(c, i, d) :> light

  let mkAreaLight (bs : baseShape) (m : material) (s : sampler) : light = 
    match bs with
        | :? BaseSphere -> SphereAreaLight(m, bs, s) :> light
        | :? BaseRectangle -> RectangleAreaLight(m, bs :?> BaseRectangle, s) :> light
        | :? BaseDisc -> DiscAreaLight(m, bs :?> BaseDisc, s) :> light
        | _ -> failwith "Specified baseShape type not supported for AreaLight"
  
  let mkEnvironmentLight (r : float) (tex : texture) (s : sampler) : light = 
    new EnvironmentLight(r, tex, s) :> light
  
  let mkAmbientLight (c : colour) (i : float) : ambientLight = 
    new AmbientLight(c, i)
  
  let mkAmbientOccluder (c : colour) (l : float) (lmin : float) (s : sampler) : ambientLight = 
    new AmbientOccluder(l, c, lmin, s) :> AmbientLight

  /////////////////////
  // Scene rendering //
  /////////////////////

  let mkScene (s : shape list) (l : light list) (a : ambientLight)(m : int) : scene = 
    new Scene(s, l, a, m)
  
  let renderToScreen (sc : scene) (c : camera) : unit = 
    let render = new Render(sc, c)
    render.RenderToScreen

  let renderToFile (sc : scene) (c : camera) (path : string) : unit = 
    let render = new Render(sc, c)
    render.RenderToFile path

  /////////////////////
  // Transformations //
  /////////////////////

  let translate (x : float) (y : float) (z : float) : transformation = 
    translate x y z

  let rotateX (angle : float) : transformation = 
    rotateX angle
  
  let rotateY (angle : float) : transformation = 
    rotateY angle
  
  let rotateZ (angle : float) : transformation = 
    rotateZ angle
  
  let sheareXY (distance : float) : transformation = 
    sheare(distance, 0., 0., 0., 0., 0.)
  
  let sheareXZ (distance : float) : transformation = 
    sheare(0., distance, 0., 0., 0., 0.)
  
  let sheareYX (distance : float) : transformation = 
    sheare(0., 0., distance, 0., 0., 0.)
  
  let sheareYZ (distance : float) : transformation = 
    sheare(0., 0., 0., distance, 0., 0.)
  
  let sheareZX (distance : float) : transformation = 
    sheare(0., 0., 0., 0., distance, 0.)
  
  let sheareZY (distance : float) : transformation = 
    sheare(0., 0., 0., 0., 0., distance)
  
  let scale (x : float) (y : float) (z : float) : transformation = 
    scale x y z
  
  let mirrorX : transformation = 
    scale -1. 1. 1.
  
  let mirrorY : transformation = 
    scale 1. -1. 1.
  
  let mirrorZ : transformation = 
    scale 1. 1. -1.
  
  let mergeTransformations (ts : transformation list) : transformation = 
    Transformation.mergeTransformations ts
  
  let transform (sh : shape) (tr : transformation) : shape = 
    Transform.transform sh tr
  
  let transformLight (l : light) (t : transformation) : light = 
    TransformLight.transformLight l t

  /////////////////////////////////
  // extended API for teams of 7 //
  /////////////////////////////////

  /// Type of acceleration structure
  type Acceleration = Acceleration.Acceleration
     ///// k-d tree; default
     //| KDTree
     ///// regular grid
     //| RegularGrid
     ///// bounding volume hierarchy
     //| BVH

  /// Set which type of acceleration structure to use
  let setAcceleration (accel : Acceleration) : unit = 
    Acceleration.setAcceleration accel