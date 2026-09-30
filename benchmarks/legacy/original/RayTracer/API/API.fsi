namespace Tracer

open Tracer.Basics.Sampling
open Tracer.Basics
open Tracer.Basics.Textures
open Transformation

module API =
  type dummy = unit

  //////////////////////
  // Type definitions //
  //////////////////////
  
  type vector = Vector
  type point = Point
  type colour = Colour
  type material = Material
  type shape = Shape
  type baseShape = BaseShape.BaseShape
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
  
  val mkVector : x:float -> y:float -> z:float -> vector
  val mkPoint : x:float -> y:float -> c:float -> point
  val fromColor : c : System.Drawing.Color -> colour
  val mkColour : r:float -> g:float -> b:float -> colour

  //////////////
  // Samplers //
  //////////////
  
  // The first argument of each sampler constructor determines how
  // many samples are in each sets where all samplers, except the
  // random sampler and the n-rooks sampler, square that argument to determine the number of
  // samples (these two just uses that argument). The second
  // argument determines the number of sample sets.

  /// Regular sampler with n^2 samples.
  val mkRegularSampler : n : int -> sampler
  
  /// Random sampler with the given number sample sets of n samples each.
  val mkRandomSampler : n : int -> sets : int -> sampler
  
  /// n-rooks sampler with the given number of sample sets of n^2 samples each
  val mkNRooksSampler : n : int -> sets : int -> sampler

  /// Jittered sampler with given number of sample sets of n^2 samples each
  val mkJitteredSampler : n : int -> sets : int -> sampler

  /// Jittered sampler with the given number of sample sets of n^2 samples each
  val mkMultiJitteredSampler : n : int -> sets : int -> sampler

  ///////////////
  // Materials //
  ///////////////

  /// Matte material
  val mkMatteMaterial :
    ca : colour ->  // ambient colour
    ka : float ->   // ambient reflection coefficient
    cd : colour ->  // diffuse colour
    kd : float ->   // diffuse reflection coefficient
    material

  /// Phong material
  val mkPhongMaterial :
    ca : colour -> // ambient colour
    ka : float ->  // ambient reflection coefficient
    cd : colour -> // diffuse colour
    kd : float ->  // diffuse reflection coefficient
    cs : colour -> // specular colour
    ks : float ->  // specular reflection coefficient
    exp : int ->   // Phong exponent
    material

  /// Matte, reflective material
  val mkMatteReflectiveMaterial :
    ca : colour ->  // ambient colour
    ka : float ->   // ambient reflection coefficient
    cd : colour ->  // diffuse colour
    kd : float ->   // diffuse reflection coefficient
    cr : colour ->  // reflective colour
    kr : float ->   // perfect reflection coefficient
    material

  /// Matte, glossy reflective material
  val mkMatteGlossyReflectiveMaterial : 
    ca : colour ->  // ambient colour
    ka : float ->   // ambient reflection coefficient
    cd : colour ->  // diffuse colour
    kd : float ->   // diffuse reflection coefficient
    cr : colour ->  // reflective colour
    kr : float ->   // glossy reflection coefficient
    expr : int ->   // Phong exponent
    s : sampler ->  // sampler for glossy reflection
    material

  /// Phong, reflective material
  val mkPhongReflectiveMaterial :
    ca : colour ->  // ambient colour
    ka : float ->   // ambient reflection coefficient
    cd : colour ->  // diffuse colour
    kd : float ->   // diffuse reflection coefficient
    cs : colour ->  // specular colour
    ks : float ->   // specular reflection coefficient
    cr : colour ->  // reflective colour
    kr : float ->   // glossy reflection coefficient
    expr : int ->   // Phong exponent for glossy reflection
    material

  /// Phong, glossy reflective material
  val mkPhongGlossyReflectiveMaterial :
    ca : colour ->  // ambient colour
    ka : float ->   // ambient reflection coefficient
    cd : colour ->  // diffuse colour
    kd : float ->   // diffuse reflection coefficient
    cs : colour ->  // specular colour
    ks : float ->   // specular reflection coefficient
    cr : colour ->  // reflective colour
    kr : float ->   // reflection coefficient
    exps : int ->   // Phong exponent for specular reflection
    expr : int ->   // Phong exponent for glossy reflection
    s : sampler ->  // sampler for glossy reflection
    material

  /// Emissive material
  val mkEmissive : colour : colour -> intensity : float -> material

  /// Transparent material
  val mkTransparent :
    cf_in : colour ->   // inner filter colour
    cf_out : colour ->  // outer filter colour
    eta_in : float ->   // inner index of refraction
    eta_out : float ->  // outer index of refraction
    material

  /// Textures are functions that take x-y coordinates and produce a
  /// material.  The x-y coordinates range over the texture space
  /// specified for the individual basic shapes (mkSphere, mkPlane
  /// etc.).
  val mkTexture : (float -> float -> material) -> texture
  
  /// Construct a texture with a constant material for each point.
  val mkMatTexture : material -> texture

  ////////////
  // Shapes //
  ////////////

  /// Construct a textured shape from a base shape.
  /// Basic shapes are textured according to the texture space given.
  val mkShape : baseShape -> texture -> shape
  
  /// Construct a sphere.
  /// texture coordinates: [0,1] X [0,1]
  val mkSphere : center : point -> radius : float -> texture -> shape

  /// Construct a sphere.
  /// texture coordinates: [0,1] X [0,1]
  val mkBaseSphere : center : point -> radius : float -> baseShape

  /// Construct a rectangle.
  /// texture coordinates: [0,1] X [0,1]
  val mkBaseRectangle : bottomLeft : point -> topLeft : point -> bottomRight : point -> baseShape

  /// Construct a rectangle.
  /// texture coordinates: [0,1] X [0,1]
  val mkRectangle : bottomLeft : point -> topLeft : point -> bottomRight : point ->  t : texture -> shape

  /// Constructe a triangle.
  val mkTriangle : a:point -> b:point -> c:point -> material -> shape

  /// Construct a plane with the equation z = 0,
  /// i.e. the x-y plane
  /// texture coordinates: R X R

  val mkPlane : texture -> shape
  /// Construct an implicit surface.
  /// texture coordinates: {(0,0)}, i.e. has only a single material
  /// The gramar for valid expressions are the following and you will need to create a parser for it
  /// x := string
  /// n := integer
  /// f := float
  /// e := e + e (addition)
  ///      e - e (subtraction)
  ///      -e    (negation)
  ///      e * e (multiplication)
  ///      e / e (division)
  ///      e^n   (exponent)
  ///      e_n   (root)
  ///      (e)   (parenthesis)
  ///      x     (variable)
  ///      n     (integer number)
  ///      f     (floating point number)
  ///
  /// Note that the expression can contain both floats and integers. The operators bind in the expected order
  /// (note that negation binds the hardest (-x^2) is (-x)^2 and not -(x^2)

  val mkImplicit : string -> baseShape
  /// Load a triangle mesh from a PLY file.
  /// texture coordinates: [0,1] X [0,1]

  val mkPLY : filename : string -> smoothShading : bool -> baseShape
  /// construct a hollow cylinder (i.e. open on both ends)
  /// texture coordinates: [0,1] X [0,1]

  val mkHollowCylinder : center : point -> radius : float -> height : float -> texture -> shape
  /// construct a solid cylinder (i.e. closed on either end by a disk)
  /// texture space: hollow cylinder part is textured like mkHollowCylinder;
  ///                top and bottom disk are textured like mkDisk

  val mkSolidCylinder : center : point -> radius : float -> height : float -> 
                        cylinder: texture -> top : texture -> bottom : texture -> shape
  /// construct a disk at point p in the plane parallel
  /// to the x-y plane
  /// texture coordinates: [0,1] X [0,1]
  val mkBaseDisk : p : point -> radius : float -> baseShape 
  val mkDisk : p : point -> radius : float -> texture -> shape
  /// construct an axis-aligned box with low being the lower left corner of the back face
  /// and high being the upper right corner of the front face
  /// textures: the six faces of the box are textured like mkRectangle
  val mkBox : low : point -> high : point -> front : texture -> back : texture ->
              top : texture -> bottom : texture -> left : texture -> right : texture  -> shape

  /////////////////////////////////
  // Constructive solid geometry //
  /////////////////////////////////
  
  /// The union of shapes s1 and s2 with all internal edges removed
  val union : 
      s1 : shape -> 
      s2 : shape -> 
      shape

  /// The intersection of shapes s1 and s2
  val intersection :
      s1 : shape -> 
      s2 : shape -> 
      shape

  /// subtracts s2 from s1 in such a way that s2's texture is maintained in the places where s1 is cut.
  val subtraction :
      s1 : shape -> 
      s2 : shape -> 
      shape

  /// groups s1 and s2 together and maintains all internal edges
  val group :
      s1 : shape -> 
      s2 : shape -> 
      shape

  /////////////
  // Cameras //
  /////////////

  /// The constructor for a pinhole camera
  val mkPinholeCamera : 
      position : point ->      // The position of the camera
      lookat : point ->        // The point that the camera is looking at
      up : vector ->           // The up-vector for the camera
      distance : float ->      // Distance to the view plane
      vpWidth : float ->       // Width of the view plane in units
      vpHeight : float ->      // Height of the view plane in units
      pixelWidth : int ->      // The horisontal resolution of the view plane  
      pixelHeight : int ->     // The vertical resolution of the view plane
      vpSampler : sampler ->   // The sampler for the view plane
      camera

  /// The constructor for a thin-lens camera
  val mkThinLensCamera : 
      position : point ->      // The position of the camera
      lookat : point ->        // The point that the camera is looking at
      up : vector ->           // The up-vector for the camera
      distance : float ->      // Distance to the view plane
      vpWidth : float ->       // Width of the view plane in units
      vpHeight : float ->      // Height of the view plane in units
      pixelWidth : int ->      // The horisontal resolution of the view plane  
      pixelHeight : int ->     // The vertical resolution of the view plane
      lensRadius : float ->    // The radius of the lens in units 
      fpDistance : float ->    // The distance to the focal plane in units
      vpSampler : sampler ->   // The sampler for the view plane
      lensSampler : sampler -> // The sampler for the camera lens
      camera

  ////////////
  // Lights //
  ////////////

  /// Point light
  val mkLight : position : point -> colour : colour -> intensity : float -> light

  /// Directional light
  val mkDirectionalLight : direction : vector -> colour : colour -> intensity : float -> light
  
  /// Area light
  val mkAreaLight : bs : baseShape -> m : material -> sm : sampler -> light

  /// Environment light
  val mkEnvironmentLight : radius : float -> tex : texture -> sm : sampler -> light

  /// Ambient light
  val mkAmbientLight : colour : colour -> intensity : float -> ambientLight
  
  /// Ambient occluder light
  val mkAmbientOccluder : colour : colour -> intensity : float -> min_intensity : float -> sm : sampler -> ambientLight

  /////////////////////
  // Scene rendering //
  /////////////////////

  /// This function assembles shapes and lights into a scene.
  val mkScene :
    shapes : shape list -> // shapes in the scene
    lights : light list -> // lights in the scene
    ambientLight ->        // the ambient light
    max_depth : int ->     // maximum depth for reflection and transparency.
    scene

  /// This function renders the given scene using the given camera and
  /// displays the resulting image on the screen.
  val renderToScreen : scene -> camera -> unit

  /// This function renders the given scene using the given camera and
  /// stores the resulting image in a file at the given file path.
  val renderToFile : scene -> camera -> filepath : string -> unit

  /////////////////////
  // Transformations //
  /////////////////////
  
  // For rotations all angles are in radians. Note that angles greater
  // than 2*pi and less than zero are possible.

  val rotateX : angle : float -> transformation
  val rotateY : angle : float -> transformation
  val rotateZ : angle : float -> transformation
  val sheareXY : distance : float -> transformation
  val sheareXZ : distance : float -> transformation
  val sheareYX : distance : float -> transformation
  val sheareYZ : distance : float -> transformation
  val sheareZX : distance : float -> transformation
  val sheareZY : distance : float -> transformation
  val scale : x : float -> y : float -> z : float -> transformation
  val translate : x : float -> y : float -> z : float -> transformation
  val mirrorX : transformation
  val mirrorY : transformation
  val mirrorZ : transformation
  
  /// Merge the givne list of transformations into one, such that the resulting
  /// transformation is equivalent to applying the individual transformations
  /// from left to right (i.e. starting with the first element in the list).
  val mergeTransformations : transformation list -> transformation

  /// Apply the given transformation to the given shape. The original
  /// shape remains unchanged; the transformed shape is returned.
  val transform : shape -> transformation -> shape

  /// Apply the given transformation to the given light. The original
  /// light remains unchanged; the transformed light is returned.
  val transformLight : light -> transformation -> light


  /////////////////////////////////
  // extended API for teams of 7 //
  /////////////////////////////////

  /// Type of acceleration structure
  type Acceleration = Acceleration.Acceleration
  //   /// k-d tree; default
  //   | KDTree
  //   /// regular grid
  //   | RegularGrid
  //   /// bounding volume hierarchy
  //   | BVH

  /// Set which type of acceleration structure to use. KDTree is the default.
  val setAcceleration : Acceleration -> unit
