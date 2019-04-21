open Tracer.Basics
open Tracer.Basics.Textures
open Tracer.Sampling.Sampling
open System.IO
open Tracer.BaseShape
open Tracer.Basics.Render
open Tracer.Basics.Transform

[<EntryPoint>]
let main _ = 
    // General settings
    Acceleration.setAcceleration Acceleration.Acceleration.KDTree
    let position = Point(0.,2.,5.)
    let lookat = Point(0.,2.,0.)
    let up = Vector(0.,1.,0.)
    let zoom = 1.
    let resX = 1000
    let resY = 700
    let width = 2.
    let height = (float(resY) / float(resX)) * width
    let maxReflectionBounces = 3
    
    //- SAMPLE SETTINGS
    // Base sampling settings
    let BASE_SAMPLE_COUNT = 4
    let BASE_SET_COUNT = 127

    // Override these if needed
    let CAM_SETS = BASE_SET_COUNT
    let VIEW_SAMPLES = 8
    let LENS_SAMPLES = 8
    let MATERIAL_SAMPLES = BASE_SAMPLE_COUNT
    let MATERIAL_SETS = BASE_SET_COUNT
    let LIGHT_SAMPLES = 8
    let LIGHT_SETS = BASE_SET_COUNT

    //- MATERIALS
    // Matte
    let matteRed = MatteMaterial(Colour.White, 1., Colour.Red, 1.)
    let matteGreen = MatteMaterial(Colour.White, 1., Colour.Green, 1.)
    let matteYellow = MatteMaterial(Colour.White, 1., Colour.Yellow, 1.)
    let matteWhite = MatteMaterial(Colour.White, 1., Colour.White, 1.)
    let matteBlue = MatteMaterial(Colour.White, 1., Colour.White, 1.)
    // Perfect
    let perfectRed = MatteReflectiveMaterial(Colour.White, 1., Colour.Red, 1., Colour.White, 1.)
    let perfectGreen = MatteReflectiveMaterial(Colour.White, 1., Colour.Red, 1., Colour.White, 1.)
    let perfectYellow = MatteReflectiveMaterial(Colour.White, 1., Colour.Yellow, 1., Colour.White, 1.)
    let perfectBlue = MatteReflectiveMaterial(Colour.White, 1., Colour.Blue, 1., Colour.White, 1.)
    let perfectWhite = MatteReflectiveMaterial(Colour.White, 1., Colour.White, 1., Colour.White, 1.)
    // Glossy
    let glossyBlue = PhongGlossyReflectiveMaterial(Colour.White, 1., Colour.Blue, 1., Colour.White, 1., Colour.White, 1., 2, 3,
                        multiJittered MATERIAL_SAMPLES MATERIAL_SETS)

    let emissive = EmissiveMaterial(Colour.White, 10000.)
    

    //- SHAPES
    let sphereRed        = SphereShape(Point(-5.,0.,2.), 0.5, mkMatTexture matteRed)
    let spherePerfectYellow     = SphereShape(Point(-2.,0.,0.), 0.5, mkMatTexture matteYellow)
    let sphereGreen      = SphereShape(Point(1.,0.,-2.), 0.5, mkMatTexture matteGreen)
    
    let matRedTex = mkMatTexture matteRed
    let matGreenTex = mkMatTexture matteGreen
    let matBlueTex = mkMatTexture matteBlue
    let matYellowTex = mkMatTexture matteYellow

    let sL = SphereShape(Point(-1., 0., -1.), 1., mkMatTexture matteRed)
    let sC = SphereShape(Point(0., 0., 0.), 1., mkMatTexture matteYellow)
    let sR = SphereShape(Point(1., 0., 1.), 1., mkMatTexture matteGreen)

    // Rectangles for testing Thin Lens.
    let thinBoxL = Box(Point(-5.5, 0., -6.), Point(-3.5, 3., -5.), matRedTex, matRedTex, matBlueTex, matBlueTex, matBlueTex, matBlueTex)
    let thinBoxC = Box(Point(-1.5, 0., -4.), Point(0.5, 3., -3.), matYellowTex, matYellowTex, matBlueTex, matBlueTex, matBlueTex, matBlueTex)
    let thinBoxR = Box(Point(2., 0., -1.), Point(4., 3., 0.), matGreenTex, matGreenTex, matBlueTex, matBlueTex, matBlueTex, matBlueTex)

    let center = Point(0.,0.,0.)
    let radius = 0.5
    let height = 2.0
    let tex = Textures.mkMatTexture(matteBlue)
    let cylinder = HollowCylinder(center, radius, height, tex)

    let sTop = SphereShape(Point(0., 0., 10.), 5., Textures.mkMatTexture matteWhite)
    let discC = Disc(Point(0., 0., 0.), 4., Textures.mkMatTexture emissive)
    let rectC = Rectangle(Point(-4., -4., 0.), Point(-4., 4., 0.), Point(4., -4., 0.), Textures.mkMatTexture emissive)
    let checker x y =
        let abs' f = if f < 0.0 then 1.0 - (f*2.0) else f * 2.0
        if (int (abs' x) + int (abs' y)) % 2 = 0
        then matteRed :> Material
        else perfectWhite :> Material
    let plane =  InfinitePlane(mkTexture(checker))

    //- CAMERA
    let camera        = PinholeCamera(position, lookat, up, zoom, width, height, resX, resY, multiJittered VIEW_SAMPLES CAM_SETS)
    //let camera          = ThinLensCamera(position, lookat, up, zoom, width, height, resX, resY, 0.3, 8.0,
    //                        multiJittered VIEW_SAMPLES CAM_SETS,
    //                        multiJittered LENS_SAMPLES CAM_SETS)
    
    //- PLAIN LIGHTS
    let lightTop = DirectionalLight(Colour.White, 1., Vector(7., 7., 7.))

    //- AREA LIGHTS
    let baseSphere = BaseSphere(Point.Zero, 1.)
    let baseRect = BaseRectangle(Point.Zero, Point(0., 1., 0.), Point(1., 0., 0.))
    let baseDisc = BaseDisc(Point.Zero, 1.)
    let lightSphere    = SphereAreaLight(emissive, baseSphere, multiJittered LIGHT_SAMPLES LIGHT_SETS)
    let lightRect      = RectangleAreaLight(emissive, baseRect, multiJittered LIGHT_SAMPLES LIGHT_SETS)
    let lightDisc      = DiscAreaLight(emissive, baseDisc, multiJittered LIGHT_SAMPLES LIGHT_SETS)

    //- FINAL
    let lights: Light list      = [lightSphere]
    //let shapes: Shape list      = [thinBoxL; thinBoxR; plane; lightSphere.Shape]
    let shapes: Shape list = [cylinder]

    let lightAmbient   = AmbientLight(Colour.White, 0.1)
    let scene = Scene(shapes, lights, lightAmbient, maxReflectionBounces)

    let render = new Render(scene, camera)
    ignore (render.RenderToFile render.RenderParallel "path")

    0