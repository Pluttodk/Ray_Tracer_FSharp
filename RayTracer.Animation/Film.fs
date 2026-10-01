namespace Tracer.Animation

open System
open Tracer.Basics
open Tracer.Basics.Sampling
open Tracer.Basics.Textures
open Tracer.Animation.Stage
open Tracer.Basics.Post

// --- a: render defaults ---
/// Render settings an animation recommends; command-line flags override each of them.
type RenderDefaults =
    { Integrator: IntegratorKind
      Denoise: bool
      Transfer: string
      SamplesPerPixel: int
      MaxBounces: int
      Post: PostSettings }
// --- end a ---

/// "Dragon flight": a 30-second short in five shots. A dragon (Quaternius, CC0) crosses a sunset mountain
/// range, sweeps around the summit, hovers to roar, and flies off into the sun.
module Film =
    let assetDirectory = IO.Path.Combine("assets", "dragon")

    let private findAsset (name: string) =
        let relative = IO.Path.Combine(assetDirectory, name)
        let rec search (dir: IO.DirectoryInfo) =
            if isNull dir then None
            else
                let candidate = IO.Path.Combine(dir.FullName, relative)
                if IO.File.Exists candidate then Some candidate else search dir.Parent
        search (IO.DirectoryInfo(Environment.CurrentDirectory))
        |> Option.orElse (search (IO.DirectoryInfo(AppContext.BaseDirectory)))

    let asset name =
        findAsset name |> Option.defaultWith (fun () ->
            invalidOp $"{IO.Path.Combine(assetDirectory, name)} is missing; run scripts/fetch-dragon-assets.sh first.")

    let terrainSettings : Terrain.Settings =
        { Size = 1800.; Resolution = 840; // --- d: finer mesh, 2.1 units per cell ---
          Relief = 60.; FeatureSize = 240.
          Peaks =
            [ { X = 0.; Z = 0.; Height = 170.; Radius = 160. }
              { X = -300.; Z = -230.; Height = 150.; Radius = 170. }
              { X = 320.; Z = -260.; Height = 160.; Radius = 170. }
              { X = -380.; Z = 260.; Height = 110.; Radius = 170. }
              { X = 380.; Z = 330.; Height = 100.; Radius = 180. }
              { X = 0.; Z = -620.; Height = 230.; Radius = 260. } ]
          SnowLine = 180.; Seed = 7 }

    let ground x z = Terrain.height terrainSettings x z

    /// A coarse ring of hazy ranges around the detailed terrain, so no edge shows on the horizon. Inside the
    /// detailed area it sinks well below it; further out a wall of peaks rises to close the horizon.
    let private farTerrain () =
        let inner = terrainSettings.Size / 2.
        let settings = { terrainSettings with Size = 12000.; Resolution = 300; Peaks = []; Relief = 90.; FeatureSize = 520.; Seed = 11 }
        let heightAt x z =
            let edge = max (abs x) (abs z)
            let r = sqrt (x * x + z * z)
            let sink = Easing.smoothstep (max 0. (min 1. ((inner + 40. - edge) / 80.)))
            let wall = 380. * Easing.smoothstep (max 0. (min 1. ((r - 1400.) / 2600.)))
            Terrain.height settings x z * 1.6 + wall - 160. * sink
        Terrain.buildWith settings heightAt (Terrain.farTexture settings)

    // --- b: sky and sun ---
    /// Golden hour: the sun 6 degrees up, at azimuth -130 degrees (atan2(x, z); the north-west,
    /// towards -x -z). Most shots look that way (establish, summit approach, roar, departure), so the
    /// dragon is back- or rim-lit and departs into the sun. One fixed sun keeps continuity across cuts.
    let sunElevation = 6. * Math.PI / 180.
    let sunAzimuth = -130. * Math.PI / 180.
    let sunDirection = Vector(cos sunElevation * sin sunAzimuth, sin sunElevation, cos sunElevation * cos sunAzimuth)

    /// Preetham sky with an attenuated, limb-darkened sun disc. The sun's lighting is a DirectionalLight
    /// and the disc is drawn only for camera and specular rays, so the sun is counted once (see Sky.fs).
    let skyModel =
        Sky.create
            { SunDirection = sunDirection; Turbidity = 2.2
              SunIrradiance = 3.0; SunToSky = Some 6. }

    /// The sky as an importance-sampled environment light (2x2 classic samples, 256x128 luminance table).
    let private sky () = Sky.light skyModel 2 256 :> Light

    let private sunLight () = Sky.sunLight skyModel :> Light
    // --- end b ---

    // --- c: atmosphere ---
    /// Height fog for aerial perspective: dense in the valleys, thinning with altitude, glowing around the sun.
    /// Near ranges stay crisp; each ridge further out is lighter and closer to the horizon colour behind it.
    let private atmosphere () =
        Some { Atmosphere.Default with
                 Density = 2.6e-4; BaseHeight = 0.; ScaleHeight = 200.; HorizonLift = 0.08
                 Tint = Colour(0.8, 1., 1.35)
                 Anisotropy = 0.7; SkyWeight = 0.8; SunWeight = 0.6
                 MaxDistance = 7000. }
    // --- c: end ---

    // ------------------------------------------------------------------ timing

    let duration = 30.
    /// Shot boundaries (seconds) and the camera for each.
    let shots = [ 0., "cam-establish"; 7., "cam-track"; 13., "cam-summit"; 19., "cam-roar"; 25., "cam-depart" ]
    /// The instant the roar peaks (the lunge of the Headbutt clip), for the soundtrack.
    let roarTime = 21.35

    /// The dragon's flight path (world positions of its body).
    let private flightPath =
        Smooth.vector
            [ 0.0, Vector(470., 168., 40.)
              3.5, Vector(330., 166., -55.)
              7.0, Vector(170., 172., -140.)
              10.0, Vector(-40., 190., -175.)
              // --- s: summit pass skims the sunlit south-west face ---
              // Cut in lower and further south (was (-170, 185, -40)), dive to 50-60 units over the face that
              // rises away from the 6-degree sun, so the dragon's shadow races up the slope 40-70 units ahead
              // of it (about 14.2-16.5 s); then over the ridge east of the summit and down to the hover.
              13.0, Vector(-170., 180., -55.)
              14.41, Vector(-137., 149., -49.)
              15.42, Vector(-89., 163., -64.)
              16.33, Vector(-27., 196., -56.)
              17.12, Vector(23., 219., -15.)
              18.4, Vector(83., 220., 89.)
              // --- s: end ---
              19.5, Vector(96., 210., 128.)
              24.5, Vector(92., 212., 131.)
              26.5, Vector(40., 222., 150.)
              28.3, Vector(-120., 240., 110.)
              30.0, Vector(-300., 262., 40.) ]

    let dragonPosition t = Sampler.evaluateVector flightPath t

    /// Where the hovering dragon looks (the roar camera).
    let private hoverPoint = Vector(94., 211., 130.)
    let private roarCamera = hoverPoint + Vector(4., 3., 46.)

    /// 0 while travelling, 1 while hovering (19.5 s to 24.5 s), with smooth transitions.
    let private hover t =
        let ramp a b x = Easing.smoothstep (max 0. (min 1. ((x - a) / (b - a))))
        ramp 18.3 19.6 t * (1. - ramp 24.4 25.6 t)

    // ------------------------------------------------------------------ dragon

    let private dragonScale = 4.2

    /// Flight orientation baked at `rate` Hz: face along the velocity, bank into turns (roll from lateral
    /// acceleration), face the camera while hovering, and pitch the body forward with speed.
    let private flightChannels rate =
        let dt = 1. / rate
        let times = [ for i in 0 .. int (duration * rate) -> float i * dt ]
        let velocity t = (dragonPosition (t + 0.05) - dragonPosition (t - 0.05)) / 0.1
        let up = Vector(0., 1., 0.)
        let horizontal (v: Vector) = Vector(v.X, 0., v.Z)
        let heading t =
            let travel = let v = horizontal (velocity t) in if v.Magnitude > 1e-6 then v.Normalise else Vector(0., 0., 1.)
            let face = (horizontal (roarCamera - dragonPosition t)).Normalise
            let h = hover t
            let blended = (1. - h) * travel + h * face
            if blended.Magnitude > 1e-6 then blended.Normalise else face
        let orientation =
            times |> List.map (fun t ->
                let forward = heading t
                let a = (velocity (t + 0.1) - velocity (t - 0.1)) / 0.2
                let right = (forward % up).Normalise
                let lateral = a * right
                let roll = (1. - hover t) * Math.Clamp(atan2 lateral 9.81 * 0.9, -0.8, 0.8)
                let bankedUp = (cos roll * up + sin roll * right).Normalise
                t, Quaternion.lookRotation (-forward) bankedUp)
        let pitch =
            times |> List.map (fun t ->
                let v = velocity t
                let climb = atan2 v.Y (max 1. (horizontal v).Magnitude)
                // Upright when hovering, near horizontal in cruise, nose following climbs and dives.
                let angle = (1. - hover t) * (1.35 - 0.6 * climb) + hover t * 0.2
                t, Quaternion.ofAxisAngle (Vector(1., 0., 0.)) angle)
        [ Clip.translate "dragon" (Sampler.linear [ for t in times -> t, dragonPosition t ])
          Clip.rotate "dragon" (Sampler.linear orientation)
          Clip.rotate "dragon-pitch" (Sampler.linear pitch) ]

    // --- e: dragon materials ---
    /// Material overrides that make the stylized model read as a creature: darker, desaturated scales with a
    /// procedural cellular bump, translucent wing membranes for back light, bone-coloured horns and a wet eye.
    /// Keyed by the glTF material names in dragon_evolved.glb.
    module DragonMaterials =
        /// Scales per unit of generated texture coordinate (one unit spans the model's largest side).
        let scaleDensity = 110.
        /// Steepness of the scale bump.
        let bumpStrength = 0.55

        let private hash (i: int) (j: int) =
            let mutable h = uint32 i * 0x8DA6B343u ^^^ uint32 j * 0xD8163841u
            h <- (h ^^^ (h >>> 15)) * 0x2C1B3C6Du
            h <- (h ^^^ (h >>> 12)) * 0x297A2D39u
            h ^^^ (h >>> 15)

        let private unit (h: uint32) = float (h >>> 8) / float (1u <<< 24)

        /// Cellular noise at (x, y) in cell units: distance to the nearest feature point, distance to the
        /// second nearest, and a random value per nearest cell.
        let voronoi (x: float) (y: float) =
            let ix, iy = int (floor x), int (floor y)
            let mutable f1 = infinity
            let mutable f2 = infinity
            let mutable cell = 0.
            for dj in -1 .. 1 do
                for di in -1 .. 1 do
                    let h = hash (ix + di) (iy + dj)
                    let px = float (ix + di) + 0.15 + 0.7 * unit h
                    let py = float (iy + dj) + 0.15 + 0.7 * unit (h * 0x9E3779B1u + 0x7F4A7C15u)
                    let d = sqrt ((x - px) * (x - px) + (y - py) * (y - py))
                    if d < f1 then
                        f2 <- f1
                        f1 <- d
                        cell <- unit (h ^^^ 0xA511E9B3u)
                    elif d < f2 then f2 <- d
            struct (f1, f2, cell)

        /// Scale height in [0,1]: a rounded dome per cell with grooves along the cell borders.
        let height (x: float) (y: float) =
            let struct (f1, f2, _) = voronoi x y
            let groove = Easing.smoothstep (min 1. ((f2 - f1) / 0.18))
            let dome = max 0. (1. - f1 * f1 * 1.6)
            groove * (0.55 + 0.45 * dome)

        /// Tangent-space normal of the scale relief, by central differences.
        let scaleNormal (u: float) (v: float) =
            let x, y = u * scaleDensity, v * scaleDensity
            let d = 0.04
            let dx = (height (x + d) y - height (x - d) y) / (2. * d)
            let dy = (height x (y + d) - height x (y - d)) / (2. * d)
            Vector(-bumpStrength * dx, -bumpStrength * dy, 1.).Normalise

        /// Albedo modulation: darker grooves and a little per-scale variation, so the pattern also reaches the
        /// denoiser's albedo guide.
        let scaleTint (u: float) (v: float) =
            let x, y = u * scaleDensity, v * scaleDensity
            let struct (f1, f2, cell) = voronoi x y
            let groove = Easing.smoothstep (min 1. ((f2 - f1) / 0.18))
            let k = (0.88 + 0.24 * cell) * (0.68 + 0.32 * groove)
            Colour(k, k, k)

        let private desaturate (amount: float) (darken: float) (c: Colour) =
            let l = 0.2126 * c.R + 0.7152 * c.G + 0.0722 * c.B
            let mix x = (x + (l - x) * amount) * darken
            Colour(mix c.R, mix c.G, mix c.B)

        /// The wing membranes are thin slabs (skin on top, membrane below) bound to the Wing joints; the body
        /// is a closed shell. Translucency must stay on the wings or the whole torso lights up like a lantern.
        let region (_: string) (joint: string) = if joint.StartsWith "Wing" then Some "wing" else None

        let private skin (p: PbrParams) =
            { p with
                BaseColour = desaturate 0.55 0.72 p.BaseColour
                BaseColourMap = Some scaleTint
                Metallic = 0.; Roughness = 0.55
                NormalMap = Some scaleNormal; NormalScale = 1.
                Sheen = 0.35; SheenColour = Colour(0.75, 0.68, 0.6) }

        let private membrane (p: PbrParams) =
            { p with
                BaseColour = desaturate 0.3 1.1 p.BaseColour
                Metallic = 0.; Roughness = 0.6
                Sheen = 0.2; SheenColour = Colour(0.7, 0.6, 0.6) }

        let apply (name: string) (p: PbrParams) =
            match name with
            | "Dragon_Main" -> skin p
            | "Dragon_Main:wing" ->
                // Scaled skin on top of the membrane, thin enough to let warm back light through.
                { skin p with DiffuseTransmission = 0.4; DiffuseTransmissionColour = Colour(0.85, 0.42, 0.22); NormalScale = 0.6 }
            | "Dragon_Secondary" -> membrane p
            | "Dragon_Secondary:wing" ->
                { membrane p with DiffuseTransmission = 0.5; DiffuseTransmissionColour = Colour(0.9, 0.5, 0.28) }
            | "Dragon_Horn" ->
                { p with BaseColour = Colour(0.36, 0.32, 0.26); Metallic = 0.; Roughness = 0.42 }
            | "Eye_White" ->
                // An amber iris under a wet, nearly mirror-smooth cornea.
                { p with BaseColour = Colour(0.5, 0.27, 0.04); Metallic = 0.; Roughness = 0.04; Ior = 1.45 }
            | "Eye_Black" ->
                { p with BaseColour = Colour(0.01, 0.01, 0.012); Metallic = 0.; Roughness = 0.03; Ior = 1.45 }
            | _ -> { p with Metallic = 0. }
    // --- e: end ---

    /// The imported dragon under a flight rig, and its performance (wing beats, hover, roar).
    let private dragon () =
        let imported = (Gltf.load (asset "dragon_evolved.glb") { Gltf.ImportOptions.Default with Clip = None; MaterialOverride = DragonMaterials.apply; MaterialRegion = DragonMaterials.region (* e *) }).Scene
        let model = imported.Roots |> List.filter (fun node -> node.Name <> imported.ActiveCamera)
        let clip name = imported.Clips |> List.find (fun c -> c.Name = "CharacterArmature|" + name)
        let rest =
            let table = AnimatedScene.nodes imported |> Seq.map (fun n -> n.Name, n.Rest) |> dict
            fun name -> table.[name]
        let loop name start speed blend = { Clip.segment (clip name) start with Loop = true; Speed = speed; Blend = blend }
        let performance =
            Clip.arrange "performance" rest
                [ loop "Fast_Flying" 0. 0.95 0.
                  loop "Fast_Flying" 12.8 1.15 0.4      // powering up for the summit pass
                  loop "Flying_Idle" 18.6 1.0 0.7       // settle into a hover
                  { Clip.segment (clip "Headbutt") 20.6 with Offset = 0.15; Speed = 0.8; Blend = 0.35 }
                  loop "Flying_Idle" 22.5 1.0 0.5
                  loop "Fast_Flying" 24.8 1.1 0.6 ]
                duration 60.
        let rig =
            Node.create "dragon"
            |> Node.withChildren
                [ Node.create "dragon-pitch"
                  |> Node.withChildren
                      [ Node.create "dragon-model"
                        |> Node.withRest { Trs.identity with Scale = Vector(dragonScale, dragonScale, dragonScale); Translation = Vector(0., -3.5, 0.) }
                        |> Node.withChildren model ] ]
        rig, performance

    // ------------------------------------------------------------------ cameras

    let private above x z lift = Vector(x, ground x z + lift, z)

    let private cameras () =
        let camera name yfov target = Node.create name |> Node.withContent [ CameraRig { CameraSpec.Default with YFov = yfov; Target = Some target } ]
        let establishing = camera "cam-establish" 0.5 "aim-establish"
        let tracking = camera "cam-track" 0.62 "aim-dragon"
        let summit = camera "cam-summit" 0.6 "aim-dragon"  // s: tighter lens for the summit pass
        let roar = camera "cam-roar" 0.42 "aim-roar"
        let depart = camera "cam-depart" 0.7 "aim-dragon"
        // Low on a foothill, panning as the dragon crosses in front of the massif.
        let establishPath = Smooth.vector [ 0., above 330. 250. 30.; 7., above 318. 232. 30. ]
        // Tracking: beside and slightly behind the dragon on its far flank, looking into the sun for rim light.
        let trackKeys =
            [ for i in 0 .. 12 ->
                let t = 7. + 6. * float i / 12.
                let p = dragonPosition t
                let v = (dragonPosition (t + 0.1) - dragonPosition (t - 0.1)).Normalise
                let side = (v % Vector(0., 1., 0.)).Normalise
                let u = float i / 12.
                t, p - (40. - 10. * u) * side - (12. - 20. * u) * v + Vector(0., 7. - 3. * u, 0.) ]
        let roarPath = Smooth.vector [ 19., roarCamera + Vector(6., -1., 12.); 21., roarCamera; 25., roarCamera + Vector(-2., 0., -3.) ]
        // --- s: summit camera ---
        // High off the south face, drifting east with the climb: it looks down on the sunlit slope, so the
        // dragon and its shadow racing up the face share the frame, with the low sun off to the left.
        let summitPath = Smooth.vector [ 13., Vector(-70., 212., -150.); 19., Vector(-20., 220., -140.) ]
        // --- s: end ---
        let departPath = Smooth.vector [ 25., Vector(150., 222., 205.); 30., Vector(135., 228., 190.) ]
        let aims =
            [ Node.create "aim-establish"; Node.create "aim-dragon"; Node.create "aim-roar" ]
        let channels =
            [ Clip.translate "cam-establish" establishPath
              Clip.translate "aim-establish" (Smooth.vector [ 0., Vector(390., 168., 10.); 7., Vector(130., 176., -115.) ])
              Clip.translate "cam-track" (Smooth.vector trackKeys)
              Clip.translate "aim-dragon" (Sampler.linear [ for i in 0 .. int (duration * 30.) -> let t = float i / 30. in t, dragonPosition t ])
              Clip.translate "cam-summit" summitPath  // s: summit camera path
              Clip.translate "cam-roar" roarPath
              Clip.translate "aim-roar" (Smooth.vector [ 19., hoverPoint + Vector(2., 1., -2.); 21.3, hoverPoint + Vector(0., 1., 1.); 25., hoverPoint + Vector(-2., 2., 1.) ])
              Clip.translate "cam-depart" departPath ]
        [ establishing; tracking; summit; roar; depart ] @ aims, Clip.create "cameras" channels

    // --- fx: particles ---
    /// Snow spindrift lifted by the downwash while the dragon skims the sunlit face (summit pass).
    module Spindrift =
        let private source : Particles.Source =
            { Position = dragonPosition
              Ground = ground
              Snow = fun x z ->
                  // The terrain texture's own snow mask (specular = 0.02 + 0.2 x snow cover).
                  let h = 1.
                  let nx = (ground (x + h) z - ground (x - h) z) / (2. * h)
                  let nz = (ground x (z + h) - ground x (z - h)) / (2. * h)
                  let steepness = 1. - 1. / sqrt (1. + nx * nx + nz * nz)
                  let _, specular = Terrain.surfaceAt terrainSettings x z (ground x z) steepness
                  max 0. (min 1. ((specular - 0.02) / 0.2)) }
        let shapes t = Particles.shapes Particles.defaults source t
    // --- fx: end ---

    // ------------------------------------------------------------------ scene

    let build () =
        let rig, performance = dragon ()
        let cameraNodes, cameraClip = cameras ()
        let terrain = Node.create "terrain" |> Node.withContent [ Geometry(Terrain.build terrainSettings); Geometry(farTerrain ()) ]
        let sun = sunLight ()  // b: sun light
        // --- fx: particles ---
        let particles = Node.create "spindrift" |> Node.withContent [ Procedural Spindrift.shapes ]
        // --- fx: end ---
        { Name = "dragon-flight"
          Roots = [ terrain; rig; particles ] @ cameraNodes
          Clips = [ Clip.create "flight" (flightChannels 30.); performance; cameraClip ]
          ActiveCamera = snd shots.Head
          Cuts = shots.Tail
          StaticShapes = []
          StaticLights = [ sun; sky () ]
          Ambient = AmbientLight(Colour.White, 0.)
          MaxBounces = 2
          Duration = duration
          Atmosphere = atmosphere () }

/// The film's soundtrack, derived from the animation itself: wing beats where the wings actually beat,
/// whooshes where the dragon actually passes the camera, all panned and attenuated from the active camera.
module FilmSound =
    open Audio

    /// Mono samples for one wing beat: a low "whoomp" of displaced air, a leathery flutter and a thump.
    let private wingBeat (noise: Noise) (strength: float) =
        let n = int (0.42 * float sampleRate)
        let air, flutter = Biquad(), Biquad()
        air.BandPass(170. + 90. * noise.Next(), 0.7)
        flutter.BandPass(900. + 250. * noise.Next(), 1.4)
        Array.init n (fun i ->
            let t = float i / float sampleRate
            let attack = min 1. (t / 0.03)
            let body = attack * exp (-t / 0.11)
            let leather = attack * exp (-t / 0.035)
            let thump = sin (2. * Math.PI * 58. * t) * exp (-t / 0.09)
            strength * (1.6 * air.Process(noise.Next()) * body + 0.35 * flutter.Process(noise.Next()) * leather + 0.45 * thump * attack))

    /// A pass-by: band-passed noise whose centre rises and falls around the closest approach.
    let private whoosh (noise: Noise) (length: float) =
        let n = int (length * float sampleRate)
        let band = Biquad()
        Array.init n (fun i ->
            let u = float i / float n
            let shape = exp (-((u - 0.55) * (u - 0.55)) / 0.03)
            if i % 64 = 0 then band.BandPass(350. + 1400. * shape, 0.9)
            band.Process(noise.Next()) * shape * 2.2)

    let private lowPass frequency (samples: float[]) =
        let f = Biquad()
        f.LowPass(frequency, 0.707)
        samples |> Array.map f.Process

    let build (scene: AnimatedScene) (load: string -> Buffer) =
        let length = scene.Duration
        let mix = silence length
        let noise = Noise(0xD2A60UL)
        // Spatial tables at 100 Hz: the dragon's body and the active camera.
        let rate = 100.
        let count = int (length * rate) + 2
        let dragon = Array.zeroCreate<Tracer.Basics.Point> count
        let wingHeight = Array.zeroCreate<float> count
        let cameras = Array.init count (fun i -> AnimatedScene.cameraPose scene (float i / rate))
        for i in 0 .. count - 1 do
            let w = AnimatedScene.worldMatrices scene (float i / rate)
            let at (name: string) = let m = w.[name] in Tracer.Basics.Point(m.Pos1x4, m.Pos2x4, m.Pos3x4)
            dragon.[i] <- at "Torso"
            let m = w.["dragon-model"]
            let up = Tracer.Basics.Vector(m.Pos1x2, m.Pos2x2, m.Pos3x2).Normalise
            wingHeight.[i] <- ((at "Wing4.L_end" - at "Torso") * up + (at "Wing4.R_end" - at "Torso") * up) / 2.
        let index t = max 0 (min (count - 1) (int (round (t * rate))))
        let distance t = let c = cameras.[index t] in (dragon.[index t] - c.Position).Magnitude
        let pan t =
            let c = cameras.[index t]
            let forward = (c.LookAt - c.Position).Normalise
            let right = (forward % c.Up).Normalise
            0.85 * ((dragon.[index t] - c.Position).Normalise * right)
        let proximity reference t = Math.Clamp(reference / max 1. (distance t), 0.02, 1.6)
        let nearCut t = scene.Cuts |> List.exists (fun (cut, _) -> abs (t - cut) < 0.3)

        // Wind: brown-ish noise with slow gusts, louder when the camera is moving fast or high up.
        let gust = Noise(0x6057UL)
        let leftWind, rightWind, gustFilter = Biquad(), Biquad(), Biquad()
        let mutable gustLevel = 0.5
        let mutable previous = cameras.[0].Position
        for i in 0 .. mix.Left.Length - 1 do
            let t = float i / float sampleRate
            if i % 480 = 0 then
                let c = cameras.[index t]
                let speed = (c.Position - previous).Magnitude * 100.
                previous <- c.Position
                gustLevel <- 0.35 + 0.65 * Terrain.valueNoise (t * 0.35) 0.5 3
                let cutoff = 220. + 700. * gustLevel + 12. * min 60. speed
                leftWind.LowPass(cutoff, 0.6)
                rightWind.LowPass(cutoff * 1.07, 0.6)
                gustFilter.LowPass(2., 0.7)
            let level = 0.3 * gustLevel + 0.005 * min 60. ((cameras.[index t].Position - cameras.[index (t - 0.1)].Position).Magnitude * 10.)
            let fade = min 1. (t / 1.5) * min 1. ((length - t) / 1.2)
            mix.Left.[i] <- mix.Left.[i] + fade * level * leftWind.Process(gust.Next())
            mix.Right.[i] <- mix.Right.[i] + fade * level * rightWind.Process(gust.Next())

        // Wing beats at each top-of-stroke, scaled by how big the stroke is.
        let heights = wingHeight
        let window = int (0.12 * rate)
        for i in window .. count - window - 1 do
            let h = heights.[i]
            let isPeak = seq { i - window .. i + window } |> Seq.forall (fun j -> heights.[j] <= h)
            if isPeak then
                let trough = seq { i .. min (count - 1) (i + int (0.5 * rate)) } |> Seq.map (fun j -> heights.[j]) |> Seq.min
                let stroke = h - trough
                if stroke > 0.8 then
                    let t = float i / rate + 0.07
                    let beat = wingBeat noise (min 1.2 (stroke / 6.))
                    mixMono mix t beat (fun x -> 0.55 * proximity 45. x) pan

        // Whooshes at fast, close passes (ignoring distance jumps caused by camera cuts, and a camera that is
        // flying alongside, where nothing rushes past).
        let relativeSpeed t =
            let before, after = index (t - 0.1), index (t + 0.1)
            ((dragon.[after] - cameras.[after].Position) - (dragon.[before] - cameras.[before].Position)).Magnitude / 0.2
        for i in 1 .. count - 2 do
            let t = float i / rate
            let d = distance t
            if d < 45. && d <= distance (t - 1. / rate) && d < distance (t + 1. / rate) && not (nearCut t) && relativeSpeed t > 15. then
                mixMono mix (t - 0.75) (whoosh noise 1.4) (fun x -> 0.6 * proximity 18. x) pan

        // Roars: the recorded grizzly, pitched down into dragon range, thickened with a pitched-down alligator
        // bellow and a sub rumble, then sent into a large reverb for the mountain echo.
        let grizzly = toMono (load "grizzly_roar.mp3")
        let bellow = toMono (load "alligator_bellow.ogg")
        /// Keeps `before`..`after` seconds around the loudest moment, with short fades; returns (samples, peak).
        let crop (samples: float[]) before after =
            let peak = loudest samples 0.05
            let a = max 0 (int ((peak - before) * float sampleRate))
            let b = min samples.Length (int ((peak + after) * float sampleRate))
            let fade = int (0.15 * float sampleRate)
            let clip = Array.sub samples a (b - a)
            let n = clip.Length
            clip |> Array.mapi (fun i x -> x * min 1. (float i / float fade) * min 1. (float (n - 1 - i) / float fade)), peak - float a / float sampleRate
        let roar pitch =
            let g, gPeak = crop (repitch pitch grizzly) 1.3 2.6
            let b, bPeak = crop (lowPass 320. (repitch (pitch * 1.15) bellow)) 0.9 1.8
            // Align the bellow's loudest moment with the roar's, and add a short sub-bass swell under it.
            let result = Array.copy g
            let shift = int ((gPeak - bPeak) * float sampleRate)
            for i in 0 .. result.Length - 1 do
                let j = i - shift
                if j >= 0 && j < b.Length then result.[i] <- result.[i] + 0.8 * b.[j]
                let t = float i / float sampleRate - gPeak
                if abs t < 1.2 then result.[i] <- result.[i] + 0.25 * sin (2. * Math.PI * 42. * t) * exp (-(t * t) / 0.25)
            result, gPeak
        let place (samples: float[], peak: float) (at: float) gain wet room =
            mixMono mix (at - peak) samples (fun x -> gain * proximity 40. x) pan
            let echo = reverb room 0.35 samples 3.5
            mixStereo mix (at - peak) echo (gain * wet)
        place (roar 0.72) Film.roarTime 2.4 0.55 1.4
        // Distant calls, mostly echo: one in the establishing shot, one as it leaves.
        let distant = let s, p = roar 0.62 in lowPass 1600. s, p
        place distant 4.6 0.3 0.8 1.8
        place distant 28.6 0.22 0.8 1.8

        master mix 0.89

/// Every built-in animation: the small demos plus the short film.
module Catalog =
    let all =
        Demos.all
        @ [ { Demos.Demo.Name = "dragon-flight"
              Demos.Demo.Description = "30 s short: a dragon over sunset mountains (needs scripts/fetch-dragon-assets.sh)"
              Demos.Demo.Build = Film.build } ]

    let tryFind name = all |> List.tryFind (fun demo -> demo.Name = name)

    // --- a: render defaults ---
    /// Recommended render settings by animation name (none: the runner's own defaults).
    let renderDefaults (name: string) : RenderDefaults option =
        match name with
        | "dragon-flight" ->
            Some { Integrator = Path; Denoise = true; Transfer = "aces"; SamplesPerPixel = 64; MaxBounces = 3
                   Post = { Bloom = 0.12; BloomThreshold = 1.; Vignette = 0.2; Exposure = 1.; WhiteBalance = 0.; Saturation = 1.1 } }
        | _ -> None
    // --- end a ---

    /// Soundtracks by animation name: given the scene and a loader for audio assets (decoded to 48 kHz stereo).
    let soundtrack (name: string) : (AnimatedScene -> (string -> Audio.Buffer) -> Audio.Buffer) option =
        match name with
        | "dragon-flight" -> Some FilmSound.build
        | _ -> None
