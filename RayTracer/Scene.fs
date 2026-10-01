namespace Tracer.Basics

open System
open System.Threading

/// Which light-transport algorithm the renderer runs.
type IntegratorKind =
    /// Whitted / classic recursive. Direct lighting, mirror and glossy
    /// reflection, refraction. No indirect diffuse illumination.
    | Classic
    /// Unidirectional path tracing with NEE, MIS and Russian roulette. Computes
    /// global illumination, and scales linearly rather than exponentially in
    /// bounce depth.
    | Path

type RenderOptions =
    { Threads: int
      TileSize: int
      Seed: int
      Transfer: string
      Acceleration: Acceleration.Acceleration option
      BvhOptions: FlatBVH.BuildOptions
      Integrator: IntegratorKind
      /// Bounce depth at which Russian roulette starts. Earlier vertices carry
      /// most of the energy, so terminating them stochastically costs more
      /// variance than it saves.
      RouletteDepth: int
      /// Run Open Image Denoise over the finished film, guided by albedo and
      /// normal buffers captured at the first non-specular hit. Silently
      /// inactive if the native library is unavailable, or for the Whitted
      /// integrator, which produces no Monte Carlo noise to remove.
      Denoise: bool
      /// Relative standard error at which a pixel stops being sampled. Zero
      /// disables adaptive sampling and every pixel gets the full budget.
      ///
      /// Defaults to OFF because it was measured as a net loss on this
      /// project's scenes: it cuts time, but variance rises faster than time
      /// falls, so the image is worse than just rendering fewer uniform samples
      /// for the same cost. Best efficiency observed was 0.82x against uniform
      /// sampling. See benchmarks/PHASE1-RESULTS.md. Enable it only on scenes
      /// with genuinely uneven variance - caustics, small bright emitters,
      /// deep shadow - which is the heterogeneity it exists to exploit.
      AdaptiveThreshold: float
      /// Samples every pixel gets before adaptive termination may apply. Too few
      /// and the variance estimate is itself noise, which stops pixels early and
      /// leaves visible blotches.
      AdaptiveMinSamples: int
      /// Linear exposure multiplier applied before the display transfer.
      Exposure: float
      CancellationToken: CancellationToken }
    static member Default =
        { Threads = Environment.ProcessorCount; TileSize = 16; Seed = 2026
          Transfer = "gamma2"; Acceleration = None; BvhOptions = { FlatBVH.defaultOptions with LeafSize = 1 }
          Integrator = Classic; RouletteDepth = 3; Denoise = false
          AdaptiveThreshold = 0.; AdaptiveMinSamples = 16; Exposure = 1.
          CancellationToken = CancellationToken.None }

type Scene(shapes: Shape list, lights: Light list, ambient : AmbientLight, maxBounces : int, ?atmosphere: Atmosphere) = 

    do
        if maxBounces < 0 || maxBounces > 32 then invalidArg (nameof maxBounces) "Bounce depth must be between 0 and 32."
        if lights |> List.exists (fun light -> light :? AmbientLight) then
            invalidArg (nameof lights) "Supply ambient lighting through the separate ambient parameter."
    let backgroundColour = new Colour(0., 0., 0.)
    let lightShapes =
        [for light in lights do
            match light with
            | :? AreaLight as area -> yield area.Shape
            | _ -> ()]
    let allShapes = shapes @ lightShapes
    let acceleration =
        match Acceleration.acceleration with
        | "KDTree" -> Acceleration.Acceleration.KDTree
        | "BVH" -> Acceleration.Acceleration.BVH
        | "RG" -> Acceleration.Acceleration.RegularGrid
        | "FlatBVH" -> Acceleration.Acceleration.FlatBVH
        | "BruteForce" -> Acceleration.Acceleration.BruteForce
        | name -> invalidOp $"Unknown acceleration strategy {name}."
    member this.Shapes = allShapes
    member this.Acceleration = acceleration
    member this.Ambient = ambient
    member this.Lights = lights
    member this.BackgroundColour = backgroundColour 
    member this.MaxBounces = maxBounces
    /// Height fog / aerial perspective. None leaves rays in vacuum, exactly as before it existed.
    member this.Atmosphere = atmosphere
    /// The atmosphere bound to this scene's lights (sky lookup and suns), built once.
    member val Fog = atmosphere |> Option.map (fun a -> AtmosphereMedium(a, lights))