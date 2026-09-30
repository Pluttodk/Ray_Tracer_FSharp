namespace Tracer.SceneFormat

open System
open System.IO
open System.Security.Cryptography
open System.Text.Json
open System.Text.Json.Serialization

type MaterialKind =
    | Matte = 0
    | Phong = 1
    | Mirror = 2
    | Glossy = 3
    | Glass = 4
    | Emissive = 5

type LightKind =
    | Point = 0
    | Directional = 1
    | Rectangle = 2
    | Environment = 3

[<CLIMutable>]
type MaterialSpec =
    { Id: string
      Kind: MaterialKind
      Colour: float array
      AmbientColour: float array
      SpecularColour: float array
      ReflectionColour: float array
      Ambient: float
      Diffuse: float
      Specular: float
      Exponent: int
      Reflectivity: float
      GlossExponent: int
      Ior: float
      Filter: float array
      Emission: float
      Texture: string }

[<CLIMutable>]
type MeshSpec =
    { Id: string
      Path: string
      Smooth: bool
      Closed: bool }

[<CLIMutable>]
type ObjectSpec =
    { Id: string
      Mesh: string
      Material: string
      Transform: float array }

[<CLIMutable>]
type LightSpec =
    { Id: string
      Kind: LightKind
      Position: float array
      Direction: float array
      Colour: float array
      Intensity: float
      Size: float array }

[<CLIMutable>]
type CameraSpec =
    { Position: float array
      Target: float array
      Up: float array
      ViewDistance: float
      ViewWidth: float
      ViewHeight: float
      LensRadius: float
      FocusDistance: float }

[<CLIMutable>]
type SceneSpec =
    { SchemaVersion: int
      Id: string
      Title: string
      Description: string
      Camera: CameraSpec
      AmbientColour: float array
      AmbientIntensity: float
      MaxBounces: int
      Materials: MaterialSpec array
      Meshes: MeshSpec array
      Objects: ObjectSpec array
      Lights: LightSpec array
      Subjects: string array }

[<CLIMutable>]
type RenderSettings =
    { Width: int
      Height: int
      CameraSamples: int
      LightSamples: int
      GlossySamples: int
      MaxBounces: int
      Threads: int
      TileSize: int
      Seed: int
      Precision: string
      Sampler: string
      Transfer: string }

[<CLIMutable>]
type PhaseTimings =
    { LoadMs: float
      BuildMs: Nullable<float>
      CompileMs: Nullable<float>
      UploadMs: Nullable<float>
      TraceMs: float
      DownloadMs: Nullable<float>
      EncodeMs: float
      CleanupMs: float
      TotalMs: float }

[<CLIMutable>]
type RenderMetrics =
    { SchemaVersion: int
      Engine: string
      Backend: string
      Device: string
      Scene: string
      Material: string
      Status: string
      Error: string
      Settings: RenderSettings
      Timings: PhaseTimings
      AllocatedBytes: Nullable<int64>
      PeakWorkingSetBytes: int64
      PeakDeviceBytes: Nullable<int64>
      GcCollections: int array
      InvalidPixels: int
      OutputPath: string
      LinearPath: string }

module SceneFiles =
    let jsonOptions =
        let options = JsonSerializerOptions(WriteIndented = true)
        options.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
        options.PropertyNameCaseInsensitive <- true
        options.UnmappedMemberHandling <- JsonUnmappedMemberHandling.Disallow
        options.Converters.Add(JsonStringEnumConverter(JsonNamingPolicy.CamelCase))
        options

    let private require condition message =
        if not condition then raise (InvalidDataException message)

    let private vector length name (values: float array) =
        require (not (isNull values) && values.Length = length) $"{name} must have {length} components."
        require (Array.forall Double.IsFinite values) $"{name} contains a non-finite value."

    let private colour name values =
        vector 3 name values
        require (Array.forall (fun value -> value >= 0.) values) $"{name} must be nonnegative."

    let private nonnegative name value =
        require (Double.IsFinite value && value >= 0.) $"{name} must be finite and nonnegative."

    let private positive name value =
        require (Double.IsFinite value && value > 0.) $"{name} must be finite and positive."

    let private identifiers name (ids: string array) =
        require (not (isNull ids)) $"{name} must be an array."
        require (Array.forall (String.IsNullOrWhiteSpace >> not) ids) $"{name} contains an empty identifier."
        require ((Array.distinct ids).Length = ids.Length) $"{name} contains duplicate identifiers."
        Set.ofArray ids

    let validate (scene: SceneSpec) =
        require (not (obj.ReferenceEquals(scene, null))) "Scene must be an object."
        require (scene.SchemaVersion = 1) "Unsupported scene schemaVersion; expected 1."
        require (not (String.IsNullOrWhiteSpace scene.Id)) "Scene id is required."
        require (not (obj.ReferenceEquals(scene.Camera, null))) "Camera is required."
        vector 3 "camera.position" scene.Camera.Position
        vector 3 "camera.target" scene.Camera.Target
        vector 3 "camera.up" scene.Camera.Up
        require (scene.Camera.Position <> scene.Camera.Target) "Camera position and target must differ."
        require (Array.exists ((<>) 0.) scene.Camera.Up) "Camera up vector must be nonzero."
        positive "camera.viewDistance" scene.Camera.ViewDistance
        positive "camera.viewWidth" scene.Camera.ViewWidth
        positive "camera.viewHeight" scene.Camera.ViewHeight
        nonnegative "camera.lensRadius" scene.Camera.LensRadius
        positive "camera.focusDistance" scene.Camera.FocusDistance
        colour "ambientColour" scene.AmbientColour
        nonnegative "ambientIntensity" scene.AmbientIntensity
        require (scene.MaxBounces >= 0 && scene.MaxBounces <= 32) "maxBounces must be between 0 and 32."
        require (not (isNull scene.Materials)) "materials must be an array."
        require (not (isNull scene.Meshes)) "meshes must be an array."
        require (not (isNull scene.Objects)) "objects must be an array."
        require (not (isNull scene.Lights)) "lights must be an array."
        for material in scene.Materials do
            require (not (obj.ReferenceEquals(material, null))) "A material must not be null."
            require (Enum.IsDefined(typeof<MaterialKind>, material.Kind)) $"Unknown material kind for {material.Id}."
            colour $"material {material.Id} colour" material.Colour
            for name, values in [ "ambientColour", material.AmbientColour; "specularColour", material.SpecularColour; "reflectionColour", material.ReflectionColour ] do
                if not (isNull values) then colour $"material {material.Id} {name}" values
            colour $"material {material.Id} filter" material.Filter
            require (Array.forall ((>=) 1.) material.Filter) $"Material {material.Id} filter must not exceed 1."
            nonnegative "ambient" material.Ambient
            nonnegative "diffuse" material.Diffuse
            nonnegative "specular" material.Specular
            nonnegative "reflectivity" material.Reflectivity
            require (material.Reflectivity <= 1.) "reflectivity must not exceed 1."
            require (material.Exponent >= 0 && material.GlossExponent >= 0) "Material exponents must be nonnegative."
            positive "ior" material.Ior
            nonnegative "emission" material.Emission
        let materials = scene.Materials |> Array.map (fun material -> material.Id) |> identifiers "materials"
        for mesh in scene.Meshes do
            require (not (obj.ReferenceEquals(mesh, null))) "A mesh must not be null."
            require (not (String.IsNullOrWhiteSpace mesh.Path)) $"Mesh {mesh.Id} path is required."
        let meshes = scene.Meshes |> Array.map (fun mesh -> mesh.Id) |> identifiers "meshes"
        for instance in scene.Objects do
            require (not (obj.ReferenceEquals(instance, null))) "An object must not be null."
            require (Set.contains instance.Mesh meshes) $"Unknown mesh {instance.Mesh} on {instance.Id}."
            require (Set.contains instance.Material materials) $"Unknown material {instance.Material} on {instance.Id}."
            vector 16 $"object {instance.Id} transform" instance.Transform
            require (instance.Transform.[12..15] = [| 0.; 0.; 0.; 1. |]) $"Object {instance.Id} requires an affine row-major transform."
            let m = instance.Transform
            let det =
                m.[0] * (m.[5] * m.[10] - m.[6] * m.[9])
                - m.[1] * (m.[4] * m.[10] - m.[6] * m.[8])
                + m.[2] * (m.[4] * m.[9] - m.[5] * m.[8])
            require (Double.IsFinite det && det <> 0.) $"Object {instance.Id} transform is singular."
        let objects = scene.Objects |> Array.map (fun instance -> instance.Id) |> identifiers "objects"
        identifiers "subjects" scene.Subjects |> Set.iter (fun id -> require (Set.contains id objects) $"Unknown subject {id}.")
        for light in scene.Lights do
            require (not (obj.ReferenceEquals(light, null))) "A light must not be null."
            require (Enum.IsDefined(typeof<LightKind>, light.Kind)) $"Unknown light kind for {light.Id}."
            vector 3 $"light {light.Id} position" light.Position
            vector 3 $"light {light.Id} direction" light.Direction
            vector 2 $"light {light.Id} size" light.Size
            colour $"light {light.Id} colour" light.Colour
            nonnegative $"light {light.Id} intensity" light.Intensity
            if light.Kind = LightKind.Directional || light.Kind = LightKind.Rectangle then
                require (Array.exists ((<>) 0.) light.Direction) $"Light {light.Id} direction must be nonzero."
            if light.Kind = LightKind.Rectangle then
                require (Array.forall (fun value -> value > 0.) light.Size) $"Light {light.Id} size must be positive."
            if light.Kind = LightKind.Environment then
                positive $"environment {light.Id} legacy sphere radius (size[0])" light.Size.[0]
        scene.Lights |> Array.map (fun light -> light.Id) |> identifiers "lights" |> ignore
        scene

    let validateSettings (settings: RenderSettings) =
        require (not (obj.ReferenceEquals(settings, null))) "Settings must be an object."
        require (settings.Width > 0 && settings.Height > 0) "Image dimensions must be positive."
        require (int64 settings.Width * int64 settings.Height <= int64 Int32.MaxValue / 4L) "Image dimensions exceed addressable storage."
        require (settings.CameraSamples > 0 && settings.LightSamples > 0 && settings.GlossySamples > 0) "Sample counts must be positive."
        require (settings.MaxBounces >= 0 && settings.MaxBounces <= 32) "Bounce depth must be between 0 and 32."
        require (settings.Threads > 0 && settings.TileSize > 0) "Thread and tile counts must be positive."
        require (settings.Precision = "float64" || settings.Precision = "float32") "precision must be float64 or float32."
        require (settings.Sampler = "regular" || settings.Sampler = "multi-jittered") "sampler must be regular or multi-jittered."
        require (settings.Transfer = "gamma2" || settings.Transfer = "srgb" || settings.Transfer = "linear" || settings.Transfer = "aces")
            "transfer must be gamma2, srgb, linear, or aces."
        settings

    let load (path: string) =
        use stream = File.OpenRead path
        JsonSerializer.Deserialize<SceneSpec>(stream, jsonOptions) |> validate

    let save (path: string) scene =
        validate scene |> ignore
        File.WriteAllText(path, JsonSerializer.Serialize(scene, jsonOptions))

    let resolveAsset (scenePath: string) (relativePath: string) =
        if Path.IsPathRooted relativePath then Path.GetFullPath relativePath
        else Path.GetFullPath(Path.Combine(Path.GetDirectoryName(Path.GetFullPath scenePath), relativePath))

    let hashFile (path: string) =
        use stream = File.OpenRead path
        Convert.ToHexString(SHA256.HashData stream).ToLowerInvariant()

    let identity =
        [| 1.; 0.; 0.; 0.
           0.; 1.; 0.; 0.
           0.; 0.; 1.; 0.
           0.; 0.; 0.; 1. |]

    let materialVariants = [| "matte"; "phong"; "mirror"; "glossy"; "glass"; "authored" |]

    let ambientColour (material: MaterialSpec) =
        if isNull material.AmbientColour then material.Colour else material.AmbientColour

    let specularColour (material: MaterialSpec) =
        if isNull material.SpecularColour then [| 1.; 1.; 1. |] else material.SpecularColour

    let reflectionColour (material: MaterialSpec) =
        if isNull material.ReflectionColour then [| 1.; 1.; 1. |] else material.ReflectionColour

    let applyMaterialVariant variant (scene: SceneSpec) =
        require (Array.contains variant materialVariants) $"Unknown material variant {variant}."
        if variant = "authored" then scene
        else
            let kind =
                match variant with
                | "matte" -> MaterialKind.Matte
                | "phong" -> MaterialKind.Phong
                | "mirror" -> MaterialKind.Mirror
                | "glossy" -> MaterialKind.Glossy
                | "glass" -> MaterialKind.Glass
                | _ -> invalidArg (nameof variant) "Unknown material variant."
            let id = "__benchmark_" + variant
            require (scene.Materials |> Array.forall (fun material -> material.Id <> id)) $"Reserved material id {id} is already present."
            let material =
                { Id = id; Kind = kind; Colour = [| 0.65; 0.5; 0.3 |]
                  AmbientColour = null; SpecularColour = null; ReflectionColour = null
                  Ambient = 0.1; Diffuse = 0.65; Specular = 0.25; Exponent = 48
                  Reflectivity = (if kind = MaterialKind.Mirror then 0.85 elif kind = MaterialKind.Glossy then 0.65 else 0.)
                  GlossExponent = 32; Ior = 1.5; Filter = [| 0.88; 0.97; 0.94 |]
                  Emission = 0.; Texture = "" }
            let subjects = Set.ofArray scene.Subjects
            let meshes = scene.Meshes |> Array.map (fun mesh -> mesh.Id, mesh) |> Map.ofArray
            let objects =
                scene.Objects
                |> Array.map (fun instance ->
                    if Set.contains instance.Id subjects then
                        if kind = MaterialKind.Glass then
                            require meshes.[instance.Mesh].Closed $"Glass subject {instance.Id} must reference a closed mesh."
                        { instance with Material = id }
                    else instance)
            { scene with Materials = Array.append scene.Materials [| material |]; Objects = objects }

    let preset name =
        let width, samples, depth, lightSamples =
            match name with
            | "quick" -> 256, 4, 2, 1
            | "standard" -> 512, 16, 3, 4
            | "high" -> 1024, 64, 4, 4
            | _ -> invalidArg (nameof name) "Unknown preset; use quick, standard, or high."
        { Width = width; Height = width; CameraSamples = samples; LightSamples = lightSamples
          GlossySamples = 1; MaxBounces = depth; Threads = Environment.ProcessorCount
          TileSize = 16; Seed = 2026; Precision = "float64"; Sampler = "regular"; Transfer = "gamma2" }
