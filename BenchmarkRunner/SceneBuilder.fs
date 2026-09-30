namespace Tracer.Benchmarks

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Reflection
open Microsoft.FSharp.Reflection
open Tracer
open Tracer.Basics
open Tracer.Basics.Transformation
open Tracer.SceneFormat

[<CLIMutable>]
type RgbTexture =
    { Width: int
      Height: int
      Pixels: byte array }

type BuiltScene =
    { Specification: SceneSpec
      Scene: API.scene
      Camera: API.camera
      LoadMs: float
      BuildMs: float }

/// This file is linked, not copied, into both process-isolated workers.
module SceneBuilder =
    let private colour (values: float array) = API.mkColour values.[0] values.[1] values.[2]
    let private point (values: float array) = API.mkPoint values.[0] values.[1] values.[2]
    let private vector (values: float array) = API.mkVector values.[0] values.[1] values.[2]

    // The frozen .fsi hides these constructors. Use the exact record/T representations,
    // equivalent to mkTransformation(forward, inverse), without changing its source or math.
    let private representationAccess = BindingFlags.Public ||| BindingFlags.NonPublic
    let private matrix =
        let fields = FSharpType.GetRecordFields(typeof<QuickMatrix>, representationAccess)
        let expected = [| for row in 1 .. 4 do for column in 1 .. 4 do yield $"Pos{row}x{column}" |]
        if fields |> Array.map (fun field -> field.Name) <> expected then
            invalidOp "Unsupported QuickMatrix representation; expected the original row-major fields."
        let create = FSharpValue.PreComputeRecordConstructor(typeof<QuickMatrix>, representationAccess)
        fun (values: float array) -> create (Array.map box values) :?> QuickMatrix

    let private transformation =
        let cases = FSharpType.GetUnionCases(typeof<Transformation>, representationAccess)
        let case =
            cases |> Array.tryFind (fun case ->
                case.Name = "T" && (case.GetFields() |> Array.map (fun field -> field.PropertyType)) = [| typeof<QuickMatrix>; typeof<QuickMatrix> |])
            |> Option.defaultWith (fun () -> invalidOp "Unsupported transformation representation; expected T(forward, inverse).")
        let create = FSharpValue.PreComputeUnionConstructor(case, representationAccess)
        fun forward inverse -> create [| box forward; box inverse |] :?> Transformation

    let affineTransform (m: float array) =
        if isNull m || m.Length <> 16 || not (Array.forall Double.IsFinite m)
           || m.[12..15] <> [| 0.; 0.; 0.; 1. |] then
            invalidArg (nameof m) "Expected a finite, affine row-major 4x4 matrix."
        let det =
            m.[0] * (m.[5] * m.[10] - m.[6] * m.[9])
            - m.[1] * (m.[4] * m.[10] - m.[6] * m.[8])
            + m.[2] * (m.[4] * m.[9] - m.[5] * m.[8])
        if not (Double.IsFinite det) || det = 0. then
            invalidArg (nameof m) "The object transform must be invertible."
        let inverse = Array.copy SceneFiles.identity
        inverse.[0] <- (m.[5] * m.[10] - m.[6] * m.[9]) / det
        inverse.[1] <- (m.[2] * m.[9] - m.[1] * m.[10]) / det
        inverse.[2] <- (m.[1] * m.[6] - m.[2] * m.[5]) / det
        inverse.[4] <- (m.[6] * m.[8] - m.[4] * m.[10]) / det
        inverse.[5] <- (m.[0] * m.[10] - m.[2] * m.[8]) / det
        inverse.[6] <- (m.[2] * m.[4] - m.[0] * m.[6]) / det
        inverse.[8] <- (m.[4] * m.[9] - m.[5] * m.[8]) / det
        inverse.[9] <- (m.[1] * m.[8] - m.[0] * m.[9]) / det
        inverse.[10] <- (m.[0] * m.[5] - m.[1] * m.[4]) / det
        for row in [| 0; 4; 8 |] do
            inverse.[row + 3] <-
                -(inverse.[row] * m.[3] + inverse.[row + 1] * m.[7] + inverse.[row + 2] * m.[11])
        if not (Array.forall Double.IsFinite inverse) then
            invalidArg (nameof m) "The object transform has a non-finite inverse."
        transformation (matrix m) (matrix inverse)

    let sampler (settings: RenderSettings) count =
        let side = int (sqrt (float count))
        if side <= 0 || int64 side * int64 side <> int64 count then
            invalidArg (nameof count) $"Sampler {settings.Sampler} requires an actual square sample count; received {count}."
        match settings.Sampler with
        | "regular" -> API.mkRegularSampler side
        | "multi-jittered" -> API.mkMultiJitteredSampler side 83
        | name -> invalidArg (nameof settings) $"Unsupported sampler {name}."

    let private material settings (spec: MaterialSpec) (texel: Colour) =
        let ca = colour (SceneFiles.ambientColour spec) * texel
        let cd = colour spec.Colour * texel
        let cs = colour (SceneFiles.specularColour spec)
        let cr = colour (SceneFiles.reflectionColour spec)
        match spec.Kind with
        | MaterialKind.Matte ->
            API.mkMatteMaterial ca spec.Ambient cd spec.Diffuse
        | MaterialKind.Phong ->
            API.mkPhongMaterial ca spec.Ambient cd spec.Diffuse cs spec.Specular spec.Exponent
        | MaterialKind.Mirror ->
            API.mkPhongReflectiveMaterial ca spec.Ambient cd spec.Diffuse cs spec.Specular cr spec.Reflectivity spec.Exponent
        | MaterialKind.Glossy ->
            API.mkPhongGlossyReflectiveMaterial ca spec.Ambient cd spec.Diffuse cs spec.Specular cr spec.Reflectivity
                spec.Exponent spec.GlossExponent (sampler settings settings.GlossySamples)
        | MaterialKind.Glass ->
            API.mkTransparent (colour spec.Filter * texel) (API.mkColour 1. 1. 1.) spec.Ior 1.
        | MaterialKind.Emissive -> API.mkEmissive cd spec.Emission
        | kind -> invalidArg (nameof spec) $"Unsupported material kind {kind}."

    let private texture settings (spec: MaterialSpec) image =
        match image with
        | None -> material settings spec (API.mkColour 1. 1. 1.) |> API.mkMatTexture
        | Some (image: RgbTexture) ->
            if image.Width <= 0 || image.Height <= 0 || isNull image.Pixels
               || int64 image.Pixels.Length <> int64 image.Width * int64 image.Height * 3L then
                raise (InvalidDataException $"Texture for {spec.Id} must contain packed top-down RGB8 pixels.")
            let palette = Dictionary<int, Material>()
            let pixels =
                Array.init (image.Width * image.Height) (fun index ->
                    let offset = index * 3
                    let r, g, b = int image.Pixels.[offset], int image.Pixels.[offset + 1], int image.Pixels.[offset + 2]
                    let key = (r <<< 16) ||| (g <<< 8) ||| b
                    match palette.TryGetValue key with
                    | true, value -> value
                    | _ ->
                        // fromColor deliberately decodes gamma 2, including for an sRGB output experiment.
                        let texel = API.fromColor (System.Drawing.Color.FromArgb(r, g, b))
                        let value = material settings spec texel
                        palette.Add(key, value)
                        value)
            let coordinate value =
                if not (Double.IsFinite value) then
                    raise (InvalidDataException "Non-finite texture coordinate.")
                max 0. (min 1. value)
            let result =
                API.mkTexture (fun u v ->
                    let x = min (image.Width - 1) (int (coordinate u * float image.Width))
                    let y = min (image.Height - 1) (int ((1. - coordinate v) * float image.Height))
                    pixels.[y * image.Width + x])
#if LEGACY
            result
#else
            if spec.Kind = MaterialKind.Glass then result else Textures.markOpaque result
#endif

    let rectangleTransform (light: LightSpec) =
        let n = (vector light.Direction).Normalise
        let helper = if abs n.Y < 0.99 then API.mkVector 0. 1. 0. else API.mkVector 1. 0. 0.
        let u = (helper.CrossProduct n).Normalise
        let v = n.CrossProduct u
        let origin = point light.Position - u * (light.Size.[0] * 0.5) - v * (light.Size.[1] * 0.5)
        affineTransform
            [| u.X; v.X; n.X; origin.X
               u.Y; v.Y; n.Y; origin.Y
               u.Z; v.Z; n.Z; origin.Z
               0.; 0.; 0.; 1. |]

    let private buildLight settings (spec: LightSpec) =
        match spec.Kind with
        | LightKind.Point -> API.mkLight (point spec.Position) (colour spec.Colour) spec.Intensity
        | LightKind.Directional -> API.mkDirectionalLight (vector spec.Direction) (colour spec.Colour) spec.Intensity
        | LightKind.Rectangle ->
            let rectangle =
                API.mkBaseRectangle (API.mkPoint 0. 0. 0.)
                    (API.mkPoint 0. spec.Size.[1] 0.) (API.mkPoint spec.Size.[0] 0. 0.)
            let emission = API.mkEmissive (colour spec.Colour) spec.Intensity
            API.transformLight (API.mkAreaLight rectangle emission (sampler settings settings.LightSamples)) (rectangleTransform spec)
        | LightKind.Environment ->
            let emission = API.mkEmissive (colour spec.Colour) spec.Intensity |> API.mkMatTexture
            API.mkEnvironmentLight spec.Size.[0] emission (sampler settings settings.LightSamples)
        | kind -> invalidArg (nameof spec) $"Unsupported light kind {kind}."

    let private buildUsing (scenePath: string) variant (settings: RenderSettings) (decodeTexture: string -> RgbTexture) (loadSpecification: unit -> SceneSpec) =
        SceneFiles.validateSettings settings |> ignore
        Sampling.setRandomSeed settings.Seed
        for count in [ settings.CameraSamples; settings.LightSamples; settings.GlossySamples ] do
            sampler settings count |> ignore
        let total = Stopwatch.StartNew()
        let mutable loadMs = 0.
        let load operation =
            let watch = Stopwatch.StartNew()
            let result = operation ()
            loadMs <- loadMs + watch.Elapsed.TotalMilliseconds
            result
        let spec =
            load (fun () ->
                let scene = loadSpecification ()
                ScenePolicy.validateVariants scene.Id [| variant |]
                SceneFiles.applyMaterialVariant variant scene)
        let meshSpecs = spec.Meshes |> Array.map (fun mesh -> mesh.Id, mesh) |> Map.ofArray
        let materialSpecs = spec.Materials |> Array.map (fun material -> material.Id, material) |> Map.ofArray
        let textures = Dictionary<string, API.texture>(StringComparer.Ordinal)
        let images = Dictionary<string, RgbTexture>(StringComparer.Ordinal)
        let shapes = Dictionary<string * string, Shape>()
        let getTexture id =
            match textures.TryGetValue id with
            | true, value -> value
            | _ ->
                let materialSpec = materialSpecs.[id]
                let image =
                    if String.IsNullOrWhiteSpace materialSpec.Texture then None
                    else
                        let path = SceneFiles.resolveAsset scenePath materialSpec.Texture
                        match images.TryGetValue path with
                        | true, value -> Some value
                        | _ ->
                            let value = load (fun () -> decodeTexture path)
                            images.Add(path, value)
                            Some value
                let value = texture settings materialSpec image
                textures.Add(id, value)
                value
        let objects =
            spec.Objects
            |> Array.map (fun instance ->
                let mesh = meshSpecs.[instance.Mesh]
                if materialSpecs.[instance.Material].Kind = MaterialKind.Glass && not mesh.Closed then
                    raise (InvalidDataException $"Glass object {instance.Id} requires a closed mesh.")
                let key = instance.Mesh, instance.Material
                let shape =
                    match shapes.TryGetValue key with
                    | true, value -> value
                    | _ ->
                        let tex = getTexture instance.Material
                        // Complete one mkPLY/mkShape pair before loading another mesh: the frozen API assigns IDs at load time.
                        let basis = load (fun () -> API.mkPLY (SceneFiles.resolveAsset scenePath mesh.Path) mesh.Smooth)
                        let value = API.mkShape basis tex
                        shapes.Add(key, value)
                        value
                API.transform shape (affineTransform instance.Transform))
            |> Array.toList
        let lights = spec.Lights |> Array.map (buildLight settings) |> Array.toList
        let cameraSpec = spec.Camera
        let camera =
            if cameraSpec.LensRadius = 0. then
                API.mkPinholeCamera (point cameraSpec.Position) (point cameraSpec.Target) (vector cameraSpec.Up)
                    cameraSpec.ViewDistance cameraSpec.ViewWidth cameraSpec.ViewHeight settings.Width settings.Height
                    (sampler settings settings.CameraSamples)
            else
                API.mkThinLensCamera (point cameraSpec.Position) (point cameraSpec.Target) (vector cameraSpec.Up)
                    cameraSpec.ViewDistance cameraSpec.ViewWidth cameraSpec.ViewHeight settings.Width settings.Height
                    cameraSpec.LensRadius cameraSpec.FocusDistance (sampler settings settings.CameraSamples)
                    (sampler settings settings.CameraSamples)
        let scene = API.mkScene objects lights (API.mkAmbientLight (colour spec.AmbientColour) spec.AmbientIntensity) settings.MaxBounces
        { Specification = spec; Scene = scene; Camera = camera; LoadMs = loadMs
          BuildMs = max 0. (total.Elapsed.TotalMilliseconds - loadMs) }

    let build scenePath variant settings decodeTexture =
        buildUsing scenePath variant settings decodeTexture (fun () -> SceneFiles.load scenePath)

    let buildLoaded scenePath variant settings decodeTexture specification =
        buildUsing scenePath variant settings decodeTexture (fun () -> SceneFiles.validate specification)
