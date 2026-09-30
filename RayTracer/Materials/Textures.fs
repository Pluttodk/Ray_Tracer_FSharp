namespace Tracer.Basics
open System
open System.Runtime.CompilerServices

module Textures = 

    type Texture = | Texture of (float -> float -> Material)

    let private constantMaterials = ConditionalWeakTable<Texture, Material>()
    let private opaqueTextures = ConditionalWeakTable<Texture, obj>()
    let private opaqueMarker = obj()

    let mkTexture func =
        Texture func

    let mkMatTexture mat =
        let func _ _ = mat
        let texture = Texture func
        constantMaterials.Add(texture, mat)
        texture

    let getFunc (Texture func) = func

    let tryGetMaterial texture =
        match constantMaterials.TryGetValue texture with
        | true, material -> Some material
        | false, _ -> None

    let markOpaque texture =
        opaqueTextures.GetValue(texture, fun _ -> opaqueMarker) |> ignore
        texture

    let isOpaque texture =
        match tryGetMaterial texture with
        | Some (:? TransparentMaterial) -> false
        | Some _ -> true
        | None -> fst (opaqueTextures.TryGetValue texture)
    
        