namespace Tracer.Basics
open System

module Textures = 

    type Texture = | Texture of (float -> float -> Material)

    let mkTexture func =
        Texture func

    let mkMatTexture mat =
        let func _ _ = mat
        Texture func

    let getFunc (Texture func) = func
    
        