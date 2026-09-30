namespace Tracer.Basics

module Textures =

    type Texture = | Texture of (float -> float -> Material)
    val mkTexture : (float -> float -> Material) -> Texture
    val mkMatTexture : Material -> Texture
    val getFunc : Texture -> (float -> float -> Material)