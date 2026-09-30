namespace Tracer.Basics

module Textures =

    type Texture = | Texture of (float -> float -> Material)
    val mkTexture : (float -> float -> Material) -> Texture
    val mkMatTexture : Material -> Texture
    val getFunc : Texture -> (float -> float -> Material)
    val tryGetMaterial : Texture -> Material option
    /// The caller must establish that every texel is opaque before applying this metadata.
    val markOpaque : Texture -> Texture
    val isOpaque : Texture -> bool