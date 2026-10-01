namespace Tracer.Basics

open System
open Tracer.Basics.PathTracing

/// Metallic-roughness parameters resolved at one surface point (the glTF 2.0 model plus thin-surface
/// diffuse transmission and sheen).
[<NoEquality; NoComparison>]
type PbrSample =
    { BaseColour: Colour
      Metallic: float
      Roughness: float
      /// Tangent-space shading normal; +Z is the unperturbed surface normal.
      Normal: Vector
      /// Ambient occlusion in [0,1], 1 = unoccluded.
      Occlusion: float
      Emissive: Colour
      /// Specular transmission (smooth dielectric), in [0,1].
      Transmission: float
      Ior: float
      /// Thin-surface diffuse transmission, in [0,1]: back light shows through.
      DiffuseTransmission: float
      DiffuseTransmissionColour: Colour
      /// Grazing tint of the diffuse lobe, in [0,1].
      Sheen: float
      SheenColour: Colour }

module PbrSample =
    let defaults =
        { BaseColour = Colour(0.8, 0.8, 0.8); Metallic = 0.; Roughness = 0.5; Normal = Vector(0., 0., 1.)
          Occlusion = 1.; Emissive = Colour.Black; Transmission = 0.; Ior = 1.5
          DiffuseTransmission = 0.; DiffuseTransmissionColour = Colour.White; Sheen = 0.; SheenColour = Colour.White }

/// How a PBR sample turns into BSDF parameters at a hit.
module PbrShading =
    let private clamp01 v = if Double.IsFinite v then max 0. (min 1. v) else 0.
    let private clampColour (c: Colour) = Colour(clamp01 c.R, clamp01 c.G, clamp01 c.B)

    /// Fixed world axis the tangent is projected from. Meshes do not carry tangents to the hit point, so
    /// tangent-space normals are applied in this frame: continuous everywhere except at the two points
    /// whose normal is parallel to the axis. Its direction is deliberately oblique, so those points do not
    /// fall on the up- or side-facing surfaces scenes show most.
    let private referenceAxis = Vector(0.48, 0.21, 0.85).Normalise
    let private fallbackAxis = Vector(-0.62, 0.78, 0.05).Normalise

    /// World-space tangent and bitangent for a unit normal.
    let tangentFrame (normal: Vector) =
        let n = normal.Normalise
        let project (axis: Vector) = axis - (axis * n) * n
        let t =
            let candidate = project referenceAxis
            if candidate.Magnitude > 1e-4 then candidate.Normalise else (project fallbackAxis).Normalise
        struct (t, n % t)

    /// World-space tangent and bitangent at a hit, for normal maps: the mesh's own tangent frame when the
    /// hit carries one (Gram-Schmidt orthonormalised against the shading normal, keeping the bitangent's
    /// handedness), otherwise `tangentFrame`. On the back of a double-sided surface, where `hit.Normal` is
    /// the flipped shading normal, the whole frame flips with it, so the perturbed normal mirrors the
    /// front side's.
    let hitTangentFrame (hit: HitPoint) =
        let n = hit.Normal.Normalise
        if not hit.HasTangent then tangentFrame n
        else
            let front = hit.ShadingNormal
            let flip = if n * front < 0. then -1. else 1.
            let t = hit.Tangent - (hit.Tangent * front) * front
            if not t.IsFinite || t.Magnitude < 1e-12 * (1. + hit.Tangent.Magnitude) then tangentFrame n
            else
                let t = t.Normalise
                let b = front % t
                let b = if b * hit.Bitangent < 0. then -b else b
                struct (flip * t, flip * b)

    /// Shading normal of `sample` at `hit`, expressed in the local frame the integrators build from
    /// `hit.Normal` (`ShadingFrame.ofNormal`).
    let localNormal (sample: PbrSample) (hit: HitPoint) (frame: ShadingFrame) =
        let n = sample.Normal
        if n.X = 0. && n.Y = 0. && n.Z = 1. then Vector(0., 0., 1.)
        elif not n.IsFinite || n.IsZero || n.Z <= 0. then Vector(0., 0., 1.)
        else
            let struct (t, b) = hitTangentFrame hit
            let world = n.X * t + n.Y * b + n.Z * hit.Normal.Normalise
            let local = (frame.ToLocal world).Normalise
            if local.Z > 1e-3 then local else Vector(0., 0., 1.)

    /// BSDF parameters for the path tracer. `exteriorIor` is the index of the medium the ray arrives
    /// through.
    let surfaceAt (sample: PbrSample) (hit: HitPoint) (exteriorIor: float) =
        let frame = ShadingFrame.ofNormal hit.Normal
        let ior = if Double.IsFinite sample.Ior && sample.Ior > 0. then sample.Ior else 1.5
        let exterior = if Double.IsFinite exteriorIor && exteriorIor > 0. then exteriorIor else 1.
        let relative = if hit.FrontFace then ior / exterior else 1. / ior
        // Baked occlusion darkens the albedo: it carries detail the geometry lacks, while the large-scale
        // occlusion it also contains is mostly what the path tracer computes anyway.
        let occlusion = clamp01 sample.Occlusion
        { SurfaceParams.defaults with
            BaseColour = clampColour (sample.BaseColour.Scale occlusion)
            Roughness = clamp01 sample.Roughness
            Metallic = clamp01 sample.Metallic
            Transmission = clamp01 sample.Transmission
            Ior = max 1e-3 relative
            SpecularF0 = SurfaceParams.f0OfIor ior
            Normal = localNormal sample hit frame
            DiffuseTransmission = clamp01 sample.DiffuseTransmission
            DiffuseTransmissionColour = clampColour (sample.DiffuseTransmissionColour.Scale occlusion)
            Sheen = clamp01 sample.Sheen
            SheenColour = clampColour sample.SheenColour }

/// A physically based (metallic-roughness) material. The path tracer reads its parameters directly
/// (see `MaterialAdapter`); the classic integrator evaluates the same BSDF for direct light and adds an
/// occlusion-weighted ambient term, without glossy or transmitted bounces.
type PbrMaterial(sample: PbrSample) =
    inherit Material()

    let ambientAlbedo =
        let k = (1. - max 0. (min 1. sample.Metallic)) * max 0. (min 1. sample.Occlusion)
        Colour(max 0. (min 1. sample.BaseColour.R), max 0. (min 1. sample.BaseColour.G), max 0. (min 1. sample.BaseColour.B)) * k

    member _.Sample = sample
    member _.Emission = sample.Emissive

    default _.AmbientColour(hit, light) = ambientAlbedo * light.GetColour hit
    default _.ReflectionFactor(_, _) = Colour.White
    default _.BounceMethod _ = [||]
    default _.IsRecursive = false
    default _.Bounce(_, hit, light) =
        let direction = light.GetDirectionFromPoint hit
        let frame = ShadingFrame.ofNormal hit.Normal
        let wi = frame.ToLocal direction
        let wo = frame.ToLocal(-hit.Ray.GetDirection.Normalise)
        if wi.Z <= 0. || wo.Z <= 0. then Colour.Black
        else
            let f = Bsdf.evalLocal (PbrShading.surfaceAt sample hit 1.) wo wi
            if f.IsBlack then Colour.Black
            else
                let pdf = light.GetProbabilityDensity hit
                if not (Double.IsFinite pdf) || pdf <= 0. then
                    invalidOp "A contributing light sample must have a positive finite PDF."
                f * light.GetColour hit * (light.GetGeometricFactor hit / pdf)

/// A PBR material description: constant factors, each optionally modulated by a map over the mesh's
/// texture coordinates (u, v), as stored on the mesh (v up). Maps return linear values. This is what the
/// glTF importer produces, and what material overrides edit.
[<NoEquality; NoComparison>]
type PbrParams =
    { BaseColour: Colour
      /// Multiplies `BaseColour`.
      BaseColourMap: (float -> float -> Colour) option
      Metallic: float
      Roughness: float
      /// (metallic, roughness) multipliers, the glTF B and G channels.
      MetallicRoughnessMap: (float -> float -> struct (float * float)) option
      /// Tangent-space unit normal (+Z = unperturbed), before `NormalScale`.
      NormalMap: (float -> float -> Vector) option
      NormalScale: float
      /// Occlusion in [0,1], blended in by `OcclusionStrength`.
      OcclusionMap: (float -> float -> float) option
      OcclusionStrength: float
      Emissive: Colour
      /// Multiplies `Emissive`.
      EmissiveMap: (float -> float -> Colour) option
      Transmission: float
      Ior: float
      DiffuseTransmission: float
      /// Multiplies `DiffuseTransmission`.
      DiffuseTransmissionMap: (float -> float -> float) option
      DiffuseTransmissionColour: Colour
      Sheen: float
      SheenColour: Colour }

module PbrParams =
    let defaults =
        { BaseColour = Colour(0.8, 0.8, 0.8); BaseColourMap = None; Metallic = 0.; Roughness = 0.5
          MetallicRoughnessMap = None; NormalMap = None; NormalScale = 1.; OcclusionMap = None; OcclusionStrength = 1.
          Emissive = Colour.Black; EmissiveMap = None; Transmission = 0.; Ior = 1.5
          DiffuseTransmission = 0.; DiffuseTransmissionMap = None; DiffuseTransmissionColour = Colour.White
          Sheen = 0.; SheenColour = Colour.White }

    /// True when no parameter varies over the surface.
    let isUniform (p: PbrParams) =
        p.BaseColourMap.IsNone && p.MetallicRoughnessMap.IsNone && p.NormalMap.IsNone
        && p.OcclusionMap.IsNone && p.EmissiveMap.IsNone && p.DiffuseTransmissionMap.IsNone

    let private clamp01 v = if Double.IsFinite v then max 0. (min 1. v) else 0.
    let private clampColour (c: Colour) = Colour(clamp01 c.R, clamp01 c.G, clamp01 c.B)
    let private nonNegative (c: Colour) =
        let fix v = if Double.IsFinite v && v > 0. then v else 0.
        Colour(fix c.R, fix c.G, fix c.B)

    /// glTF normal scaling: the tangent-plane components scale, then the normal is renormalized.
    let scaleNormal (scale: float) (n: Vector) =
        let scaled = Vector(n.X * scale, n.Y * scale, n.Z)
        if not scaled.IsFinite || scaled.IsZero || scaled.Z <= 0. then Vector(0., 0., 1.) else scaled.Normalise

    /// Resolve every map at (u, v).
    let sampleAt (p: PbrParams) (u: float) (v: float) : PbrSample =
        let baseColour =
            match p.BaseColourMap with
            | Some map -> p.BaseColour * map u v
            | None -> p.BaseColour
        let metallic, roughness =
            match p.MetallicRoughnessMap with
            | Some map -> let struct (m, r) = map u v in p.Metallic * m, p.Roughness * r
            | None -> p.Metallic, p.Roughness
        let normal =
            match p.NormalMap with
            | Some map -> scaleNormal p.NormalScale (map u v)
            | None -> Vector(0., 0., 1.)
        let occlusion =
            match p.OcclusionMap with
            | Some map -> 1. + clamp01 p.OcclusionStrength * (clamp01 (map u v) - 1.)
            | None -> 1.
        let emissive =
            match p.EmissiveMap with
            | Some map -> p.Emissive * map u v
            | None -> p.Emissive
        { BaseColour = clampColour baseColour
          Metallic = clamp01 metallic
          Roughness = clamp01 roughness
          Normal = normal
          Occlusion = occlusion
          Emissive = nonNegative emissive
          Transmission = clamp01 p.Transmission
          Ior = (if Double.IsFinite p.Ior && p.Ior > 0. then p.Ior else 1.5)
          DiffuseTransmission =
            match p.DiffuseTransmissionMap with
            | Some map -> clamp01 (p.DiffuseTransmission * map u v)
            | None -> clamp01 p.DiffuseTransmission
          DiffuseTransmissionColour = clampColour p.DiffuseTransmissionColour
          Sheen = clamp01 p.Sheen
          SheenColour = clampColour p.SheenColour }
