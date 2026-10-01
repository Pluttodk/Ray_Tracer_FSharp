namespace Tracer.Basics.PathTracing

open System
open Tracer.Basics

/// Projects the historical material hierarchy onto the parametric BSDF, so the
/// path tracer can render every existing scene without those scenes being
/// re-authored.
///
/// This is a translation, not an equivalence. The old materials are a Whitted
/// model: "reflectivity" is an arbitrary multiplier on a mirror ray rather than
/// a Fresnel term, and diffuse and mirror lobes were simply added with no
/// shared energy budget. Mapping them onto an energy-conserving BSDF therefore
/// changes the image by construction - which is the point, but it does mean
/// these renders are not comparable pixel-for-pixel with the classic ones.
module MaterialAdapter =

    let private clamp01 v = if Double.IsFinite v then max 0. (min 1. v) else 0.

    let private clampColour (c: Colour) =
        let fix v = if Double.IsFinite v then max 0. (min 1. v) else 0.
        Colour(fix c.R, fix c.G, fix c.B)

    /// Phong/Blinn cosine exponent to GGX roughness.
    ///
    /// The standard correspondence is alpha = sqrt(2/(n+2)), and this BSDF
    /// parameterizes alpha = roughness^2, so roughness = (2/(n+2))^(1/4).
    let roughnessOfExponent (exponent: int) =
        if exponent <= 0 then 1.
        else
            let alpha = sqrt (2. / (float exponent + 2.))
            clamp01 (sqrt alpha)

    /// Surface parameters for a hit, in the medium the ray is currently in.
    /// `exteriorIor` is the index of the medium the ray is travelling through,
    /// which the integrator tracks across dielectric boundaries.
    let surfaceAt (hit: HitPoint) (exteriorIor: float) =
        let material = hit.Material
        match material with
        | :? PbrMaterial as pbr ->
            // Physically based already: no translation, just resolve the
            // shading normal and the IOR for the side being hit.
            PbrShading.surfaceAt pbr.Sample hit exteriorIor

        | :? TransparentMaterial as glass ->
            // Indices() already orders these by which face was hit, so the
            // integrator does not need to flip anything here.
            let etaT, _ = glass.Indices hit
            let relative = if etaT > 0. && Double.IsFinite etaT then etaT / exteriorIor else 1.5
            { SurfaceParams.defaults with
                BaseColour = Colour.White
                Roughness = 0.
                Metallic = 0.
                Transmission = 1.
                Ior = max 1e-3 relative
                SpecularF0 = SurfaceParams.f0OfIor (max 1e-3 relative)
                TransmissionFilter = clampColour glass.InnerFilterColour }

        | :? PhongGlossyReflectiveMaterial as m ->
            { SurfaceParams.defaults with
                BaseColour = clampColour m.MatteColour |> fun c -> c.Scale(clamp01 m.MatteCoefficient)
                Roughness = roughnessOfExponent m.GlossyExponent
                SpecularF0 = clampColour (m.ReflectionColour.Scale(clamp01 m.ReflectionCoefficient))
                Ior = 1.5 }

        | :? MatteGlossyReflectiveMaterial as m ->
            { SurfaceParams.defaults with
                BaseColour = clampColour m.MatteColour |> fun c -> c.Scale(clamp01 m.MatteCoefficient)
                Roughness = roughnessOfExponent m.GlossyExponent
                SpecularF0 = clampColour (m.ReflectionColour.Scale(clamp01 m.ReflectionCoefficient))
                Ior = 1.5 }

        | :? PhongReflectiveMaterial as m ->
            // A legacy "mirror": a delta lobe whose strength is authored
            // directly, so it maps onto F0 rather than onto an IOR.
            { SurfaceParams.defaults with
                BaseColour = clampColour m.MatteColour |> fun c -> c.Scale(clamp01 m.MatteCoefficient)
                Roughness = 0.
                SpecularF0 = clampColour (m.ReflectionColour.Scale(clamp01 m.ReflectionCoefficient))
                Ior = 1.5 }

        | :? MatteReflectiveMaterial as m ->
            { SurfaceParams.defaults with
                BaseColour = clampColour m.MatteColour |> fun c -> c.Scale(clamp01 m.MatteCoefficient)
                Roughness = 0.
                SpecularF0 = clampColour (m.ReflectionColour.Scale(clamp01 m.ReflectionCoefficient))
                Ior = 1.5 }

        | :? PhongMaterial as m ->
            // The Phong specular highlight becomes a rough dielectric lobe of
            // matching width, with its coefficient carried into F0.
            let f0 = clamp01 m.SpecularCoefficient
            { SurfaceParams.defaults with
                BaseColour = clampColour m.MatteColour |> fun c -> c.Scale(clamp01 m.MatteCoefficient)
                Roughness = roughnessOfExponent m.SpecularExponent
                SpecularF0 = clampColour (m.SpecularColour.Scale f0)
                Ior = 1.5 }

        | :? MatteMaterial as m ->
            { SurfaceParams.defaults with
                BaseColour = clampColour m.MatteColour |> fun c -> c.Scale(clamp01 m.MatteCoefficient)
                Roughness = 1.
                SpecularF0 = Colour.Black }

        | _ ->
            // Emissive and any unrecognized material: no scattering. Emission is
            // added by the integrator, not by the BSDF.
            { SurfaceParams.defaults with
                BaseColour = Colour.Black
                Roughness = 1.
                SpecularF0 = Colour.Black }

    /// Radiance emitted from the front face, if this material emits.
    let emissionAt (hit: HitPoint) =
        match hit.Material with
        | :? EmissiveMaterial as m when hit.FrontFace -> m.EmisiveRadience
        | :? PbrMaterial as m when hit.FrontFace -> m.Emission
        | _ -> Colour.Black

    /// True when the surface scatters nothing and only emits.
    let isPurelyEmissive (material: Material) =
        match material with
        | :? EmissiveMaterial -> true
        | _ -> false
