namespace Tracer.Basics.PathTracing

open System
open Tracer.Basics

/// A single parametric surface BSDF, in the lineage of the Disney "principled"
/// BRDF and OpenPBR Surface. One parameter set covers diffuse, plastic, metal
/// and glass, so the integrator never needs to know which kind of material it
/// stands on - only eval / sample / pdf.
[<Struct; NoEquality; NoComparison>]
type SurfaceParams =
    { /// Diffuse albedo when dielectric; normal-incidence reflectance when metallic.
      BaseColour: Colour
      /// Perceptual roughness in [0,1]; alpha = roughness^2.
      Roughness: float
      /// Dielectric (0) to conductor (1).
      Metallic: float
      /// Opaque (0) to fully transmissive (1). Ignored when metallic.
      Transmission: float
      /// Index of refraction of the far side RELATIVE to the near side, for the
      /// crossing about to happen. For an opaque surface in air this is just the
      /// material IOR; for a transmissive one the integrator flips it to
      /// etaOutside/etaInside when the ray is leaving the medium.
      Ior: float
      /// Normal-incidence reflectance (F0) of the dielectric specular lobe.
      /// Defaults to the IOR-derived 0.04 of common dielectrics; legacy mirror
      /// and glossy materials map their reflection colour straight onto it.
      SpecularF0: Colour
      /// Beer-Lambert filter applied per unit distance inside the medium.
      TransmissionFilter: Colour
      /// Shading normal in the local frame of the surface normal; +Z leaves the
      /// surface unperturbed. Normal and bump maps tilt it.
      Normal: Vector
      /// Fraction of the diffuse lobe that passes through a thin surface to the
      /// far side (leaves, membranes, paper) instead of reflecting.
      DiffuseTransmission: float
      /// Albedo of the diffuse transmission lobe.
      DiffuseTransmissionColour: Colour
      /// Grazing-angle tint of the diffuse lobe, in [0,1].
      Sheen: float
      SheenColour: Colour }

module SurfaceParams =
    let defaults =
        { BaseColour = Colour(0.5, 0.5, 0.5)
          Roughness = 1.
          Metallic = 0.
          Transmission = 0.
          Ior = 1.5
          SpecularF0 = Colour(0.04, 0.04, 0.04)
          TransmissionFilter = Colour.White
          Normal = Vector(0., 0., 1.)
          DiffuseTransmission = 0.
          DiffuseTransmissionColour = Colour.White
          Sheen = 0.
          SheenColour = Colour.White }

    let diffuse (colour: Colour) = { defaults with BaseColour = colour }

    /// Normal-incidence reflectance of a smooth dielectric boundary in air.
    let f0OfIor (ior: float) =
        let r = (1. - ior) / (1. + ior)
        let v = max 0. (min 1. (r * r))
        Colour(v, v, v)

/// One scattering event drawn from a BSDF.
[<Struct; NoEquality; NoComparison>]
type BsdfSample =
    { /// Outgoing direction, world space.
      Direction: Vector
      /// Throughput multiplier: f(wo,wi) * |cos wi| / pdf.
      Weight: Colour
      /// Solid-angle density of `Direction`. Zero for delta lobes.
      Pdf: float
      /// True for Dirac-delta lobes, where MIS must not weight against the light.
      IsSpecular: bool
      /// True when the ray crossed the surface rather than reflecting.
      IsTransmitted: bool }

module BsdfSample =
    let none =
        { Direction = Vector.Zero; Weight = Colour.Black; Pdf = 0.
          IsSpecular = false; IsTransmitted = false }

module Bsdf =
    let private black = Colour.Black

    /// Guards the Colour constructor, which rejects negative or non-finite
    /// components, against accumulated floating-point drift.
    let private clampColour (c: Colour) =
        let fix v = if Double.IsFinite v && v > 0. then v else 0.
        Colour(fix c.R, fix c.G, fix c.B)

    let private scale (c: Colour) (s: float) =
        if not (Double.IsFinite s) || s <= 0. then black
        else clampColour (Colour(c.R * s, c.G * s, c.B * s))

    let private mulColour (a: Colour) (b: Colour) =
        clampColour (Colour(a.R * b.R, a.G * b.G, a.B * b.B))

    let private addColour (a: Colour) (b: Colour) =
        clampColour (Colour(a.R + b.R, a.G + b.G, a.B + b.B))

    let private lerpColour (a: Colour) (b: Colour) t =
        clampColour (Colour(a.R + (b.R - a.R) * t, a.G + (b.G - a.G) * t, a.B + (b.B - a.B) * t))

    let private luminance (c: Colour) = 0.2126 * c.R + 0.7152 * c.G + 0.0722 * c.B

    let private clamp01 v = max 0. (min 1. v)

    /// True when the specular lobe collapses to a mirror. Below this alpha the
    /// VNDF math loses all precision, and a delta lobe is both cheaper and
    /// exactly the limit it converges to.
    let isSmooth (p: SurfaceParams) = Ggx.alphaOfRoughness p.Roughness < Ggx.smoothThreshold

    /// True when the surface scatters light diffusely to its far side.
    let hasDiffuseTransmission (p: SurfaceParams) =
        clamp01 p.DiffuseTransmission > 0. && clamp01 p.Metallic < 1. && clamp01 p.Transmission < 1.

    /// Probability of choosing each lobe when sampling, in the order
    /// (diffuse, specular reflection, transmission, diffuse transmission). Any
    /// positive normalized choice is unbiased; matching lobe albedo just lowers
    /// variance.
    ///
    /// `pdfLocal` and `sampleLocal` MUST read these from this one function -
    /// if the two ever disagree, MIS weights go silently wrong.
    let private selectionProbabilities (p: SurfaceParams) =
        let metallic = clamp01 p.Metallic
        let transmission = clamp01 p.Transmission * (1. - metallic)
        let translucency = clamp01 p.DiffuseTransmission
        let diffuse = (1. - metallic) * (1. - transmission) * (1. - translucency) * max 0.05 (luminance p.BaseColour)
        let diffuseTransmission =
            if translucency > 0. then
                (1. - metallic) * (1. - transmission) * translucency * max 0.05 (luminance p.DiffuseTransmissionColour)
            else 0.
        // Dielectric specular carries little energy but must never be starved,
        // or grazing highlights turn into fireflies.
        let specular = metallic + (1. - metallic) * (1. - transmission) * 0.25
        let total = diffuse + specular + transmission + diffuseTransmission
        if total <= 0. then struct (1., 0., 0., 0.)
        else struct (diffuse / total, specular / total, transmission / total, diffuseTransmission / total)

    /// Normal-incidence reflectance of the specular lobe, which is what the
    /// multiple-scattering compensation is parameterized on.
    let private normalIncidenceF0 (p: SurfaceParams) =
        lerpColour p.SpecularF0 p.BaseColour (clamp01 p.Metallic)

    /// Directional albedo of the dielectric specular layer, i.e. the energy the
    /// diffuse substrate below it does not receive.
    let private dielectricSpecularAlbedo (p: SurfaceParams) (wo: Vector) =
        if isSmooth p then luminance (Fresnel.schlick p.SpecularF0 wo.Z)
        else MultipleScattering.specularAlbedo (luminance p.SpecularF0) (Ggx.alphaOfRoughness p.Roughness) wo.Z

    /// Diffuse albedo, tinted towards the sheen colour at grazing half-angles.
    /// A convex blend, so sheen can never add energy.
    let private diffuseColour (p: SurfaceParams) (wo: Vector) (wi: Vector) =
        let sheen = clamp01 p.Sheen
        if sheen <= 0. then p.BaseColour
        else
            let wm = (wo + wi).Normalise
            let cosD = if wm.IsZero then 0. else clamp01 (wi * wm)
            let m = 1. - cosD
            lerpColour p.BaseColour p.SheenColour (sheen * m * m * m * m * m)

    /// f(wo,wi) * |cos wi| for the reflective lobes, local space.
    let private evalReflection (p: SurfaceParams) (wo: Vector) (wi: Vector) =
        if wo.Z <= 0. || wi.Z <= 0. then black
        else
            let metallic = clamp01 p.Metallic
            let transmission = clamp01 p.Transmission * (1. - metallic)
            // Albedo scaling: the diffuse substrate receives only what the
            // specular layer above it did not reflect. That must be the lobe's
            // ACTUAL directional albedo, not Fresnel at the view angle - at
            // grazing incidence F reaches 0.34 while a rough lobe reflects far
            // less, and the difference is energy that simply disappears.
            let f0DielectricColour = p.SpecularF0
            let specularAlbedo = dielectricSpecularAlbedo p wo
            let diffuseWeight =
                (1. - metallic) * (1. - transmission) * (1. - specularAlbedo) * (1. - clamp01 p.DiffuseTransmission)
            let diffuse = scale (diffuseColour p wo wi) (diffuseWeight * wi.Z / Math.PI)
            let specular =
                if isSmooth p then black
                else
                    let alpha = Ggx.alphaOfRoughness p.Roughness
                    let wm = (wo + wi).Normalise
                    if wm.IsZero || wm.Z <= 0. then black
                    else
                        let cosOm = max 0. (wo * wm)
                        let fMetal = Fresnel.schlick p.BaseColour cosOm
                        // Schlick, not the exact dielectric form, so that the
                        // lobe agrees with the Schlick-based split-sum table the
                        // diffuse substrate is scaled by. Mixing the two leaves
                        // a ~1% energy gain at cos 0.7. The exact form is still
                        // used for transmission, where TIR must be right.
                        let fDiel = Fresnel.schlick f0DielectricColour cosOm
                        let f = lerpColour fDiel fMetal metallic
                        let d = Ggx.distribution alpha wm
                        let g = Ggx.g2 alpha wo wi
                        // Restore the inter-microfacet bounces the single
                        // scattering model drops; without this a roughness-1
                        // metal loses ~62% of its energy.
                        let comp = MultipleScattering.compensation (normalIncidenceF0 p) alpha wo.Z
                        // f * D * G2 / (4 cosO cosI), times the |cos wi| we fold
                        // in, cancels one cosI.
                        scale (mulColour f comp) (d * g / (4. * wo.Z))
            addColour diffuse specular

    /// f(wo,wi) * |cos wi| for the thin-surface diffuse transmission lobe:
    /// a Lambertian lobe on the far side, fed by what the specular layer and
    /// the diffuse reflection leave over.
    let private evalDiffuseTransmission (p: SurfaceParams) (wo: Vector) (wi: Vector) =
        if wo.Z <= 0. || wi.Z >= 0. || not (hasDiffuseTransmission p) then black
        else
            let metallic = clamp01 p.Metallic
            let transmission = clamp01 p.Transmission * (1. - metallic)
            let weight =
                (1. - metallic) * (1. - transmission) * (1. - dielectricSpecularAlbedo p wo) * clamp01 p.DiffuseTransmission
            scale p.DiffuseTransmissionColour (weight * -wi.Z / Math.PI)

    /// Combined solid-angle pdf for the non-delta lobes. Delta lobes contribute
    /// zero, which is what MIS requires.
    let private pdfUnperturbed (p: SurfaceParams) (wo: Vector) (wi: Vector) =
        if wo.Z <= 0. then 0.
        elif wi.Z < 0. then
            let struct (_, _, _, pTranslucent) = selectionProbabilities p
            if pTranslucent > 0. then pTranslucent * (-wi.Z / Math.PI) else 0.
        elif wi.Z = 0. then 0.
        else
            let struct (pDiffuse, pSpecular, _, _) = selectionProbabilities p
            let mutable total = 0.
            if pDiffuse > 0. then
                total <- total + pDiffuse * SampleWarp.cosineHemispherePdf wi
            if pSpecular > 0. && not (isSmooth p) then
                let alpha = Ggx.alphaOfRoughness p.Roughness
                let wm = (wo + wi).Normalise
                if not wm.IsZero && wm.Z > 0. then
                    let jacobian = 1. / (4. * max 1e-9 (wo * wm))
                    total <- total + pSpecular * Ggx.visibleNormalPdf alpha wo wm * jacobian
            if Double.IsFinite total && total > 0. then total else 0.

    let private evalUnperturbed (p: SurfaceParams) (wo: Vector) (wi: Vector) =
        if wi.Z < 0. then evalDiffuseTransmission p wo wi else evalReflection p wo wi

    /// Refract `wo` (pointing away from the surface, local space, +Z side) about
    /// +Z. Returns None on total internal reflection.
    let private refract (wo: Vector) (eta: float) =
        let cosI = wo.Z
        let sinT2 = eta * eta * (1. - cosI * cosI)
        if sinT2 >= 1. then None
        else
            let cosT = sqrt (max 0. (1. - sinT2))
            Some(Vector(-wo.X * eta, -wo.Y * eta, -cosT))

    let private sampleUnperturbed (p: SurfaceParams) (wo: Vector) uLobe u1 u2 =
        if wo.Z <= 0. then BsdfSample.none
        else
            let struct (pDiffuse, pSpecular, pTransmission, pTranslucent) = selectionProbabilities p
            let metallic = clamp01 p.Metallic

            if uLobe < pDiffuse then
                let wi = SampleWarp.cosineHemisphere u1 u2
                let pdf = pdfUnperturbed p wo wi
                if pdf <= 0. then BsdfSample.none
                else
                    let f = evalUnperturbed p wo wi
                    { Direction = wi; Weight = scale f (1. / pdf); Pdf = pdf
                      IsSpecular = false; IsTransmitted = false }

            elif uLobe < pDiffuse + pSpecular then
                if isSmooth p then
                    // Perfect mirror: a delta lobe, so weight is f/pdf with both
                    // deltas cancelling, leaving just the Fresnel reflectance.
                    let wi = Vector(-wo.X, -wo.Y, wo.Z)
                    let fMetal = Fresnel.schlick p.BaseColour wo.Z
                    let fDiel = Fresnel.schlick p.SpecularF0 wo.Z
                    let f = lerpColour fDiel fMetal metallic
                    { Direction = wi; Weight = scale f (1. / pSpecular); Pdf = 0.
                      IsSpecular = true; IsTransmitted = false }
                else
                    let alpha = Ggx.alphaOfRoughness p.Roughness
                    let wm = Ggx.sampleVisibleNormal alpha wo u1 u2
                    let wi = (2. * (wo * wm) * wm - wo).Normalise
                    if wi.Z <= 0. then BsdfSample.none
                    else
                        let pdf = pdfUnperturbed p wo wi
                        if pdf <= 0. then BsdfSample.none
                        else
                            let f = evalUnperturbed p wo wi
                            { Direction = wi; Weight = scale f (1. / pdf); Pdf = pdf
                              IsSpecular = false; IsTransmitted = false }

            elif pTranslucent > 0. && uLobe >= 1. - pTranslucent then
                // Thin-surface diffuse transmission: a cosine lobe on the far side.
                let up = SampleWarp.cosineHemisphere u1 u2
                let wi = Vector(up.X, up.Y, -up.Z)
                let pdf = pdfUnperturbed p wo wi
                if pdf <= 0. then BsdfSample.none
                else
                    let f = evalDiffuseTransmission p wo wi
                    { Direction = wi; Weight = scale f (1. / pdf); Pdf = pdf
                      IsSpecular = false; IsTransmitted = true }

            elif pTransmission > 0. then
                // Smooth dielectric: reflect or refract, chosen by Fresnel.
                // Both branches are delta lobes.
                let fresnel = Fresnel.dielectric wo.Z 1. p.Ior
                let reflectProbability =
                    // Keep both branches sampled even at grazing angles so the
                    // estimator stays low-variance rather than merely unbiased.
                    max 0.05 (min 0.95 fresnel)
                let choose = (uLobe - pDiffuse - pSpecular) / pTransmission
                if choose < reflectProbability then
                    let wi = Vector(-wo.X, -wo.Y, wo.Z)
                    let weight = fresnel / (reflectProbability * pTransmission)
                    { Direction = wi; Weight = scale Colour.White weight; Pdf = 0.
                      IsSpecular = true; IsTransmitted = false }
                else
                    match refract wo (1. / p.Ior) with
                    | None -> BsdfSample.none
                    | Some wi ->
                        // Radiance compression across the boundary: the eta^2
                        // factor. Omitting it is a classic energy bug.
                        let etaScale = (1. / p.Ior) * (1. / p.Ior)
                        let weight = (1. - fresnel) * etaScale / ((1. - reflectProbability) * pTransmission)
                        { Direction = wi; Weight = scale p.BaseColour weight; Pdf = 0.
                          IsSpecular = true; IsTransmitted = true }
            else BsdfSample.none

    // ------------------------------------------------------------ normal maps

    let private isPerturbed (p: SurfaceParams) =
        let n = p.Normal
        not (n.X = 0. && n.Y = 0. && n.Z = 1.) && n.IsFinite && n.Z > 0.

    /// Frame of the perturbed shading normal. Where a normal map tilts the
    /// normal so far that the viewer would sit behind it, the normal is bent
    /// back towards the geometric one until it faces the viewer again;
    /// otherwise the surface turns black at silhouettes.
    let private perturbedFrame (p: SurfaceParams) (wo: Vector) =
        let n = p.Normal.Normalise
        let facing = wo * n
        let minimum = 0.5 * wo.Z
        let n =
            if facing >= minimum then n
            else
                let t = (minimum - facing) / (wo.Z - facing)
                ((1. - t) * n + t * Vector(0., 0., 1.)).Normalise
        ShadingFrame.ofNormal n

    /// A perturbed lobe may only reach the side of the true surface it claims
    /// to: reflection above it, transmission below.
    let private sameSide (outer: Vector) (inner: Vector) =
        (outer.Z > 0. && inner.Z > 0.) || (outer.Z < 0. && inner.Z < 0.)

    /// Combined solid-angle pdf for the non-delta lobes, local space.
    let pdfLocal (p: SurfaceParams) (wo: Vector) (wi: Vector) =
        if not (isPerturbed p) then pdfUnperturbed p wo wi
        elif wo.Z <= 0. then 0.
        else
            let frame = perturbedFrame p wo
            let wiP = frame.ToLocal wi
            if sameSide wi wiP then pdfUnperturbed p (frame.ToLocal wo) wiP else 0.

    /// f(wo,wi) * |cos wi|, local space. Zero for delta lobes by construction.
    let evalLocal (p: SurfaceParams) (wo: Vector) (wi: Vector) =
        if not (isPerturbed p) then evalUnperturbed p wo wi
        elif wo.Z <= 0. then black
        else
            let frame = perturbedFrame p wo
            let wiP = frame.ToLocal wi
            if sameSide wi wiP then evalUnperturbed p (frame.ToLocal wo) wiP else black

    /// Draw a scattering direction. `u1`,`u2` select within the chosen lobe and
    /// `uLobe` chooses the lobe.
    let sampleLocal (p: SurfaceParams) (wo: Vector) uLobe u1 u2 =
        if not (isPerturbed p) then sampleUnperturbed p wo uLobe u1 u2
        elif wo.Z <= 0. then BsdfSample.none
        else
            let frame = perturbedFrame p wo
            let s = sampleUnperturbed p (frame.ToLocal wo) uLobe u1 u2
            if s.Weight.IsBlack then s
            else
                let direction = (frame.ToWorld s.Direction).Normalise
                if sameSide direction s.Direction then { s with Direction = direction }
                else BsdfSample.none
