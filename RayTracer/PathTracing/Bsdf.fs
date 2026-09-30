namespace Tracer.Basics.PathTracing

open System
open Tracer.Basics

/// Orthonormal shading frame. Directions in "local" space have the shading
/// normal as +Z, which is what every formula in this file assumes.
[<Struct; NoEquality; NoComparison>]
type ShadingFrame =
    { TangentU: Vector
      TangentV: Vector
      Normal: Vector }

    member this.ToLocal(world: Vector) =
        Vector(world * this.TangentU, world * this.TangentV, world * this.Normal)

    member this.ToWorld(local: Vector) =
        local.X * this.TangentU + local.Y * this.TangentV + local.Z * this.Normal

module ShadingFrame =
    /// Duff et al., "Building an Orthonormal Basis, Revisited" (JCGT 2017).
    /// Branchless and stable at the poles, unlike an up-vector cross product.
    let ofNormal (normal: Vector) =
        let n = normal.Normalise
        if n.IsZero || not n.IsFinite then
            invalidArg (nameof normal) "A shading frame requires a finite nonzero normal."
        let sign = if n.Z >= 0. then 1. else -1.
        let a = -1. / (sign + n.Z)
        let b = n.X * n.Y * a
        { TangentU = Vector(1. + sign * n.X * n.X * a, sign * b, -sign * n.X)
          TangentV = Vector(b, sign + n.Y * n.Y * a, -n.Y)
          Normal = n }

module Fresnel =
    /// Exact unpolarized Fresnel for a dielectric boundary. `cosI` is measured
    /// against the side the ray arrives from, so it is always nonnegative here.
    let dielectric cosI etaI etaT =
        let cosI = max 0. (min 1. cosI)
        let eta = etaI / etaT
        let sinT2 = eta * eta * (1. - cosI * cosI)
        if sinT2 >= 1. then 1. // total internal reflection
        else
            let cosT = sqrt (max 0. (1. - sinT2))
            let rParallel = (etaT * cosI - etaI * cosT) / (etaT * cosI + etaI * cosT)
            let rPerp = (etaI * cosI - etaT * cosT) / (etaI * cosI + etaT * cosT)
            0.5 * (rParallel * rParallel + rPerp * rPerp)

    /// Schlick approximation, used for the conductor lobe where the exact
    /// complex-IOR form buys nothing visually.
    let schlick (f0: Colour) cosTheta =
        let m = max 0. (min 1. (1. - cosTheta))
        let m2 = m * m
        let weight = m2 * m2 * m
        Colour(f0.R + (1. - f0.R) * weight,
               f0.G + (1. - f0.G) * weight,
               f0.B + (1. - f0.B) * weight)

/// Trowbridge-Reitz (GGX) microfacet distribution with the height-correlated
/// Smith masking-shadowing term.
module Ggx =
    /// Below this, the lobe is treated as a perfect mirror: the VNDF sampling
    /// math loses all precision as alpha approaches zero, and a delta lobe is
    /// both cheaper and exactly what the limit converges to.
    let smoothThreshold = 1e-4

    let alphaOfRoughness (roughness: float) =
        let r = max 0. (min 1. roughness)
        r * r

    /// D(wm): normal distribution function, wm in local space.
    let distribution alpha (wm: Vector) =
        if wm.Z <= 0. then 0.
        else
            let a2 = alpha * alpha
            let cos2 = wm.Z * wm.Z
            let t = cos2 * (a2 - 1.) + 1.
            if t <= 0. then 0. else a2 / (Math.PI * t * t)

    /// Smith Lambda; the auxiliary term both G1 and G2 are built from.
    let private lambda alpha (w: Vector) =
        let cos2 = w.Z * w.Z
        if cos2 >= 1. then 0.
        elif cos2 <= 0. then infinity
        else
            let tan2 = (1. - cos2) / cos2
            0.5 * (sqrt (1. + alpha * alpha * tan2) - 1.)

    /// Single-direction masking.
    let g1 alpha (w: Vector) =
        let l = lambda alpha w
        if Double.IsPositiveInfinity l then 0. else 1. / (1. + l)

    /// Height-correlated masking-shadowing. Cheaper and less energy-losing than
    /// the separable G1(wo)*G1(wi) form.
    let g2 alpha (wo: Vector) (wi: Vector) =
        let l = lambda alpha wo + lambda alpha wi
        if Double.IsPositiveInfinity l then 0. else 1. / (1. + l)

    /// Sample a visible normal. Dupuy & Benyoub, "Sampling Visible GGX Normals
    /// with Spherical Caps" (2023) - simpler and faster than the classic
    /// Heitz projection, and numerically better behaved at grazing angles.
    let sampleVisibleNormal alpha (wo: Vector) u1 u2 =
        // Warp the view direction into the hemisphere configuration.
        let woStd = Vector(wo.X * alpha, wo.Y * alpha, wo.Z).Normalise
        // Sample the spherical cap visible from woStd.
        let b = woStd.Z
        let z = (1. - u1) * (1. + b) - b
        let sinTheta = sqrt (max 0. (min 1. (1. - z * z)))
        let phi = 2. * Math.PI * u2
        let c = Vector(sinTheta * cos phi, sinTheta * sin phi, z)
        let wmStd = c + woStd
        // Unwarp back to the ellipsoid configuration.
        Vector(wmStd.X * alpha, wmStd.Y * alpha, wmStd.Z).Normalise

    /// Density of `sampleVisibleNormal`, in solid angle over wm.
    let visibleNormalPdf alpha (wo: Vector) (wm: Vector) =
        if wo.Z <= 0. then 0.
        else
            let d = distribution alpha wm
            if d = 0. then 0.
            else g1 alpha wo * d * max 0. (wo * wm) / wo.Z

module SampleWarp =
    /// Cosine-weighted hemisphere via concentric disk mapping, which preserves
    /// stratification far better than the polar mapping.
    let cosineHemisphere u1 u2 =
        let a = 2. * u1 - 1.
        let b = 2. * u2 - 1.
        if a = 0. && b = 0. then Vector(0., 0., 1.)
        else
            // The radius stays SIGNED: a negative r reflects the point through
            // the origin, which is what covers the far half of the disk. Taking
            // abs here collapses the disk into a half-disk and silently biases
            // every diffuse bounce.
            let r, phi =
                if a * a > b * b then a, (Math.PI / 4.) * (b / a)
                else b, (Math.PI / 2.) - (Math.PI / 4.) * (a / b)
            let x = r * cos phi
            let y = r * sin phi
            Vector(x, y, sqrt (max 0. (1. - x * x - y * y)))

    let cosineHemispherePdf (wi: Vector) =
        if wi.Z <= 0. then 0. else wi.Z / Math.PI

/// Single-scattering microfacet models lose energy: rays that would have
/// bounced a second time between microfacets are simply dropped, so a rough
/// metal darkens badly (measured here: 62% of energy missing at roughness 1).
///
/// This module tabulates three quantities over (alpha, cos theta), all with the
/// same VNDF estimator, where the per-sample weight collapses to G2/G1:
///
///   E     - directional albedo of the white-furnace (F=1) lobe. Drives the
///           Turquin (2019) compensation factor 1 + F0*(1-E)/E.
///   A, B  - split-sum coefficients such that the lobe's true directional
///           albedo is F0*A + B for any F0 (Karis). This is what the diffuse
///           substrate must be scaled by: using 1 - F(cos theta) instead
///           destroys energy at grazing angles, because a rough lobe does not
///           actually reflect the grazing Fresnel value.
///
/// Tabulated rather than fitted, so there is no published curve to
/// mis-transcribe; the table is exact by construction up to its resolution.
module MultipleScattering =
    let private muCount = 32
    let private alphaCount = 32
    let private sqrtSamples = 64

    [<Literal>]
    let private MinAlpha = 1e-4

    let private build () =
        let e = Array.zeroCreate (alphaCount * muCount)
        let a = Array.zeroCreate (alphaCount * muCount)
        let b = Array.zeroCreate (alphaCount * muCount)
        for ai in 0 .. alphaCount - 1 do
            // Tabulate against sqrt(alpha): the low-roughness end is where E
            // changes fastest, so that is where the resolution belongs.
            let alpha = let t = (float ai + 0.5) / float alphaCount in max MinAlpha (t * t)
            for mi in 0 .. muCount - 1 do
                let mu = max 1e-3 ((float mi + 0.5) / float muCount)
                let wo = Vector(sqrt (max 0. (1. - mu * mu)), 0., mu)
                let g1 = Ggx.g1 alpha wo
                let mutable sumE = 0.
                let mutable sumA = 0.
                let mutable sumB = 0.
                if g1 > 0. then
                    // Deterministic stratified grid; this table must not vary
                    // between runs or renders stop being reproducible.
                    for i in 0 .. sqrtSamples - 1 do
                        for j in 0 .. sqrtSamples - 1 do
                            let u1 = (float i + 0.5) / float sqrtSamples
                            let u2 = (float j + 0.5) / float sqrtSamples
                            let wm = Ggx.sampleVisibleNormal alpha wo u1 u2
                            let wi = (2. * (wo * wm) * wm - wo).Normalise
                            if wi.Z > 0. then
                                let weight = Ggx.g2 alpha wo wi / g1
                                let cosOm = max 0. (min 1. (wo * wm))
                                let m = 1. - cosOm
                                let fc = m * m * m * m * m
                                sumE <- sumE + weight
                                sumA <- sumA + weight * (1. - fc)
                                sumB <- sumB + weight * fc
                    let inv = 1. / float (sqrtSamples * sqrtSamples)
                    sumE <- sumE * inv
                    sumA <- sumA * inv
                    sumB <- sumB * inv
                e.[ai * muCount + mi] <- max 1e-3 (min 1. sumE)
                a.[ai * muCount + mi] <- max 0. (min 1. sumA)
                b.[ai * muCount + mi] <- max 0. (min 1. sumB)
        e, a, b

    let private tables = lazy (build ())

    let private lookup (table: float[]) (alpha: float) (mu: float) =
        let af = (sqrt (max MinAlpha (min 1. alpha))) * float alphaCount - 0.5
        let mf = (max 0. (min 1. mu)) * float muCount - 0.5
        let a0 = max 0 (min (alphaCount - 1) (int (floor af)))
        let a1 = min (alphaCount - 1) (a0 + 1)
        let m0 = max 0 (min (muCount - 1) (int (floor mf)))
        let m1 = min (muCount - 1) (m0 + 1)
        let da = max 0. (min 1. (af - float a0))
        let dm = max 0. (min 1. (mf - float m0))
        let v00 = table.[a0 * muCount + m0]
        let v01 = table.[a0 * muCount + m1]
        let v10 = table.[a1 * muCount + m0]
        let v11 = table.[a1 * muCount + m1]
        let v0 = v00 + (v01 - v00) * dm
        let v1 = v10 + (v11 - v10) * dm
        v0 + (v1 - v0) * da

    /// Directional albedo of the single-scattering lobe under a white furnace.
    let directionalAlbedo (alpha: float) (mu: float) =
        let e, _, _ = tables.Force()
        lookup e alpha mu

    /// True directional albedo of the specular lobe for a given normal-incidence
    /// reflectance, via the split-sum approximation F0*A + B.
    let specularAlbedo (f0: float) (alpha: float) (mu: float) =
        let _, a, b = tables.Force()
        let value = f0 * lookup a alpha mu + lookup b alpha mu
        max 0. (min 1. value)

    /// Per-channel scale restoring the missing multiple-scattering energy.
    let compensation (f0: Colour) (alpha: float) (mu: float) =
        let e = directionalAlbedo alpha mu
        if e >= 1. || e <= 0. then Colour.White
        else
            let k = (1. - e) / e
            Colour(1. + f0.R * k, 1. + f0.G * k, 1. + f0.B * k)
