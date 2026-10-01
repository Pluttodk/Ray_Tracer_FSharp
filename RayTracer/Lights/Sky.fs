namespace Tracer.Basics

open System
open Tracer.Basics.Sampling
open Tracer.Basics.Textures

/// How a sky is set up. Brightness is art-directed through two numbers rather than raw photometric
/// units: the sun's irradiance on a surface facing it, in render units, and optionally how many
/// times brighter that is than the sky's irradiance on a horizontal surface.
type SkySettings =
    { /// Towards the sun; normalised by `Sky.create`.
      SunDirection: Vector
      /// Preetham turbidity, 1.7 (very clear) to 10 (hazy).
      Turbidity: float
      /// Luminance of the sun's irradiance at normal incidence, in render units.
      SunIrradiance: float
      /// Sun irradiance (normal incidence) over sky irradiance (horizontal surface), both luminance.
      /// None keeps the physical ratio of the model.
      SunToSky: float option }

/// An analytic daylight sky: the Preetham, Shirley and Smits (1999) model with an attenuated sun.
///
/// * Sky radiance comes from the Perez distribution for luminance Y and chromaticity x, y, fitted to
///   the sun's zenith angle and the turbidity, converted to linear sRGB. Below the horizon it fades
///   to a darkened copy of the horizon, standing in for ground that the scene does not model.
/// * The sun is a disc of 0.53 degrees whose extraterrestrial luminance is attenuated per channel by
///   Rayleigh and aerosol (Angstrom) optical depth over the Kasten-Young air mass, which reddens it
///   near the horizon. The disc is limb darkened (blue darkens more than red).
///
/// The sun is represented exactly once in the light transport: as a DirectionalLight (`Sky.sunLight`)
/// carrying the disc's whole irradiance. The environment light (`Sky.light`) lights the scene with
/// the sky alone and shows the disc only to camera rays and perfectly specular chains, which can
/// never receive the DirectionalLight. Glossy reflections get the sun's highlight from the
/// DirectionalLight through the BSDF, not from the disc, so nothing is counted twice and the tiny,
/// extremely bright disc never has to be found by random sampling.
type Sky =
    { Settings: SkySettings
      SunDirection: Vector
      /// Multiplies the model's sky radiance (kcd/m^2) into render units.
      SkyScale: float
      /// Mean radiance of the sun disc, render units.
      SunRadiance: Colour
      /// Perez coefficients A..E for Y, x and y.
      PerezY: float[]
      PerezX: float[]
      PerezYy: float[]
      /// Zenith luminance (kcd/m^2) and chromaticity.
      Zenith: float * float * float
      /// Perez value at the zenith for Y, x and y (the normalisers).
      ZenithF: float * float * float }

[<RequireQualifiedAccess>]
module Sky =
    /// Angular radius of the sun disc (0.533 degrees across).
    let sunAngularRadius = 0.533 / 2. * Math.PI / 180.
    let sunSolidAngle = 2. * Math.PI * (1. - cos sunAngularRadius)

    /// Extraterrestrial solar illuminance, klux; luminances below are in kcd/m^2 like Preetham's Y.
    let private solarIlluminance = 127.5
    /// Effective wavelengths (micrometres) of the linear sRGB primaries.
    let private wavelengths = [| 0.680; 0.550; 0.440 |]
    /// Limb-darkening coefficients per channel: I(mu)/I(1) = 1 - u (1 - mu).
    let private limb = [| 0.50; 0.62; 0.74 |]

    let luminance (c: Colour) = 0.2126 * c.R + 0.7152 * c.G + 0.0722 * c.B

    let private perez (c: float[]) (cosTheta: float) (gamma: float) (cosGamma: float) =
        (1. + c.[0] * exp (c.[1] / cosTheta)) * (1. + c.[2] * exp (c.[3] * gamma) + c.[4] * cosGamma * cosGamma)

    let private xyYToRgb (x: float) (y: float) (luma: float) =
        if y <= 0. || luma <= 0. then Colour.Black
        else
            let bigX = x / y * luma
            let bigZ = (1. - x - y) / y * luma
            let r = 3.2406 * bigX - 1.5372 * luma - 0.4986 * bigZ
            let g = -0.9689 * bigX + 1.8758 * luma + 0.0415 * bigZ
            let b = 0.0557 * bigX - 0.2040 * luma + 1.0570 * bigZ
            Colour(max 0. r, max 0. g, max 0. b)

    /// Kasten-Young (1989) relative optical air mass for a zenith angle in radians.
    let private airMass (zenith: float) =
        let degrees = min 93.9 (zenith * 180. / Math.PI)
        1. / (cos (degrees * Math.PI / 180.) + 0.50572 * Math.Pow(96.07995 - degrees, -1.6364))

    /// Model sky radiance (kcd/m^2, linear sRGB) for a direction at or above the horizon.
    let private modelSky (sky: Sky) (direction: Vector) =
        let s = sky.SunDirection
        let cosTheta = max 1e-3 direction.Y
        let cosGamma = max -1. (min 1. (direction * s))
        let gamma = acos cosGamma
        let zY, zx, zy = sky.Zenith
        let fY, fx, fy = sky.ZenithF
        let luma = zY * perez sky.PerezY cosTheta gamma cosGamma / fY
        let x = zx * perez sky.PerezX cosTheta gamma cosGamma / fx
        let y = zy * perez sky.PerezYy cosTheta gamma cosGamma / fy
        xyYToRgb x y luma

    /// Sky radiance seen along `direction` (towards the sky), render units, without the sun disc.
    /// This is what lights the scene, and the in-scatter colour for aerial perspective.
    let radiance (sky: Sky) (direction: Vector) =
        let d = direction.Normalise
        if d.Y >= 0. then modelSky sky d * sky.SkyScale
        else
            // Darkened horizon below it, fading in over a few degrees so there is no seam.
            let horizontal = Vector(d.X, 0., d.Z)
            let h = if horizontal.Magnitude > 1e-9 then horizontal.Normalise else Vector(1., 0., 0.)
            let fade = 0.35 + 0.65 * exp (d.Y * 30.)
            modelSky sky h * (sky.SkyScale * fade)

    let sunDirection (sky: Sky) = sky.SunDirection

    /// Mean radiance of the sun disc, render units.
    let sunRadiance (sky: Sky) = sky.SunRadiance

    /// The sun's irradiance at normal incidence, render units (mean disc radiance times its solid angle).
    let sunIrradiance (sky: Sky) = sky.SunRadiance * sunSolidAngle

    /// Limb-darkened sun disc radiance along `direction`; black outside the disc.
    let discRadiance (sky: Sky) (direction: Vector) =
        let cosine = direction.Normalise * sky.SunDirection
        let cosRadius = cos sunAngularRadius
        if cosine < cosRadius || sky.SunDirection.Y < -sunAngularRadius then Colour.Black
        else
            let angle = acos (min 1. cosine)
            let r = min 1. (angle / sunAngularRadius)
            let mu = sqrt (max 0. (1. - r * r))
            // Normalised so the disc's mean is SunRadiance: the mean of 1 - u(1 - mu) is 1 - u/3.
            let darken i = (1. - limb.[i] * (1. - mu)) / (1. - limb.[i] / 3.)
            Colour(sky.SunRadiance.R * darken 0, sky.SunRadiance.G * darken 1, sky.SunRadiance.B * darken 2)

    /// What a camera ray sees: the sky plus the sun disc.
    let visible (sky: Sky) (direction: Vector) = radiance sky direction + discRadiance sky direction

    /// Model (unscaled) sky irradiance luminance on a horizontal surface, kcd/m^2 * sr.
    let private horizontalIrradiance (sky: Sky) =
        let rows, columns = 96, 192
        let mutable sum = 0.
        for j in 0 .. rows - 1 do
            // Midpoint rule in (cos theta, phi), where the solid-angle element is d(cos theta) d(phi).
            let c0, c1 = 1. - float j / float rows, 1. - float (j + 1) / float rows
            let cosine = 0.5 * (c0 + c1)
            let sine = sqrt (1. - cosine * cosine)
            for i in 0 .. columns - 1 do
                let phi = 2. * Math.PI * (float i + 0.5) / float columns
                let d = Vector(sine * sin phi, cosine, sine * cos phi)
                sum <- sum + luminance (modelSky sky d) * cosine * (c0 - c1) * (2. * Math.PI / float columns)
        sum

    let create (settings: SkySettings) =
        let t = settings.Turbidity
        if not (Double.IsFinite t) || t < 1.7 || t > 10. then
            invalidArg (nameof settings) "Turbidity must be in [1.7, 10]."
        if not (Double.IsFinite settings.SunIrradiance) || settings.SunIrradiance <= 0. then
            invalidArg (nameof settings) "Sun irradiance must be finite and positive."
        match settings.SunToSky with
        | Some r when not (Double.IsFinite r) || r <= 0. -> invalidArg (nameof settings) "SunToSky must be positive."
        | _ -> ()
        let s = settings.SunDirection.Normalise
        if not (Double.IsFinite s.X && Double.IsFinite s.Y && Double.IsFinite s.Z) then
            invalidArg (nameof settings) "Sun direction must be finite and nonzero."
        if s.Y < 0.01 then invalidArg (nameof settings) "The sun must be above the horizon (elevation of at least 0.6 degrees)."
        let thetaS = acos s.Y
        let perezY = [| 0.1787 * t - 1.4630; -0.3554 * t + 0.4275; -0.0227 * t + 5.3251; 0.1206 * t - 2.5771; -0.0670 * t + 0.3703 |]
        let perezX = [| -0.0193 * t - 0.2592; -0.0665 * t + 0.0008; -0.0004 * t + 0.2125; -0.0641 * t - 0.8989; -0.0033 * t + 0.0452 |]
        let perezYy = [| -0.0167 * t - 0.2608; -0.0950 * t + 0.0092; -0.0079 * t + 0.2102; -0.0441 * t - 1.6537; -0.0109 * t + 0.0529 |]
        let chi = (4. / 9. - t / 120.) * (Math.PI - 2. * thetaS)
        let zY = max 1e-3 ((4.0453 * t - 4.9710) * tan chi - 0.2155 * t + 2.4192)
        let cubic (a: float[]) = a.[0] * thetaS ** 3. + a.[1] * thetaS ** 2. + a.[2] * thetaS + a.[3]
        let t2 = t * t
        let zx = t2 * cubic [| 0.00166; -0.00375; 0.00209; 0. |] + t * cubic [| -0.02903; 0.06377; -0.03202; 0.00394 |] + cubic [| 0.11693; -0.21196; 0.06052; 0.25886 |]
        let zy = t2 * cubic [| 0.00275; -0.00610; 0.00317; 0. |] + t * cubic [| -0.04214; 0.08970; -0.04153; 0.00516 |] + cubic [| 0.15346; -0.26756; 0.06670; 0.26688 |]
        let atZenith (c: float[]) = perez c 1. thetaS s.Y
        // Sun: extraterrestrial disc luminance, attenuated per channel along the slant path.
        let mass = airMass thetaS
        let beta = 0.04608 * t - 0.04586
        let transmittance =
            wavelengths |> Array.map (fun l ->
                let rayleigh = 0.008735 * Math.Pow(l, -4.08)
                let aerosol = beta * Math.Pow(l, -1.3)
                exp (-mass * (rayleigh + aerosol)))
        let sunModel = Colour(transmittance.[0], transmittance.[1], transmittance.[2]) * (solarIlluminance / sunSolidAngle)
        let unscaled =
            { Settings = settings; SunDirection = s; SkyScale = 1.; SunRadiance = sunModel
              PerezY = perezY; PerezX = perezX; PerezYy = perezYy
              Zenith = (zY, zx, zy); ZenithF = (atZenith perezY, atZenith perezX, atZenith perezYy) }
        let sunNormal = luminance sunModel * sunSolidAngle
        let skyHorizontal = horizontalIrradiance unscaled
        // Scale the sun relative to the sky if asked, then both into render units.
        let sunBoost = match settings.SunToSky with Some r -> r * skyHorizontal / sunNormal | None -> 1.
        let exposure = settings.SunIrradiance / (sunNormal * sunBoost)
        { unscaled with SkyScale = exposure; SunRadiance = sunModel * (sunBoost * exposure) }

    /// The sun's irradiance on a surface facing it over the sky's irradiance on a horizontal one
    /// (luminance), as rendered: the sky integrated over the upper hemisphere without the disc.
    let sunToSkyRatio (sky: Sky) =
        luminance (sunIrradiance sky) / (horizontalIrradiance sky * sky.SkyScale)

    /// The sun as a DirectionalLight carrying the disc's whole irradiance.
    let sunLight (sky: Sky) =
        let e = sunIrradiance sky
        DirectionalLight(e, 1., sky.SunDirection)

    /// The sky as an importance-sampled environment light. It lights with `radiance` (no disc) and
    /// shows `visible` (with the disc) to camera and specular rays; pair it with `sunLight`.
    /// The classic integrator takes `samplesPerAxis`^2 samples per hit (the path tracer draws its
    /// own); `tableWidth` is the width of the luminance table (its height is half of it).
    let light (sky: Sky) (samplesPerAxis: int) (tableWidth: int) =
        let texture =
            mkTexture (fun u v ->
                // Inverse of EnvironmentLight's lat-long mapping, for consumers that read the texture.
                let polar = (1. - v) * Math.PI
                let azimuth = 2. * Math.PI * u
                let d = Vector(sin polar * sin azimuth, cos polar, sin polar * cos azimuth)
                EmissiveMaterial(radiance sky d, 1.) :> Material)
        EnvironmentLight(1e6, texture, multiJittered samplesPerAxis 17, Some (radiance sky), Some (visible sky), tableWidth)
