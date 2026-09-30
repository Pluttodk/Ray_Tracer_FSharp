namespace Tracer.SceneAssets

open System
open Tracer.SceneFormat

module Scenes =
    let private rgb r g b = [| r; g; b |]

    let private material id kind colour reflectivity =
        { Id = id; Kind = kind; Colour = colour
          AmbientColour = null; SpecularColour = rgb 1. 0.97 0.92; ReflectionColour = rgb 1. 1. 1.
          Ambient = 0.24; Diffuse = 0.70; Specular = 0.20; Exponent = 72
          Reflectivity = reflectivity; GlossExponent = 48; Ior = 1.5; Filter = rgb 0.88 0.96 0.94
          Emission = 0.; Texture = "" }

    let private matte id colour = material id MaterialKind.Matte colour 0.
    let private polished id colour = material id MaterialKind.Phong colour 0.
    let private glossy id colour reflection = material id MaterialKind.Glossy colour reflection
    let private emissive id colour intensity =
        { material id MaterialKind.Emissive colour 0. with Ambient = 0.; Diffuse = 0.; Specular = 0.; Emission = intensity }

    let private textureFor id =
        match id with
        | "walnut" -> "wood-grain"
        | "walnut-endgrain" -> "wood-endgrain"
        | "marble" | "hair-stone" | "toga" -> "marble-veins"
        | "teal-fabric" | "cape" | "hero-teal" | "hero-orange" | "hero-purple" -> "woven-fabric"
        | "rock" | "rock-top" | "arena-stone" -> "basalt"
        | "sentinel-shell" | "sentinel-panel" | "sentinel-copper" -> "brushed-metal"
        | _ -> ""

    let private palette =
        [| polished "walnut" (rgb 0.31 0.105 0.038)
           polished "walnut-endgrain" (rgb 0.19 0.057 0.022)
           glossy "brass" (rgb 0.62 0.36 0.10) 0.32
           matte "teal-fabric" (rgb 0.035 0.30 0.31)
           polished "teal-piping" (rgb 0.075 0.43 0.43)
           matte "charcoal" (rgb 0.055 0.071 0.09)
           matte "warm-floor" (rgb 0.53 0.43 0.34)
           matte "blue-wall" (rgb 0.065 0.14 0.22)
           matte "slate-stage" (rgb 0.22 0.27 0.29)
           polished "marble" (rgb 0.79 0.74 0.63)
           polished "marble-shade" (rgb 0.43 0.40 0.34)
           polished "marble-eye" (rgb 0.69 0.68 0.61)
           matte "marble-incision" (rgb 0.18 0.20 0.19)
           polished "hair-stone" (rgb 0.37 0.31 0.22)
           glossy "antique-bronze" (rgb 0.40 0.25 0.10) 0.28
           matte "toga" (rgb 0.46 0.53 0.52)
           matte "bust-wall" (rgb 0.12 0.20 0.22)
           polished "dark-pedestal" (rgb 0.105 0.13 0.13)
           glossy "sentinel-shell" (rgb 0.055 0.071 0.13) 0.30
           glossy "sentinel-panel" (rgb 0.105 0.145 0.20) 0.24
           glossy "sentinel-copper" (rgb 0.52 0.23 0.11) 0.34
           polished "sentinel-joint" (rgb 0.08 0.095 0.11)
           matte "cape" (rgb 0.19 0.045 0.135)
           matte "sentinel-wall" (rgb 0.09 0.055 0.13)
           emissive "visor" (rgb 0.12 0.72 0.83) 1.4
           emissive "amber-signal" (rgb 1. 0.25 0.045) 1.2
           emissive "reflection-card" (rgb 0.82 0.92 1.) 1.1
           { matte "sky" (rgb 0.055 0.24 0.59) with Ambient = 2.7; Diffuse = 0.; Specular = 0. }
           matte "cloud" (rgb 0.68 0.77 0.88)
           matte "cloud-shadow" (rgb 0.30 0.34 0.56)
           matte "rock" (rgb 0.16 0.055 0.30)
           matte "rock-top" (rgb 0.20 0.36 0.57)
           matte "arena-stone" (rgb 0.26 0.27 0.52)
           polished "arena-trim" (rgb 0.86 0.36 0.08)
           matte "hero-skin" (rgb 0.67 0.33 0.17)
           matte "hero-skin-shade" (rgb 0.33 0.12 0.065)
           matte "hero-teal" (rgb 0.025 0.49 0.47)
           matte "hero-orange" (rgb 0.90 0.22 0.045)
           matte "hero-purple" (rgb 0.24 0.055 0.42)
           polished "hero-hair" (rgb 0.035 0.045 0.11)
           matte "hero-wrap" (rgb 0.85 0.74 0.51)
           polished "hero-eye" (rgb 0.85 0.85 0.75)
           polished "hero-iris" (rgb 0.045 0.15 0.14)
           emissive "energy-blue" (rgb 0.09 0.65 1.) 1.8
           emissive "energy-gold" (rgb 1. 0.35 0.055) 1.5
           matte "lab-white" (rgb 0.70 0.70 0.68)
           matte "lab-red" (rgb 0.63 0.075 0.055)
           matte "lab-blue" (rgb 0.06 0.18 0.57)
           matte "lab-clay" (rgb 0.64 0.35 0.16)
           { material "lab-mirror" MaterialKind.Mirror (rgb 0.20 0.23 0.25) 0.88 with Diffuse = 0.08 }
           { material "lab-glass" MaterialKind.Glass (rgb 0.90 0.96 0.94) 0. with Diffuse = 0.05; Ambient = 0.02; Filter = rgb 0.78 0.95 0.91 }
           glossy "lab-glossy" (rgb 0.32 0.17 0.08) 0.65 |]
        |> Array.map (fun material ->
            match textureFor material.Id with
            | "" -> material
            | texture -> { material with Texture = $"../../artifacts/scene-assets/textures/{texture}.png" })

    type private Builder(library: Mesh array) =
        let objects = ResizeArray<ObjectSpec>()
        let subjects = ResizeArray<string>()
        member _.Add(subject, id, mesh, material, transform) =
            objects.Add { Id = id; Mesh = mesh; Material = material; Transform = transform }
            if subject then subjects.Add id
        member _.Scene(id, title, description, camera, ambient, lights) =
            let meshIds = objects |> Seq.map _.Mesh |> Set.ofSeq
            let materialIds = objects |> Seq.map _.Material |> Set.ofSeq
            { SchemaVersion = 1; Id = id; Title = title; Description = description
              Camera = camera; AmbientColour = rgb 0.90 0.94 1.; AmbientIntensity = ambient
              MaxBounces = 4
              Materials = palette |> Array.filter (fun material -> materialIds.Contains material.Id)
              Meshes =
                library |> Array.filter (fun mesh -> meshIds.Contains mesh.Id)
                |> Array.map (fun mesh ->
                    { Id = mesh.Id; Path = $"../../artifacts/scene-assets/meshes/{mesh.Id}.ply"; Smooth = mesh.Smooth; Closed = true })
              Objects = objects.ToArray(); Lights = lights; Subjects = subjects.ToArray() }
            |> SceneFiles.validate

    let private camera position target fov =
        let position, target = let (x, y, z), (a, b, c) = position, target in V3.create x y z, V3.create a b c
        let size = 2. * tan (fov * Math.PI / 360.)
        { Position = V3.toArray position; Target = V3.toArray target; Up = rgb 0. 1. 0.
          ViewDistance = 1.; ViewWidth = size; ViewHeight = size; LensRadius = 0.
          FocusDistance = V3.length (V3.sub target position) }

    let private point id (x, y, z) colour intensity =
        { Id = id; Kind = LightKind.Point; Position = rgb x y z; Direction = rgb 0. 0. 0.
          Colour = colour; Intensity = intensity; Size = [| 1.; 1. |] }

    let private directional id (x, y, z) colour intensity =
        { Id = id; Kind = LightKind.Directional; Position = rgb 0. 0. 0.; Direction = rgb x y z
          Colour = colour; Intensity = intensity; Size = [| 1.; 1. |] }

    let private add (b: Builder) subject id mesh material position size rotation =
        b.Add(subject, id, mesh, material, Matrix.trs position size rotation)

    let private put b subject id mesh material position size = add b subject id mesh material position size (0., 0., 0.)

    let private link (b: Builder) subject id mesh material (ax, ay, az) (bx, by, bz) rx rz =
        b.Add(subject, id, mesh, material, Matrix.between (V3.create ax ay az) (V3.create bx by bz) rx rz)

    let private studio b floor wall =
        put b false "studio-floor" "rounded-box" floor (0., -0.16, 0.) (12., 0.12, 10.)
        put b false "studio-backdrop" "rounded-box" wall (0., 5.0, -8.0) (14., 5.2, 0.12)
        put b false "left-reflection-card" "rounded-box" "reflection-card" (-6.5, 3.1, 2.8) (0.025, 2.15, 1.45)
        put b false "right-reflection-card" "rounded-box" "reflection-card" (6.5, 3.5, 2.8) (0.025, 1.65, 1.20)

    let private chair library =
        let b = Builder library
        let part = put b true
        studio b "warm-floor" "blue-wall"
        put b false "presentation-disc" "disc" "slate-stage" (0., 0.005, 0.) (1.67, 0.045, 1.57)
        put b false "presentation-rim" "ring" "brass" (0., 0.005, 0.) (1.62, 0.17, 1.52)
        part "seat-frame" "rounded-box" "walnut" (0., 1.09, 0.) (0.755, 0.125, 0.68)
        part "seat-cushion" "rounded-box" "teal-fabric" (0., 1.29, 0.015) (0.704, 0.155, 0.633)
        part "upholstery-piping" "seat-piping" "teal-piping" (0., 1.35, 0.015) (1., 1., 1.)
        for x, side in [ -1., "left"; 1., "right" ] do
            for z, endName in [ -1., "rear"; 1., "front" ] do
                let bottom, top = (0.79 * x, 0.06, 0.65 * z), (0.61 * x, 1.10, 0.51 * z)
                link b true $"{side}-{endName}-leg" "tapered-leg" "walnut" bottom top 0.09 0.09
                link b true $"{side}-{endName}-foot" "rounded-cylinder" "walnut-endgrain"
                    bottom (0.773 * x, 0.16, 0.637 * z) 0.073 0.073
            link b true $"{side}-side-stretcher" "rounded-cylinder" "walnut"
                (0.70 * x, 0.55, -0.58) (0.70 * x, 0.55, 0.58) 0.048 0.048
            link b true $"{side}-back-post" "rounded-cylinder" "walnut"
                (0.62 * x, 1.06, -0.50) (0.68 * x, 2.69, -0.73) 0.079 0.079
            add b true $"{side}-front-join-pin" "disc" "brass"
                (0.615 * x, 1.08, 0.676) (0.024, 0.006, 0.024) (90., 0., 0.)
        for y, name, z in [ 0.94, "front-apron", 0.54; 0.96, "rear-apron", -0.54; 0.48, "cross-stretcher", -0.55 ] do
            link b true name "rounded-cylinder" "walnut" (-0.64, y, z) (0.64, y, z) 0.047 0.047
        add b true "top-bentwood-rail" "chair-curved-rail" "walnut" (0., 2.64, -0.70) (0.735, 0.90, 0.80) (0., 0., 0.)
        add b true "lower-bentwood-rail" "chair-curved-rail" "walnut" (0., 1.68, -0.575) (0.675, 0.72, 0.66) (0., 0., 0.)
        for index in -2 .. 2 do
            let x = float index * 0.208
            let z = -0.615 - 0.070 * (1. - (x / 0.65) ** 2.)
            add b true $"back-slat-{index + 2}" "chair-curved-slat" "walnut"
                (x, 2.17, z) (1., 1., 1.) (-7., -float index * 3., 0.)
            for y, suffix in [ 1.78, "lower"; 2.53, "upper" ] do
                add b true $"slat-{index + 2}-{suffix}-pin" "disc" "brass"
                    (x, y, z + 0.063 - (y - 2.17) * 0.12) (0.016, 0.004, 0.016) (90., 0., 0.)
        for x in [ -0.27; 0.27 ] do
            for z in [ -0.24; 0.24 ] do
                part $"cushion-tuft-{x}-{z}" "small-sphere" "teal-piping" (x, 1.44, z) (0.023, 0.009, 0.023)
        part "maker-inlay" "rounded-box" "brass" (0., 1.085, 0.681) (0.064, 0.025, 0.006)
        b.Scene(
            "chair", "Arcback — walnut and woven teal",
            "Original bentwood chair: splayed tapered legs, visible brass join pins, bowed rails, five curved back slats, piped upholstered seat and a circular studio dais. Procedural original geometry; no borrowed furniture model. Visible emissive reflection cards are not indirect illumination.",
            camera (4.15, 3.0, 6.15) (0., 1.34, 0.) 34., 0.30,
            [| point "warm-key" (-3.2, 5.8, 4.8) (rgb 1. 0.86 0.68) 2.6
               point "cool-fill" (3.8, 3.4, 2.0) (rgb 0.56 0.76 1.) 0.95
               point "back-rim" (1.0, 4.6, -2.4) (rgb 0.83 0.93 1.) 1.7
               directional "front-lift" (0.1, 0.6, 1.) (rgb 0.92 0.96 1.) 0.22 |])

    let private romanBust library =
        let b = Builder library
        let part = put b true
        studio b "warm-floor" "bust-wall"
        put b false "plinth-bottom" "rounded-box" "dark-pedestal" (0., 0.11, 0.) (0.91, 0.15, 0.68)
        put b false "plinth-bronze-reveal" "rounded-box" "antique-bronze" (0., 0.28, 0.) (0.83, 0.032, 0.60)
        put b false "moulded-plinth" "moulded-pedestal" "dark-pedestal" (0., 0.90, 0.) (0.74, 0.60, 0.54)
        put b false "plinth-cap" "rounded-box" "marble" (0., 1.54, 0.) (0.77, 0.095, 0.56)
        put b false "plinth-inscription-panel" "rounded-box" "antique-bronze" (0., 0.97, 0.381) (0.27, 0.11, 0.012)
        for index in 0 .. 4 do
            put b false $"abstract-inscription-{index}" "rounded-box" "dark-pedestal"
                (-0.16 + float index * 0.08, 0.97, 0.397) (0.009, 0.040 + 0.007 * float (index % 3), 0.006)
        part "sculpted-shoulders" "classical-shoulders" "marble" (0., 1.61, 0.) (1., 1., 1.)
        part "neck" "small-sphere" "marble" (0., 2.82, -0.015) (0.29, 0.52, 0.275)
        part "head" "classical-head" "marble" (0., 3.55, 0.01) (0.66, 0.86, 0.66)
        part "nose" "classical-nose" "marble" (0., 3.55, 0.01) (0.66, 0.86, 0.66)
        part "chin-plane" "small-sphere" "marble" (0., 2.95, 0.38) (0.18, 0.105, 0.105)
        for side, name in [ -1., "left"; 1., "right" ] do
            part $"{name}-ear" "small-sphere" "marble" (0.545 * side, 3.49, 0.015) (0.12, 0.225, 0.11)
            part $"{name}-ear-concha" "small-sphere" "marble-shade" (0.567 * side, 3.49, 0.101) (0.059, 0.122, 0.018)
            add b true $"{name}-ear-helix" "ring" "marble"
                (0.564 * side, 3.51, 0.103) (0.075, 0.30, 0.161) (90., 0., 0.)
            part $"{name}-ear-lobe" "small-sphere" "marble" (0.547 * side, 3.33, 0.047) (0.08, 0.074, 0.074)
            part $"{name}-eye" "small-sphere" "marble-eye" (0.235 * side, 3.64, 0.462) (0.115, 0.061, 0.059)
            part $"{name}-iris" "small-sphere" "marble-shade" (0.224 * side, 3.638, 0.516) (0.037, 0.041, 0.010)
            part $"{name}-pupil-incision" "small-sphere" "marble-incision" (0.222 * side, 3.639, 0.525) (0.014, 0.020, 0.006)
            add b true $"{name}-upper-lid" "eyelid" "marble" (0.235 * side, 3.642, 0.506) (0.124, 0.074, 0.091) (0., 0., -3. * side)
            add b true $"{name}-lower-lid" "eyelid" "marble" (0.235 * side, 3.636, 0.501) (0.122, 0.041, 0.082) (0., 0., 180. - 3. * side)
            link b true $"{name}-carved-brow" "rounded-cylinder" "marble-shade"
                (0.10 * side, 3.782, 0.488) (0.382 * side, 3.755, 0.415) 0.024 0.035
            part $"{name}-nostril" "small-sphere" "marble-shade" (0.068 * side, 3.325, 0.665) (0.030, 0.016, 0.022)
            add b true $"{name}-upper-lip" "small-sphere" "marble"
                (0.063 * side, 3.142, 0.503) (0.089, 0.027, 0.043) (0., 0., 11. * side)
        part "mouth-incision" "small-sphere" "marble-shade" (0., 3.122, 0.536) (0.151, 0.010, 0.010)
        part "lower-lip" "small-sphere" "marble" (0., 3.102, 0.513) (0.133, 0.036, 0.038)
        part "hair-cap" "sphere" "hair-stone" (0., 4.079, -0.073) (0.553, 0.372, 0.524)
        for row in 0 .. 2 do
            let count = if row = 0 then 13 elif row = 1 then 11 else 8
            for index in 0 .. count - 1 do
                let angle = -Math.PI * 0.62 + 1.24 * Math.PI * float index / float (count - 1)
                let radius = 0.515 - float row * 0.085
                let x, z = radius * sin angle, -0.055 + (radius + 0.06) * cos angle
                let y = 4.035 + float row * 0.145 - 0.055 * cos angle
                add b true $"curl-{row}-{index}" "sculpted-curl" "hair-stone"
                    (x, y, z) (0.113 - float row * 0.008, 0.111, 0.120)
                    (-12. - float row * 26., angle * 180. / Math.PI, 19. * float ((index * 3 + row) % 5))
        for side, name in [ -1., "left"; 1., "right" ] do
            for index in 0 .. 4 do
                let t = float index / 4.
                add b true $"{name}-laurel-{index}" "leaf" "antique-bronze"
                    (side * (0.49 - 0.18 * t), 3.94 + 0.34 * t, 0.20 + 0.14 * t)
                    (0.12, 0.145, 0.16) (0., 20. * side, side * (-37. + 15. * t))
        part "draped-toga" "toga-drape" "toga" (0., 2.16, 0.02) (1., 1., 1.)
        add b true "toga-shoulder-fold" "rounded-cylinder" "toga" (-0.74, 2.54, 0.28) (0.18, 0.36, 0.16) (18., 0., -29.)
        add b true "toga-clasp" "disc" "antique-bronze" (-0.64, 2.69, 0.40) (0.087, 0.029, 0.087) (74., 0., 0.)
        part "clasp-center" "small-sphere" "marble" (-0.64, 2.702, 0.432) (0.032, 0.030, 0.019)
        b.Scene(
            "roman-bust", "An imagined orator — stone, laurel and drapery",
            "An original procedural classical/Roman-style bust, not a scan or a portrait of a historical person: modeled eye sockets, lids, irises, nasal bridge and nostrils, lips, ears with helix, individually instanced spiral curls, bronze laurel leaves and thick folded toga on a moulded plinth. All subject parts are individually closed solids.",
            camera (3.85, 3.08, 9.65) (0., 2.24, 0.) 34., 0.31,
            [| point "sculpture-key" (-3.6, 6.7, 5.4) (rgb 1. 0.89 0.72) 2.4
               point "sculpture-fill" (3.6, 3.9, 3.0) (rgb 0.63 0.82 1.) 0.95
               point "sculpture-rim" (1.1, 5.4, -2.2) (rgb 0.75 0.90 1.) 1.65
               directional "frontal-lift" (0., 0.3, 1.) (rgb 1. 0.98 0.91) 0.18 |])

    let private sentinel library =
        let b = Builder library
        let part = put b true
        studio b "charcoal" "sentinel-wall"
        put b false "sentinel-dais" "disc" "sentinel-joint" (0., 0.12, 0.) (1.92, 0.16, 1.52)
        put b false "sentinel-dais-ring" "ring" "sentinel-copper" (0., 0.18, 0.) (1.83, 0.50, 1.43)
        put b false "bust-mount" "moulded-pedestal" "sentinel-panel" (0., 0.64, -0.08) (0.56, 0.39, 0.39)
        part "cape" "sentinel-cape" "cape" (0., 0., 0.) (1., 1., 1.)
        part "cuirass" "armor-torso" "sentinel-shell" (0., 0.93, 0.) (1., 1., 1.)
        for index in 0 .. 3 do
            part $"neck-ring-{index}" "disc" "sentinel-joint" (0., 2.66 + float index * 0.106, 0.) (0.35, 0.039, 0.33)
        part "helmet" "sentinel-helmet" "sentinel-shell" (0., 3.72, 0.) (0.78, 0.86, 0.78)
        add b true "mask-outline" "sentinel-faceplate" "sentinel-copper" (0., 3.38, 0.564) (0.81, 0.71, 0.35) (3., 0., 0.)
        add b true "mask" "sentinel-faceplate" "sentinel-panel" (0., 3.396, 0.625) (0.755, 0.659, 0.35) (3., 0., 0.)
        add b true "visor-recess" "sentinel-visor" "charcoal" (0., 3.965, 0.606) (0.755, 0.87, 0.92) (-5., 0., 0.)
        add b true "broken-chevron-visor" "sentinel-visor" "visor" (0., 3.981, 0.674) (0.713, 0.57, 0.24) (-5., 0., 0.)
        link b true "crown-copper-seam" "rounded-cylinder" "sentinel-copper" (-0.27, 4.16, 0.524) (-0.22, 4.55, 0.29) 0.017 0.022
        add b true "offset-crown-fin" "sentinel-fin" "sentinel-panel" (-0.24, 4.50, -0.065) (0.28, 0.44, 0.63) (0., 9., -12.)
        add b true "crown-fin-inlay" "sentinel-fin" "sentinel-copper" (-0.24, 4.50, 0.001) (0.10, 0.35, 0.20) (0., 9., -12.)
        for side, name in [ -1., "left"; 1., "right" ] do
            add b true $"{name}-temple" "armor-panel" "sentinel-panel"
                (side * 0.60, 3.68, 0.10) (0.34, 0.53, 0.45) (0., side * 63., side * -13.)
            add b true $"{name}-ear-sensor" "disc" "sentinel-copper"
                (side * 0.689, 3.81, -0.04) (0.142, 0.027, 0.142) (0., 0., 90.)
            add b true $"{name}-sensor-lens" "disc" "sentinel-joint"
                (side * 0.718, 3.81, -0.04) (0.083, 0.029, 0.083) (0., 0., 90.)
            part $"{name}-shoulder-joint" "small-sphere" "sentinel-joint" (side * 1.02, 2.34, -0.02) (0.38, 0.35, 0.36)
            add b true $"{name}-pauldron-trim" "armor-panel" "sentinel-copper"
                (side * 1.12, 2.43, 0.08) (0.63, 0.57, 1.65) (-8., side * 13., side * -26.)
            add b true $"{name}-pauldron" "armor-panel" "sentinel-shell"
                (side * 1.12, 2.452, 0.155) (0.594, 0.531, 1.47) (-8., side * 13., side * -26.)
            add b true $"{name}-pectoral-trim" "armor-panel" "sentinel-copper"
                (side * 0.48, 2.02, 0.443) (0.565, 0.525, 0.52) (0., side * 17., side * 8.)
            add b true $"{name}-pectoral" "armor-panel" "sentinel-panel"
                (side * 0.48, 2.034, 0.495) (0.529, 0.485, 0.47) (0., side * 17., side * 8.)
            for index in 0 .. 2 do
                add b true $"{name}-abdominal-lamella-{index}" "rounded-box" "sentinel-panel"
                    (side * 0.315, 1.53 - float index * 0.15, 0.43 - float index * 0.030)
                    (0.27 - float index * 0.026, 0.049, 0.08) (0., side * 16., side * -9.)
            add b true $"{name}-cape-clasp" "disc" "sentinel-copper"
                (side * 0.70, 2.61, 0.341) (0.071, 0.025, 0.071) (82., 0., 0.)
        add b true "chest-insignia" "octagonal-stone" "sentinel-copper" (0.03, 2.13, 0.584) (0.135, 0.026, 0.17) (90., 0., 0.)
        add b true "insignia-light" "sentinel-fin" "visor" (0.03, 2.15, 0.625) (0.065, 0.097, 0.10) (0., 0., -18.)
        for index in 0 .. 2 do
            add b true $"offset-cheek-vent-{index}" "rounded-box" "charcoal"
                (0.225, 3.50 - float index * 0.075, 0.704) (0.085 - float index * 0.01, 0.012, 0.006) (0., 0., 9.)
        part "cheek-status-light" "small-sphere" "amber-signal" (-0.25, 3.46, 0.714) (0.014, 0.042, 0.007)
        b.Scene(
            "space-sentinel", "Vesper Sentinel — an original masked guardian",
            "Original science-fiction armored bust with a twelve-facet elongated helmet, offset crown fin, broken-chevron cyan visor, ungrilled asymmetric mask, copper-edged cuirass, broad segmented pauldrons and wine-purple cape. Not Darth Vader or a replica costume. Emissive visor/status details are visible accents; separate explicit lights supply direct illumination.",
            camera (4.30, 3.66, 10.15) (0., 2.41, 0.) 34., 0.36,
            [| point "warm-key" (-3.9, 6.8, 5.1) (rgb 1. 0.78 0.57) 2.65
               point "cyan-fill" (4.2, 3.6, 3.0) (rgb 0.39 0.83 1.) 1.35
               point "violet-rim" (1.2, 5.9, -2.25) (rgb 0.74 0.39 1.) 2.75
               directional "soft-front" (0.2, 0.7, 1.) (rgb 0.78 0.87 1.) 0.45 |])

    let private skyArena library =
        let b = Builder library
        let part = put b true
        put b false "sky-backdrop" "rounded-box" "sky" (0., 5., -6.2) (18., 11., 0.12)
        put b false "cloud-deck" "rounded-box" "cloud-shadow" (0., -2.25, 0.) (18., 0.12, 16.)
        for index, x, y, z, size in
            [ 0, -3.0, -1.26, -1.8, 1.40; 1, -1.45, -1.44, -2.8, 1.7; 2, 0.8, -1.48, -2.0, 1.6
              3, 2.8, -1.28, -2.8, 1.45; 4, 3.65, -0.90, -4.0, 1.20; 5, -3.2, 0.35, -5.0, 1.2 ] do
            put b false $"cloud-{index}" "sphere" "cloud" (x, y, z) (size, 0.32 + 0.05 * float (index % 3), size * 0.59)
        add b false "main-floating-island" "floating-rock" "rock" (-0.50, 0.15, 0.) (1.30, 1.05, 1.06) (0., 18., 0.)
        add b false "main-arena-surface" "octagonal-stone" "rock-top" (-0.50, 1.18, 0.) (1.30, 0.085, 1.06) (0., 18., 0.)
        put b false "island-energy-seam" "ring" "energy-gold" (-0.50, 1.04, 0.) (1.15, 0.22, 0.96)
        add b false "distant-floating-island" "floating-rock" "rock" (2.40, 0.66, -2.10) (1.04, 0.86, 0.83) (0., -23., 0.)
        add b false "distant-arena-surface" "octagonal-stone" "rock-top" (2.40, 1.49, -2.10) (1.05, 0.07, 0.83) (0., -23., 0.)
        for index, position, size, rotation in
            [ 0, (-2.35, 0.65, 0.25), (0.29, 0.34, 0.23), (5., 38., -12.)
              1, (1.65, -0.26, -0.40), (0.29, 0.51, 0.27), (9., -13., 18.)
              2, (-1.65, -0.77, 0.58), (0.19, 0.25, 0.18), (22., 17., 0.)
              3, (2.12, 0.71, 0.82), (0.17, 0.27, 0.19), (-14., 41., 19.) ] do
            add b false $"floating-fragment-{index}" "floating-rock" "rock" position size rotation
        put b false "ruined-pylon-base" "octagonal-stone" "arena-stone" (2.57, 1.61, -2.20) (0.37, 0.10, 0.34)
        put b false "ruined-pylon" "octagonal-stone" "arena-stone" (2.57, 2.48, -2.20) (0.22, 0.84, 0.22)
        put b false "pylon-orange-cap" "octagonal-stone" "arena-trim" (2.57, 3.36, -2.20) (0.34, 0.07, 0.31)
        put b false "pylon-glowing-cap" "octagonal-stone" "energy-blue" (2.57, 3.46, -2.20) (0.19, 0.025, 0.18)
        for index in 0 .. 6 do
            let angle = (34. + float index * 19.) * Math.PI / 180.
            let x, y = -0.30 + 2.32 * cos angle, 1.15 + 2.88 * sin angle
            add b false $"broken-arch-block-{index}" "rounded-box" (if index % 3 = 0 then "arena-trim" else "arena-stone")
                (x, y, -3.0) (0.29, 0.39, 0.29) (0., 0., 90. - angle * 180. / Math.PI)
        part "pelvis" "small-sphere" "hero-purple" (-0.08, 2.46, 0.13) (0.29, 0.23, 0.235)
        add b true "tunic-body" "rounded-box" "hero-teal" (0.035, 2.94, 0.13) (0.31, 0.48, 0.23) (0., -5., -12.)
        add b true "orange-wrap-front" "tunic-panel" "hero-orange" (0.00, 2.97, 0.365) (0.27, 0.37, 0.36) (0., -5., -12.)
        add b true "left-tunic-tail" "tunic-panel" "hero-teal" (-0.24, 2.37, 0.30) (0.20, 0.27, 0.30) (7., -16., -14.)
        add b true "right-tunic-tail" "tunic-panel" "hero-teal" (0.17, 2.37, 0.29) (0.18, 0.24, 0.28) (8., 13., 18.)
        add b true "waist-belt" "disc" "hero-wrap" (-0.07, 2.54, 0.13) (0.32, 0.047, 0.25) (0., -5., -8.)
        add b true "belt-clasp" "octagonal-stone" "arena-trim" (-0.13, 2.55, 0.393) (0.065, 0.021, 0.071) (90., 0., 0.)
        link b true "neck" "rounded-cylinder" "hero-skin" (0.12, 3.29, 0.13) (0.21, 3.53, 0.16) 0.105 0.10
        add b true "neck-scarf" "ring" "hero-orange" (0.155, 3.38, 0.16) (0.19, 0.74, 0.16) (0., 0., -11.)
        add b true "streaming-scarf" "hero-scarf" "hero-orange" (0.02, 3.37, -0.105) (0.88, 0.83, 0.82) (0., 0., 163.)
        let leftShoulder, leftElbow, leftWrist = (-0.29, 3.23, 0.17), (-0.87, 3.06, 0.30), (-1.36, 3.32, 0.52)
        let rightShoulder, rightElbow, rightWrist = (0.40, 3.26, 0.11), (0.91, 3.59, -0.015), (0.65, 3.90, 0.18)
        for side, shoulder, elbow, wrist in
            [ "left", leftShoulder, leftElbow, leftWrist; "right", rightShoulder, rightElbow, rightWrist ] do
            link b true $"{side}-upper-sleeve" "rounded-cylinder" "hero-orange" shoulder elbow 0.159 0.153
            part $"{side}-elbow" "small-sphere" "hero-skin" elbow (0.121, 0.122, 0.116)
            link b true $"{side}-forearm" "tapered-leg" "hero-skin" wrist elbow 0.104 0.101
            let wx, wy, wz = wrist
            let ex, ey, ez = elbow
            link b true $"{side}-wrist-wrap" "rounded-cylinder" "hero-wrap" wrist
                (wx * 0.73 + ex * 0.27, wy * 0.73 + ey * 0.27, wz * 0.73 + ez * 0.27) 0.113 0.105
        add b true "open-palm" "small-sphere" "hero-skin" (-1.465, 3.407, 0.558) (0.13, 0.100, 0.055) (0., 0., -24.)
        for index in 0 .. 3 do
            let t = float index
            link b true $"open-hand-finger-{index}" "rounded-cylinder" "hero-skin"
                (-1.51 + 0.045 * t, 3.447 + 0.01 * t, 0.56)
                (-1.58 + 0.065 * t, 3.57 + 0.013 * sin t, 0.57) 0.022 0.022
        link b true "open-hand-thumb" "rounded-cylinder" "hero-skin" (-1.48, 3.36, 0.565) (-1.62, 3.40, 0.59) 0.033 0.031
        add b true "raised-fist" "small-sphere" "hero-skin" (0.592, 3.991, 0.23) (0.114, 0.115, 0.085) (0., 0., -25.)
        for index in 0 .. 3 do
            part $"fist-knuckle-{index}" "small-sphere" "hero-skin" (0.52 + float index * 0.041, 4.054, 0.263) (0.029, 0.035, 0.034)
        let legParts =
            [ "planted", (-0.24, 2.43, 0.09), (-0.82, 1.96, 0.26), (-0.70, 1.42, 0.26), (-0.76, 1.345, 0.40), -15.
              "raised", (0.13, 2.42, 0.06), (0.75, 2.33, 0.31), (1.04, 1.92, 0.17), (1.17, 1.85, 0.33), 34. ]
        for name, hip, knee, ankle, foot, yaw in legParts do
            link b true $"{name}-thigh" "rounded-cylinder" "hero-purple" hip knee 0.153 0.147
            part $"{name}-knee" "small-sphere" "hero-purple" knee (0.146, 0.145, 0.141)
            link b true $"{name}-shin" "tapered-leg" "hero-purple" ankle knee 0.122 0.112
            let kx, ky, kz = knee
            part $"{name}-kneepad" "small-sphere" "hero-orange" (kx, ky, kz + 0.125) (0.100, 0.119, 0.032)
            let ax, ay, az = ankle
            part $"{name}-boot-cuff" "rounded-cylinder" "hero-wrap" (ax, ay + 0.028, az) (0.135, 0.125, 0.12)
            add b true $"{name}-boot" "rounded-box" "hero-wrap" foot (0.157, 0.097, 0.255) (0., yaw, 0.)
            let fx, fy, fz = foot
            add b true $"{name}-boot-sole" "rounded-box" "hero-hair" (fx, fy - 0.077, fz) (0.159, 0.026, 0.257) (0., yaw, 0.)
        let headFrame = Matrix.trs (0.205, 3.737, 0.19) (1., 1., 1.) (0., -15., -11.)
        let head id mesh material position size rotation =
            b.Add(true, id, mesh, material, Matrix.multiply headFrame (Matrix.trs position size rotation))
        head "hero-head" "sphere" "hero-skin" (0., 0., 0.) (0.282, 0.337, 0.266) (0., 0., 0.)
        head "hero-jaw" "small-sphere" "hero-skin" (0., -0.175, 0.084) (0.202, 0.174, 0.196) (0., 0., 0.)
        head "hero-nose" "small-sphere" "hero-skin" (0., -0.055, 0.276) (0.065, 0.065, 0.077) (0., 0., 0.)
        for side, name in [ -1., "left"; 1., "right" ] do
            head $"{name}-hero-ear" "small-sphere" "hero-skin" (side * 0.273, -0.019, 0.012) (0.061, 0.094, 0.055) (0., 0., 0.)
            head $"{name}-hero-ear-inset" "small-sphere" "hero-skin-shade" (side * 0.285, -0.017, 0.059) (0.028, 0.052, 0.01) (0., 0., 0.)
            head $"{name}-hero-eye" "small-sphere" "hero-eye" (side * 0.105, 0.038, 0.244) (0.067, 0.043, 0.034) (0., side * 12., 0.)
            head $"{name}-hero-iris" "small-sphere" "hero-iris" (side * 0.094 - 0.01, 0.039, 0.275) (0.024, 0.030, 0.006) (0., 0., 0.)
            head $"{name}-hero-brow" "rounded-box" "hero-hair" (side * 0.104, 0.101, 0.245) (0.075, 0.017, 0.017) (0., side * 10., side * 9.)
        head "hero-mouth" "small-sphere" "hero-skin-shade" (0., -0.154, 0.260) (0.078, 0.010, 0.007) (0., 0., -5.)
        head "cropped-hair-base" "sphere" "hero-hair" (0., 0.202, -0.047) (0.301, 0.200, 0.286) (0., 0., 0.)
        for index in 0 .. 5 do
            let t = float index
            head $"swept-lock-{index}" "swept-hair-lock" "hero-hair"
                (-0.19 + t * 0.056, 0.227 + 0.01 * sin t, 0.151 - t * 0.023)
                (0.46 - 0.021 * t, 0.48, 0.43) (8., -9. + 8. * t, 18. + 3. * t)
        head "side-lock" "swept-hair-lock" "hero-hair" (-0.268, 0.167, 0.083) (0.35, 0.34, 0.39) (0., -30., -108.)
        put b false "palm-energy-core" "sphere" "energy-blue" (-1.83, 3.72, 0.66) (0.135, 0.135, 0.135)
        add b false "palm-energy-orbit" "ring" "energy-blue" (-1.83, 3.72, 0.66) (0.34, 0.18, 0.34) (67., 8., -22.)
        add b false "palm-energy-cross-orbit" "ring" "energy-gold" (-1.83, 3.72, 0.66) (0.27, 0.13, 0.27) (15., -18., 52.)
        for index in 0 .. 4 do
            let angle = float index * 2. * Math.PI / 5.
            add b false $"energy-spark-{index}" "sentinel-fin" "energy-blue"
                (-1.83 + 0.51 * cos angle, 3.72 + 0.46 * sin angle, 0.65)
                (0.027, 0.065, 0.045) (0., 0., angle * 180. / Math.PI - 90.)
        b.Scene(
            "sky-arena", "Aster at the suspended arena",
            "Original stylized martial-arts action composition: a teal-and-tangerine courier in a planted/bent-knee stance, open casting palm and raised fist, swept asymmetric short hair, boots, wraps and wind-swept scarf. Faceted floating islands, fragments, a broken masonry arc, ambient-only painted blue backdrop/opaque cloud staging and cyan/gold energy accents. No licensed character references. Emission is visible only; explicit point/directional lights, not GI, illuminate the scene.",
            camera (3.60, 3.85, 9.90) (0., 1.80, -0.25) 38., 0.37,
            [| point "sun-key" (-3.7, 7.3, 5.2) (rgb 1. 0.85 0.63) 2.6
               point "sky-fill" (4.3, 4.9, 2.7) (rgb 0.49 0.72 1.) 1.05
               point "violet-rim" (-0.3, 5.7, -3.6) (rgb 0.73 0.47 1.) 1.8
               directional "high-sun" (-0.5, 1., 0.25) (rgb 1. 0.92 0.75) 0.34
               point "energy-direct-accent" (-1.55, 3.84, 1.16) (rgb 0.12 0.64 1.) 0.22 |])

    let private materialLab library =
        let b = Builder library
        put b false "floor" "rounded-box" "lab-white" (0., -0.12, 0.) (2.5, 0.12, 2.7)
        put b false "back" "rounded-box" "lab-white" (0., 2.15, -2.45) (2.5, 2.25, 0.12)
        put b false "left" "rounded-box" "lab-red" (-2.5, 2.15, 0.) (0.12, 2.25, 2.45)
        put b false "right" "rounded-box" "lab-blue" (2.5, 2.15, 0.) (0.12, 2.25, 2.45)
        put b false "ceiling" "rounded-box" "lab-white" (0., 4.35, 0.) (2.5, 0.12, 2.45)
        for x, id, material in [ -1.50, "clay-sphere", "lab-clay"; 0., "glass-sphere", "lab-glass"; 1.50, "mirror-sphere", "lab-mirror" ] do
            put b true id "sphere" material (x, 0.70, 0.55) (0.65, 0.65, 0.65)
            put b false $"{id}-stand" "disc" "charcoal" (x, 0.025, 0.55) (0.71, 0.026, 0.71)
        add b true "rear-glossy-block" "rounded-box" "lab-glossy" (-0.78, 0.83, -1.23) (0.59, 0.83, 0.55) (0., -19., 0.)
        put b true "rear-polished-sphere" "sphere" "marble" (0.80, 1.22, -1.15) (0.52, 0.52, 0.52)
        put b false "rear-pedestal" "rounded-box" "lab-white" (0.80, 0.35, -1.15) (0.54, 0.35, 0.54)
        put b false "visible-ceiling-emitter" "rounded-box" "reflection-card" (0., 4.19, -0.15) (0.77, 0.015, 0.47)
        let area =
            { Id = "ceiling-rectangle"; Kind = LightKind.Rectangle; Position = rgb 0. 4.14 -0.15
              Direction = rgb 0. -1. 0.; Colour = rgb 1. 0.92 0.78; Intensity = 0.65; Size = [| 1.5; 0.9 |] }
        b.Scene(
            "material-lab", "Direct-light material laboratory",
            "Cornell-style diagnostic box for direct visibility, point/rectangle lighting, matte, Phong, glossy, mirror and closed glass geometry. This is a classic direct-light scene: no claimed indirect color bleeding or path-traced transport. Point fill deliberately keeps the frozen legacy area-light defect from making the scene unreadable.",
            camera (0., 2.60, 8.1) (0., 1.55, -0.30) 39., 0.20,
            [| point "reliable-point-key" (-0.6, 3.8, 2.1) (rgb 1. 0.92 0.79) 2.3
               point "cool-point-fill" (1.75, 2.5, 1.6) (rgb 0.65 0.82 1.) 0.40; area |])

    let all library =
        [| chair library; romanBust library; sentinel library; skyArena library; materialLab library |]
