# F# Ray Tracer

A headless classic/Whitted ray tracer, modernized from the spring 2018
Second Year Project exam at the IT University of Copenhagen. The renderer
remains F#: direct lighting, shadows, reflection, glossy reflection, refraction,
meshes, CSG and implicit surfaces. It is **not a path tracer** and does not
simulate indirect diffuse illumination or refractive caustics.

## Run on Linux

Install the .NET SDK pinned in [`global.json`](global.json). No Windows Forms,
`System.Drawing.Bitmap`, X server or native image library is required.

```sh
dotnet restore RayTracer.sln --locked-mode
dotnet build RayTracer.sln -c Release --no-restore
dotnet UnitTests/bin/Release/net10.0/TDD.dll
dotnet run --project TracerTest -c Release --no-build -- --smoke
```

The small example writes `artifacts/smoke/cpu.png`. Tests are noninteractive
and return a nonzero exit code on failure; use `--list` or
`--suites geometry,mesh,numerics,lighting-regression` to select suites.
If `dotnet` was installed locally, add `$HOME/.dotnet` to `PATH`.

## Render and compare

The benchmark command prepares deterministic, original assets, builds isolated
Release workers, then renders identical scene descriptions through the frozen
legacy renderer and the corrected CPU renderer:

```sh
dotnet fsi scripts/benchmark.fsx -- \
  --presets quick --scenes chair --materials authored \
  --engines legacy,cpu --output artifacts/quick-comparison
```

Open `artifacts/quick-comparison/index.html` locally. Every measured attempt
retains its PNG, unclamped linear RGB PFM, settings, metrics, logs and hashes.
The report includes CSV/JSON results, before/after views, failures and image
differences. Nothing is uploaded.

| Scene | Original composition |
|---|---|
| `chair` | Bentwood chair with curved slats, joinery, brass details and upholstery |
| `roman-bust` | Procedural classical bust with facial features, hair, drapery and pedestal |
| `space-sentinel` | Original masked space-villain figure with helmet, armor and cape |
| `sky-arena` | Original stylized martial-arts scene with posed figures, architecture and energy accents |

Each scene supports `matte`, `phong`, `mirror`, `glossy`, `glass` and `authored`.
Only subject materials change; staging, cameras and lights remain fixed.
Default assets generate offline and are checked for closed, consistently
oriented topology before entering the glass sweep.

```sh
# Complete standard/high matrix; this is a substantial workload.
dotnet fsi scripts/benchmark.fsx -- \
  --presets standard,high --scenes all --materials all \
  --engines legacy,cpu --camera-samples 4 --repeats 1 --warmups 0 \
  --output artifacts/benchmarks

# Resume only completed attempts with matching fingerprints and intact files.
dotnet fsi scripts/benchmark.fsx -- \
  --presets standard,high --scenes all --materials all \
  --engines legacy,cpu --camera-samples 4 --repeats 1 --warmups 0 \
  --output artifacts/benchmarks --resume
```

Use `--help` for sample counts, repetitions, timeouts, thread/tile selection,
acceleration choices and custom scenes. `--engines cpu,gpu` explicitly selects
the optional CUDA backend. GPU compilation does not require a GPU, but rendering
does; unavailable devices, unsupported precision/features and device errors
fail explicitly, never becoming hidden CPU renders. See the
[GPU scope and diagnostics](RayTracer.Gpu/README.md).

The full-image command uses the documented pilot-calibrated collection
(512/1024 pixels, four camera samples, one cold observation per case).
It is not a five-repeat statistical study. Remove these overrides for the
larger default workload; see the calibration notes in the benchmark protocol.

`legacy-port` preserves the original algorithms and known rendering defects,
with audited Linux/compiler/image-output adaptations. It runs on the same
modern .NET runtime, **not the historical Windows runtime**. Lighting corrections
change both images and ray work; before/after time differences alone are not
pure implementation speedups. See the
[benchmark protocol](benchmarks/BENCHMARKS.md) and
[baseline audit](benchmarks/legacy/README.md).

## Detailed GPU images

For a higher-quality CUDA rendering workload, rather than the calibrated
four-camera-sample CPU comparison:

```sh
dotnet fsi scripts/benchmark.fsx -- \
  --presets high --scenes all --materials authored --engines gpu \
  --glossy-samples 4 --repeats 1 --warmups 0 \
  --output artifacts/gpu-detailed
```

This uses 1024x1024 images, 64 camera samples, four light/glossy samples and
four secondary bounces. Use `--materials matte,phong,mirror,glossy,glass`
with a separate output directory for the remaining variants. These are
quality renders with more work, not matched-work speedups over the CPU run.
The optional GPU remains FP64 CUDA compute, not hardware RT-core tracing.

For the current local collection, convenience copies are exposed under `artifacts/images/`, with plainly
named `old`/`new` preview files, `standard/` and `high/` CPU-side PNGs, and
`gpu/` for detailed CUDA images. `artifacts/images/index.html` is the image
overview. Its additional live galleries are `artifacts/gpu-detailed/live.html`
and `artifacts/gpu-detailed-materials/live.html`. These are local browsing
conveniences; the benchmark command itself reproducibly produces the final
`index.html`, trial data and nested original image artifacts.

The completed local CUDA collection contains **24 images**. Open
`artifacts/images/gpu/index.html` for the combined gallery, or see the
[collection results and boundaries](benchmarks/RESULTS.md).

## Original Gold Dragon

The original example referred to a missing `ply/dragon.ply`. Its optional
preparation script retrieves the unmodified Stanford reconstruction, verifies
pinned hashes and records attribution and separate usage terms:

```sh
dotnet fsi scripts/prepare-gold-dragon.fsx
dotnet run --project TracerTest -c Release -- --gold-dragon-preview
dotnet run --project TracerTest -c Release --no-build -- --gold-dragon
```

The final command preserves the original 1024x768 camera, 16 camera/light/glossy
samples, gold material, environment, infinite floor and two secondary bounces.
It writes `artifacts/gold-dragon/original-settings.png`; the preview has its own
clearly named output. The 871,414-triangle scan has holes and is deliberately
excluded from the solid-glass sweep.

For a separate neutral-scene before/after comparison, run
`dotnet fsi SceneAssets/GoldDragon.fsx` after preparation, then use the
[bonus benchmark command](benchmarks/BENCHMARKS.md#optional-gold-dragon-bonus).
That portable mesh-only scene explicitly uses a large finite floor slab;
it does not replace the native infinite-floor example.

This optional scan is **not GPL project data**. Stanford's credited
noncommercial research terms apply; commercial use requires permission.
See [`benchmarks/gold-dragon.json`](benchmarks/gold-dragon.json) and the generated
provenance. The normal benchmark does not download or depend on this asset.

## Animation

[`RayTracer.Animation`](RayTracer.Animation) adds keyframed and simulated
animation, and [`AnimationRunner`](AnimationRunner) renders it to numbered PNGs
and an MP4 (when `ffmpeg` is on `PATH`):

```sh
dotnet run --project AnimationRunner -c Release -- --list
dotnet run --project AnimationRunner -c Release -- --demo lamp --res 960x540 --spp 16
dotnet run --project AnimationRunner -c Release -- --scene shot.glb --clip Action --integrator path
dotnet run --project AnimationRunner -c Release -- --demo bouncing-ball --export-gltf bounce.glb
```

Frames go to `artifacts/anim/NAME/frame_00000.png`. An interrupted render
resumes from the first missing frame; `manifest.json` refuses to mix frames
rendered with different settings. Each frame gets its own seed unless
`--fixed-noise` is given.

- **Telegram delivery.** `--telegram` uploads the finished MP4 to a Telegram
  chat. Create a bot with [@BotFather](https://t.me/BotFather), send it a
  message, and read your chat ID from
  `https://api.telegram.org/bot<token>/getUpdates`. Then set
  `TELEGRAM_BOT_TOKEN` and `TELEGRAM_CHAT_ID` in the environment. Missing
  credentials fail before any rendering starts. The Bot API accepts videos of
  at most 50 MB. A failed upload keeps the frames and video and exits with
  code 3.
- **glTF 2.0 interchange.** `--scene` loads `.gltf`/`.glb` files as exported by
  Blender and other tools: node hierarchy, TRS animation with step, linear and
  cubic-spline keys, triangle meshes with base-colour textures, perspective
  cameras and `KHR_lights_punctual`. Metallic-roughness materials map to the
  nearest classic material, and anything lost is reported as a warning. Skins,
  morph targets and orthographic cameras are not supported. `--export-gltf`
  writes a scene back out for inspection in Blender. Analytic shapes are
  tessellated, procedural textures are baked to images, and aimed cameras and
  eased tracks are resampled.
- **Motion blur.** Rays carry a shutter time. Cameras stratify it across the
  shutter (`--shutter 0.5` is a 180° shutter), and moving objects are
  intersected at the pose for each ray's time. Rotations blur along arcs.
  Lights and the camera itself are placed at mid-shutter.
- **Authoring in code.** A scene is a tree of named nodes with rest TRS poses,
  animated by clips. `Smooth.vector`/`Smooth.rotation` give auto-clamped spline
  keys, and `Easing` covers anticipation (`backIn`) and follow-through
  (`backOut`). A camera can aim at any node.
- **Physics.** `Physics` simulates rigid spheres with restitution, Coulomb
  friction and rolling, against planes, boxes and each other. `Bake` turns a
  simulation into ordinary keyframe tracks, optionally with volume-preserving
  squash on impact and stretch in flight. Baked motion therefore exports to
  glTF and mixes with hand-keyed animation.

The demos are `hop`, `rolling-ball`, `bouncing-ball`, `camera-dolly` and
`lamp`.

### Dragon flight

`dragon-flight` is a 30-second short with sound. At golden hour, a dragon
crosses a mountain range, skims the summit's sunlit face, hovers to roar,
and flies into the sun, across five shots with camera cuts.

```sh
scripts/fetch-dragon-assets.sh          # CC0/public-domain model and recordings, see assets/dragon/SOURCES.md
dotnet run --project AnimationRunner -c Release -- --demo dragon-flight --res 960x540 --spp 56
# The film recommends path tracing, denoising, ACES, 64 spp, 3 bounces and a light bloom/vignette/grade; flags override:
dotnet run --project AnimationRunner -c Release -- --demo dragon-flight --res 960x540 --spp 16 --bloom 0.2 --vignette 0.3 --exposure 1.1 --saturation 1 --white-balance 0 --integrator classic --no-denoise --transfer srgb
dotnet run --project AnimationRunner -c Release -- --demo dragon-flight --audio-only   # iterate on the mix
scripts/review-stills.sh LABEL [flags]  # one still per shot plus a contact sheet, in artifacts/review/LABEL
```

At 960x540 and 56 spp a frame takes about 30 s on 128 cores, so the whole
film takes about 6 hours.

It uses the following:

- **Sky and sun.** A Preetham sky with a limb-darkened 0.53° sun disc,
  reddened by the air mass at a 6° elevation. The sun lights the scene as a
  directional light and the disc is only seen by camera and mirror rays, so
  it is counted once. The sky is importance-sampled from a luminance table,
  with MIS in the path tracer.
- **Aerial perspective.** Exponential height fog with closed-form
  transmittance, in-scattered sky light and a Henyey–Greenstein glow around
  the sun, applied to camera and secondary rays.
- **Physically based materials.** glTF materials import as a PBR material
  (base colour, metallic/roughness, normal, occlusion and emissive maps,
  `KHR_texture_transform`, bilinear filtering), plus sheen and a thin-sheet
  diffuse transmission lobe. The film overrides the model's materials by
  name and skinning region: cellular scales, translucent wing membranes
  that glow when back-lit, bone horns and wet eyes.
- **Skeletal skinning.** glTF skins are posed with linear blend skinning at
  every motion step and intersected as a deforming mesh, so beating wings
  blur along their arc.
- **`Clip.arrange`.** It sequences the model's own clips (looped fast flight,
  a hover, and the head-butt lunge used as the roar), with speed changes and
  crossfades.
- **Cameras.** `AnimatedScene.Cuts` switches between camera nodes. Tracking
  cameras move during the shutter, and the close-ups use a moving thin lens
  that autofocuses on the dragon.
- **Procedural terrain.** A ridged-noise heightfield with smooth normals and
  a world-space surface: noise-modulated rock strata, patchy forest and
  meadow, and a ragged snow line with a little specular. Shadow rays start
  from Hanika's terminator-corrected point, so low sun on a coarse mesh
  leaves no black ridge lines. A coarse ring of ranges closes the horizon.
- **Snow spindrift.** The dragon's downwash lifts powder off the snow as it
  skims the summit. The particles are a pure function of time and seed, so
  frames render independently.
- **Post-processing.** Multi-scale bloom, a grade and a vignette in linear
  light, after denoising and before ACES tone mapping.
- **A soundtrack derived from the animation.** Wing beats are detected from
  the wing bones and whooshes from fast, close passes. Everything is panned
  and attenuated from the active camera. Public-domain grizzly and alligator
  recordings, pitched down and run through a mountain reverb, make the roar.
  The mix is written as `soundtrack.wav` and muxed into the MP4.

The top-level acceleration structure is rebuilt every frame, while mesh
BVHs are built once and reused.

### Sponza: the knight's vigil

`sponza` is a 20-second short in Intel's Sponza 2022 atrium (CC-BY 4.0, with
its curtains, ivy and cypress add-ons). An armoured knight holds his guard in
the courtyard's sun patch, crouches, swings and advances. Five shots cover it,
from the scene's own high view to a crane that ends over him.

```sh
scripts/fetch-sponza-assets.sh          # Intel packages + knight cache, see assets/sponza/SOURCES.md
DOTNET_GCgen0size=0x10000000 dotnet run --project AnimationRunner -c Release -- --demo sponza --res 960x540 --spp 32
scripts/sponza/compare.sh 9             # our frame next to a Cycles reference of the same scene
```

`scenes/sponza.json` holds the parts, sun, HDRI sky, lamps, haze, knight
placement and shots. The F# demo (`RayTracer.Animation/Sponza.fs`) and the
Cycles ground truth (`scripts/sponza/cycles_reference.py`) both read it, so the
two renderers can be compared frame for frame. They agree to within a few
percent in brightness.

It uses the following:

- **HDRI sky with a separate sun.** A Radiance `.hdr` loader and an
  importance-sampled environment light. The map's own sun is clamped out and
  replaced by an analytic directional sun, so it casts crisp shadows without
  noise.
- **Many lights.** The 22 lanterns are sphere lights with inverse-square
  falloff and soft shadows. With more than eight local lights, a light BVH
  picks one per vertex.
- **Alpha-masked and double-sided surfaces** for leaves, and real glTF
  tangents (or generated ones) for normal maps.
- **The knight.** Intel's per-vertex animated USD is baked by Blender into a
  vertex cache (`scripts/sponza/convert-knight.py`). It is played back as a
  deforming mesh, so it gets motion blur.
- **Haze.** A bounded volume with shadowed single scattering from the sun and
  lamps (equiangular sampling for lamps), so sunbeams and lantern halos show
  in the air.
- **Speed.** Hit-time and occlusion-only fast paths through the BVHs,
  byte-encoded textures, GC settings, and tiles small enough to keep 128
  threads busy. OIDN is found in common install locations, including a
  Blender bundle; the runner warns when denoising was requested but could
  not run.

## Implementation

The modern renderer uses hit-local barycentrics and face orientation,
scale-aware ray origins, coherent bounded light samples, weighted secondary
transport, Fresnel/TIR and colored medium absorption. Deterministic sample keys
and tiled CPU work remove dependence on thread scheduling. Immutable mesh data,
cached transforms and a flat binned-SAH BVH provide the optimized CPU path.
The retained accelerators and brute-force queries support reference comparisons.
The [measured CPU tuning notes](benchmarks/PERFORMANCE.md) separate those
same-integrator improvements from before/after rendering-correctness changes.

[`SceneFormat`](SceneFormat/SceneFormat.fs) is the neutral input contract.
The CPU and legacy workers link the same scene adapter and run in separate
processes. PNG/JPEG handling lives in
[`RayTracer.ImageIO`](RayTracer.ImageIO/README.md).
[Scene asset documentation](SceneAssets/README.md) describes geometry, textures,
coordinates, provenance and regeneration.

Historical examples remain available through `TracerTest --list`, `--group`,
`--test` and explicit `--all`; some require external assets not distributed with
the repository. Generated images and large assets are excluded from Git.

## License

The original project remains under [GPL version 2](LICENSE). New procedural
benchmark assets carry their own recorded GPL-2.0-only attribution. Dependency
notices and optional third-party dataset terms are separate; do not assume the
project license licenses an external scan or texture.
