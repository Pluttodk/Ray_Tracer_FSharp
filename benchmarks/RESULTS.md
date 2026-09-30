# Local rendering results

Collection date: 2026-09-07. Images and large assets are local ignored
artifacts, not uploaded or included in the repository.

## Completed detailed GPU collection

**24 CUDA images are complete:** four original scenes, each rendered as
authored, matte, Phong, mirror, glossy and glass.

| Setting | Recorded value |
|---|---|
| Resolution | 1024x1024 |
| Camera samples | 64 per pixel |
| Glossy samples | 4 per branch |
| Light samples | 4 for sampled lights |
| Secondary depth | 4 |
| Arithmetic / linear storage | FP64 / RGB32 PFM |
| Sampler / display transfer | Regular / gamma2 |
| Device | NVIDIA RTX 3000 Ada Generation Laptop GPU |
| Backend | F# CUDA compute, not RT-core tracing |
| Observations | One cold process per case; no warmup |

Open **`artifacts/images/gpu/index.html`** for the combined gallery.
The same directory contains plainly named PNGs, `sources.json` and
`timings.csv`. Original PNG/PFM/metrics/logs, manifests and reports are retained
under `artifacts/gpu-detailed/` (four authored scenes) and
`artifacts/gpu-detailed-materials/` (20 uniform-material scenes).

The authored and material runs used separately frozen source/worker copies.
The material revision includes the corrected GPU shared-triangle-edge
calculation. Both versions and their fingerprints remain recorded; completed
images were not silently relabeled as a different build.

These are higher-work quality renders, **not a matched-work GPU speedup over
the CPU collection below**. They remain classic/Whitted images: no indirect
diffuse lighting, physically correct refractive caustics, or path tracing.
Glass subjects consist of closed, sometimes intersecting components rather
than one Boolean-unioned shell. Finite depth can truncate paths through
multiple interfaces; transparent shadows use straight-segment attenuation.

## Accepted CPU comparison

The user accepted the existing CPU comparison and requested that it stop
before the detailed GPU collection. **55 completed PNGs remain available:**

| Preset | Completed images | Remaining status |
|---|---:|---|
| Standard, 512x512 | 47 | One original legacy sampler crash |
| High, 1024x1024 | 8 | Stopped at user request |

Each completed case used four camera samples, four light samples, one glossy
sample and its declared preset depth, identically for legacy and modern CPU.
The original sampler's parallel array-resize race was preserved and reported,
not patched to manufacture a successful baseline.

Open `artifacts/benchmarks/index.html` for the intentionally cancelled
collection's retained results, or `artifacts/images/standard/` and `high/`
for plainly named copies. Do not describe this as a completed 96-image
CPU matrix or a statistical study.

An earlier invocation was interrupted when concurrent development changed
source/binaries after its freeze. Its 24 existing images remain in the
attempt history, with invalidated trial records and an archived report under
`artifacts/benchmarks/interrupted-invocation/`. They are not timing evidence.
Subsequent collections used isolated working copies so development could
not overwrite active renderer assemblies.

For actual same-integrator CPU acceleration experiments, rather than
before/after correctness changes, see [PERFORMANCE.md](PERFORMANCE.md).

## Gold Dragon and preview images

`artifacts/images/gold-dragon-new-1024x768.png` is the **modern CPU**
render with the original Gold Dragon scene/settings: sixteen camera/light/
glossy samples, two bounces and the genuine infinite blue floor.
It is not a legacy-engine or GPU render.

The old/new Gold preview pair in `artifacts/images/` is 128x96 with reduced
sampling and the separately documented finite staging slab. There is no
claimed full-resolution legacy Gold render. The optional Stanford scan and
derived research renders retain Stanford's attribution and noncommercial
terms, separately from the project's GPL license.

`artifacts/images/index.html` links the complete GPU gallery, accepted
CPU-side images, original-settings Gold Dragon and earlier four-scene
before/after previews.
