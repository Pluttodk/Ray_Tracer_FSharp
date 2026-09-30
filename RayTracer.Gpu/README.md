# Optional F# CUDA classic renderer

This worker implements bounded **classic/Whitted** rendering in F#-authored CUDA
compute kernels through ILGPU. It does not implement path tracing, diffuse
indirect illumination, hardware RT-core traversal, or a CPU fallback.

The optional project does not change the CPU renderer. Its host preparation
uses the shared `SceneFormat`, robust `PLYParser`, CPU BVH builder, and portable
`RgbImage`/`Colour` output boundary. Only flattened value data enters kernels.

## Build and run

Requires the repository's .NET SDK and a working NVIDIA CUDA driver. On the
validated WSL2 installation, use the existing Windows-host driver exposure;
**do not install a Linux display driver**.

From the repository root:

```sh
mkdir -p RayTracer.Gpu/.work
export TMPDIR="$PWD/RayTracer.Gpu/.work"
export LD_LIBRARY_PATH="/usr/lib/wsl/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"

/home/movj/.dotnet/dotnet restore RayTracer.Gpu/RayTracer.Gpu.fsproj --locked-mode
/home/movj/.dotnet/dotnet build RayTracer.Gpu/RayTracer.Gpu.fsproj \
  -c Release --no-restore

/home/movj/.dotnet/dotnet RayTracer.Gpu/bin/Release/net10.0/RayTracer.Gpu.dll \
  --scene benchmarks/scenes/chair.json \
  --material matte \
  --settings RayTracer.Gpu/Tests/tiny-settings.json \
  --output RayTracer.Gpu/validation/chair-matte.png \
  --linear RayTracer.Gpu/validation/chair-matte.pfm \
  --metrics RayTracer.Gpu/validation/chair-matte.metrics.json
```

`dotnet` on PATH can replace the installation-specific executable above.
Generate the shared procedural scene assets using the repository's scene
preparation command before rendering if they are not already present.

Every successful render saves PNG, retained linear pixels, and the shared
`RenderMetrics` JSON record. The record identifies `gpu` /
`cuda-compute`, `status: "success"`, the actual CUDA device, and the requested common settings.
Invalid options, unsupported features, memory exhaustion, kernel errors,
capacity exhaustion, and encoding failures produce structured errors and a
nonzero exit code. Worker metrics use `unavailable` for missing CUDA or
unsupported backend features and `failure` for invalid inputs, execution,
capacity, or output errors. Only `success` represents a completed render.
An explicit GPU request never becomes a CPU render.

Failure metrics are written only after the original scene and all declared
mesh/texture paths are known to be separate from the output destinations.
File/directory symlink aliases and the `.gpu-pending` sidecars participate in
these checks. A rejected or unprovable metrics destination receives no file;
the worker still emits structured failure JSON to stderr and exits nonzero.
Material overrides do not remove the original textures from this protection.

## Supported boundary

* **FP32 geometry and radiance.** Ada runs FP64 at 1/64 the FP32 rate, so the
  FP64 path cost roughly 3.3x for no visible benefit; `"precision": "float64"`
  is now rejected rather than silently downgraded. Measured speedup, accuracy
  and the reasoning behind every tolerance are in
  [benchmarks/PHASE2-RESULTS.md](../benchmarks/PHASE2-RESULTS.md).
* Pinhole cameras; shared regular and seeded multi-jittered samplers with square
  sample counts. Multi-jittered sampling retains all 83 shared sample sets.
* Triangulated indexed PLY meshes, affine instances, smooth supplied normals,
  and UV-mapped baked RGB textures. Scene/material overrides are applied by
  `SceneFiles.applyMaterialVariant`, not a separate GPU scene variant.
  Textures match `BenchmarkRunner/SceneBuilder.fs`: nearest sampling with
  coordinates clamped to `[0,1]`, `v=0` at the bottom.
  Shared gamma2-decoded texels multiply the material's diffuse/ambient colors
  (and emission color); they do not discard the authored color tint.
* Matte/Lambert, classic Phong, mirror, normalized multi-sample glossy
  reflection, solid dielectric reflection/refraction, and front-face emission.
  Host preparation resolves optional material colors through
  `SceneFiles.ambientColour`, `specularColour`, and `reflectionColour`:
  null ambient defaults to the diffuse color, while null specular/reflection
  default to white. Explicit colors remain independent.
* Point, directional, rectangular area, and constant environment lights.
* Bounded per-light-sample visibility; straight-segment dielectric shadow
  transmittance, Fresnel/TIR, medium tracking, and distance-dependent colored
  absorption. This is classic transparent-shadow transport, not refractive
  shadow caustics.
* Scale-aware geometric-normal offsets and one-ULP outward rounding, normalized
  oriented shading normals, and the corrected CPU dominant-axis triangle edge
  tests.

Glass objects must declare closed, consistently oriented meshes in the common
scene specification. Assets must honor that contract. Open surfaces are not
silently treated as valid solid glass.
The neutral material contract uses exterior IOR 1 and a white exterior filter;
native CPU material overrides for exterior media are not encoded by this schema.

Shared scene validation requires a positive environment `Size[0]`, the legacy
sphere radius. CUDA accepts that compatibility value but ignores the radius:
its corrected constant environment has infinite visibility and background
radiance, not a finite sky sphere.

CSG, arbitrary implicit shapes, arbitrary F# texture functions, ambient
occlusion, thin-lens cameras, samplers other than regular/multi-jittered, missing
generated normals, image-varying dielectric filters, multi-jittered textured
glossy materials, and FP32 scene rendering remain outside this worker's validated boundary.
Use the explicit CPU worker for those features; this worker rejects them.

Multi-jittered tables follow the common CPU adapter's seeded construction order,
including its three validation samplers, first-used glossy materials, sampled
lights in declaration order, and the camera last. Each glossy material and each
rectangle/environment light retains its own table; unused materials consume no
samples. The textured-glossy restriction is explicit because the CPU texture
palette constructs a separate sampler for every distinct texel color. Replacing
those with one material-wide random table would change the image.

The adapter consumes `Acceleration.buildWith FlatBVH` and
`Acceleration.tryExportFlat`: the **shared flat, binned-SAH CPU BVH builder**
and its detached FP64 node/index snapshot. It does not maintain a separate
GPU-side parser or construction algorithm. Traversal is software CUDA compute,
not RTX/OptiX.
Triangles retain their source object-space coordinates. World-space bounds feed
the shared SAH builder, and flattened per-object inverse transforms are uploaded
alongside the geometry. Candidate triangles are intersected with unnormalized
local-space rays, preserving the world-ray parameter and avoiding precision loss
from baking a large instance scale into its vertices. Consecutive triangle
queries reuse the current object's transformed ray. Normals and transport remain
world-space. Repeated instances share input parsing but not device triangle
storage.

Exactly zero-area source triangles are nonintersecting in the shared CPU mesh.
GPU preparation omits those primitives and reports the expanded instance count
as `skippedDegenerateTriangles` in the successful worker response. Non-finite
normals, collapsed nondegenerate normals, and invalid inverse transforms remain
errors. This does not repair holes or turn an open scan into solid glass.

## Historical Gold Dragon bonus

`gold-dragon` is an optional, **authored/gold-only** extra. It is never added to
the four primary scene IDs or their six-material sweep, and the GPU worker
rejects any other material override for it. Parent-owned
`benchmarks/gold-dragon.json` records the original settings and provenance.
Prepare its optional data with the shared `scripts/prepare-gold-dragon.fsx`
command; the GPU project neither downloads nor modifies the scan.

The source is Stanford University's 437,645-vertex / 871,414-triangle Dragon,
SHA-256 `fea87ff48f2aba22fb53e7b67c3ff3f7b8c2a3b3a0653af62c48bba67c6d5744`.
The official `dragon_recon.tar.gz` archive is pinned by the parent acquisition
record to SHA-256
`74ac1d90989c9b1732edee82d57e9ce71452144cf4355f108d8c9c616d28d02f`.
It is flat shaded and open, with 108 zero-area triangles that the CPU treats as
nonintersecting. Stanford's attributed noncommercial research terms apply to
the optional scan and derived research renders; they are **not GPL-relicensed**.
See <https://graphics.stanford.edu/data/3Dscanrep/>.

The neutral bonus scene preserves the original gold material, camera, transform,
two point lights, blue ambient illumination, and constant environment. Its blue
floor is the documented finite 200000-by-200000 slab, not the truly infinite
plane retained by the separately restored original CPU function.
Gold uses lemon `[1,1,0.3]` diffuse, orange `[1,0.75,0.5]` ambient, and white
specular/reflection colors rather than tinting every component with diffuse.

```sh
/home/movj/.dotnet/dotnet RayTracer.Gpu/bin/Release/net10.0/RayTracer.Gpu.dll \
  --scene benchmarks/scenes/gold-dragon.json --material authored \
  --settings RayTracer.Gpu/Tests/gold-preview-settings.json \
  --output RayTracer.Gpu/validation/gold-dragon.preview.gpu.png \
  --linear RayTracer.Gpu/validation/gold-dragon.preview.gpu.pfm \
  --metrics RayTracer.Gpu/validation/gold-dragon.preview.gpu.metrics.json
```

This executed **96x72 preview** retains the original sixteen camera, light, and
glossy samples, 83 multi-jittered sets, and two bounces. All 20,736 retained RGB components matched the independently constructed CPU
worker exactly; the 24x18 fixture also matched exactly. **That result was
obtained while the device was FP64 and no longer holds:** the renderer is now
FP32, so CPU/GPU agreement is measured as RMSE with a bounded number of
silhouette pixels allowed to differ, not as exact equality. See
[benchmarks/PHASE2-RESULTS.md](../benchmarks/PHASE2-RESULTS.md). Reports are
`validation/gold-dragon.preview.comparison.json` and
`validation/gold-dragon.multi-jittered.comparison.json`.

The final pinned 96x72 CUDA run measured 70.14 s synchronized tracing, 79.93 s
total, and 257,366,028 explicitly allocated device bytes. The matching
22-thread CPU preview measured 73.95 s tracing and 79.05 s total. These
measurements do **not** establish an end-to-end GPU
speedup. The previews are not a completed original-resolution 1024x768 render;
the full profile keeps the same sampling and depth and must be scheduled separately
from the primary benchmark matrix.
The final image and metrics are
`validation/final/gold-dragon-authored.gold-preview.gpu.{png,pfm,metrics.json}`.

## Bounded execution and memory

All radiance transport and shadow queries run on-device. There are no
CPU/GPU per-ray round trips. A depth-first device work stack retains weighted
reflection/transmission/glossy children; each hit evaluates its own material
and adds emission and local direct lighting once.

Preflight derives the maximum pending-ray count as
`1 + MaxBounces * (maxBranching - 1)` and the geometric-series work bound.
It rejects requests requiring more than 4,096 pending rays or 1,048,576 total
ray-work items per camera sample. Runtime overflow is an error, never dropped
work. The explicit bounds are:

* At most 64 simultaneously tracked dielectric media.
* At most 2,048 surface crossings in one visibility/classification query.
* BVH depth at most 126, with a traversal stack sized to the prepared tree.
* Scene settings retain the common 0–32 bounce-depth range; a highly branching
  request can fail the stricter total-work preflight.

Immutable scene buffers upload once. Ray, medium, traversal, error, and output
buffers are reused for batches of at most 8,192 pixels. Batch sizing uses actual
free device memory, reserves 256 MiB, and limits the budget to 75% of total VRAM.
`Threads` and `TileSize` stay in the common settings record; CUDA execution uses
its own launch geometry and reported batch size.

## Timing and retained pixels

Metrics separately record load, host build, CUDA compilation/context setup,
allocation/upload, tracing, download, encoding, cleanup, and total wall time.
CUDA work is synchronized at each measured device phase. Trace measurements
are synchronized wall times including submission/first-launch overhead, not a
claim to exclude host launch costs. Compilation can benefit from existing
driver caches; no cache is purged from the user's environment.

`PeakDeviceBytes` reports peak explicitly allocated worker buffers, not driver
or compiler-private allocations. The worker also records process peak working
set, .NET allocations, and GC counts. Driver/system memory is checked during
batch preflight.

PNG uses the shared `Colour.ToDisplayColor` transfer (`gamma2`, `srgb`, or
`linear`). Linear output uses `BenchmarkRunner/LinearOutput.fs`, linked as the
same source into CPU, legacy and GPU workers. Packing the downloaded RGB
values and PFM validation/serialization are included in encoding time.
The writer validates dimensions, component count and finite float32 storage
conversion before opening its destination. Linear output is standard RGB PFM:

* Header `PF`, decimal `width height`, scale `-1.0`.
* Little-endian IEEE-754 float32 RGB triples, bottom row first.
* No clipping, display transfer, or tone mapping.
* The renderer computes in FP32, so PFM float32 output is now exact rather than
  a storage quantization. Host-side colour and the PFM writer remain FP64 types
  at the boundary; widening from the device is exact.

The in-memory analytical tests compare FP64 results before PFM quantization.
GPU links `BenchmarkRunner/LinearOutput.fs` and calls the same `writePfm`
implementation as the CPU/legacy workers. Downloaded FP64 RGB values are packed
for that host-only writer without display conversion; this packing is included
in encoding time. There is no dependency on the CPU worker assembly and no
separate GPU PFM-format implementation.

## Executed feasibility and numerical checks

```sh
/home/movj/.dotnet/dotnet RayTracer.Gpu/bin/Release/net10.0/RayTracer.Gpu.dll \
  --gate RayTracer.Gpu/validation/feasibility.json
/home/movj/.dotnet/dotnet RayTracer.Gpu/bin/Release/net10.0/RayTracer.Gpu.dll \
  --self-test RayTracer.Gpu/validation/classic-self-test.json
/home/movj/.dotnet/dotnet RayTracer.Gpu/bin/Release/net10.0/RayTracer.Gpu.dll \
  --edge-test RayTracer.Gpu/validation/shared-edge-self-test.json
/home/movj/.dotnet/dotnet RayTracer.Gpu/bin/Release/net10.0/RayTracer.Gpu.dll \
  --compare-core benchmarks/scenes/chair.json matte \
  RayTracer.Gpu/Tests/tiny-settings.json \
  RayTracer.Gpu/validation/chair-matte.core-comparison.json
```

The actual early gate on 2026-09-07 used:

| Component | Observed version/device |
|---|---|
| GPU | NVIDIA RTX 3000 Ada Generation Laptop GPU, 8,585,216,000 device bytes |
| CUDA compute capability | 8.9 |
| NVIDIA host driver | 596.47 |
| CUDA driver API | 13.2 |
| SDK / runtime | .NET SDK 10.0.400 / runtime 10.0.11 |
| F# compiler / language | 15.2.400.0 / F# 10.0 |
| FSharp.Core | 10.1.400 |
| ILGPU / ILGPU.Algorithms | 1.5.3 / 1.5.3 |

The first 4,096-case F# CUDA kernel checked nested value layouts (56-byte input,
32-byte output), triangle and parallel-axis AABB hits, FP64/FP32 arithmetic,
integer indexing, synchronization, and download. Maximum geometry error and
index mismatches were zero; maximum absolute FP64/FP32 errors were
`8.88e-16` / `4.77e-7`. That run measured compilation 3,043 ms,
allocation/upload 13.52 ms, first synchronized launch 23.38 ms, mean warm launch
0.0414 ms over 40 launches, and download 2.85 ms.

The full integration kernel also ran all six material families against
independent analytical expectations, including two direct lights, a four-child
glossy reflection, and Fresnel reflection/transmission. After matching the CPU
watertight geometry/normalization routines, maximum FP64 absolute RGB error was
`2.22e-16`.

Sixteen additional executed transport fixtures cover material changes on reflection,
absorption through a closed slab, a camera inside solid and nested colored glass, TIR, per-light
opaque visibility, colored transparent finite shadow segments, four-sample
rectangle visibility including the emitter endpoint, and constant environment
direct/background lighting, constant U/V clamped endpoints, and a large affine
floor that must not shadow itself.
The latter reproduced a `0.13125` RGB error with baked world-space triangle
intersections; object-space intersections reduced it to `1.11e-16` without an
arbitrary offset increase. GPU offsets now follow the current shared
`128 * epsilon * max(abs(point), abs(rayOrigin))` rule plus one-ULP outward
rounding. Finite shadow endpoints follow the CPU's scale-aware margin and then
move one ULP inward, with no unit-scale minimum: a near-light blocker in a
`1e-12`-scale scene must not disappear from the visibility segment.
Two oblique nested-glass cases independently check Fresnel transmission and
distance-dependent absorption along formerly failing shared diagonals.
`validation/classic-self-test.json` retains all twenty-two analytical
cases and their FP64 errors. An unsafe branching request was rejected before
launch, and a deliberate invalid-ray case must fail on-device.

All 24 shared scene/material combinations also rendered at 32×32, one camera
sample, and depth two, saving PNG, finite nonnegative PFM pixels, and valid
metrics; `validation/tiny-matrix.json` records the executed smoke matrix.
The chair/matte PNG was also decoded and viewed. These are **feasibility/smoke checks,
not standard/high benchmark results or a CPU/GPU speedup claim**. Full shared
CPU/GPU image comparisons and the standard/high matrix belong to the common
benchmark runner.

`--compare-core` is an explicitly named **test diagnostic**, never a fallback:
it renders through CUDA first, then compares FP64 values to the corrected CPU
`Render`/`ClassicIntegrator` using the exported world-space geometry and an
independent CPU BVH/intersector. It reconstructs the uploaded sampler tables and
currently accepts point/directional scenes only. Its report distinguishes this from comparing
the independently constructed common workers. Nearest-texel boundaries
are discontinuous: a one-ULP UV difference at a pixel boundary can fail this
diagnostic's strict whole-image threshold despite agreeing transport equations.
The report retains the exact worst pixels and UVs instead of hiding the difference.

The independent common CPU worker was also run for all 24 shared scene/material
pairs and an odd 31×29 case with four camera/glossy samples, using the same
settings and matched CPU host assemblies. Every retained PFM RGB component
agreed; the report is `validation/worker-comparisons.json`.
After switching to the shared binned-SAH export, all 25 were rechecked, along
with a fresh 97×89 fixture exercising an 8,192-pixel batch plus a partial final
batch. All 26 retained buffers still matched the independent CPU fixtures;
`validation/sah-worker-comparisons.json` includes binary fingerprints and the
declared thresholds. PFM equality is equality after the documented float32
storage conversion, not a claim of bitwise FP64 execution.

### Coordinated texture and material colors

The later comparisons pin both workers' assemblies so concurrent solution
builds cannot replace one reference during a run. Two coordinated changes
matter: UV interpolation preserves constant coordinates using
`a + beta*(b-a) + gamma*(c-a)`, and GPU texture addressing follows the current
shared worker's **nearest/clamp** policy rather than its earlier repeat policy.
The shared geometry owner made the CPU interpolation change; this optional
project does not modify CPU geometry or the shared scene adapter.

The addressing rule is shared by **modern CPU, legacy, and CUDA**, not a
GPU-only seam workaround. `benchmarks/legacy/Runner/LegacyRunner.fsproj` links
the same `BenchmarkRunner/SceneBuilder.fs` that the modern CPU worker compiles.
For finite UVs and a top-down image of width `W` and height `H`, all three use:

```text
clamp(t) = max(0, min(1, t))
x = min(W - 1, floor(clamp(u) * W))
y = min(H - 1, floor((1 - clamp(v)) * H))
```

Thus both `v=1` and `v=0.9999999999999999` select the top row of the 512-pixel
fabric texture; `u=1` selects the last column. Integer conversion after
clamping is floor, with no epsilon snapping or repeat wrapping. Gamma2
decoding and multiplication by the authored material tint also remain shared.
The frozen legacy geometry/interpolation is not rewritten by this adapter
policy. Any future addressing change must be coordinated across all three
engines rather than applied only in the GPU shader.

The earlier texture/material validation recorded **55 paired images** matching
exactly after PFM storage with their pinned binaries, without changing
the `1e-6` maximum / `1e-8` RMSE acceptance limits. This covers all 24 primary
scene/material pairs at both 32x32 and 64x64, the odd-resolution and partial-batch
cases, multi-jittered glossy/authored chair, the independent sampled-light/color
fixture, and both original-sampling Gold Dragon previews. The sampled-light
fixture covers reversed/first-used/reused/unused glossy materials,
rectangle/environment sampler tables, explicit ambient/specular/reflection
color overrides, null color defaults, and zero-area source triangles.
`validation/final-worker-comparisons.json` records 31 cases;
`validation/parity64-worker-comparisons.json` records the 24-case 64x64 gate.
Both retain the matching worker/core fingerprints.

These are targeted correctness gates, not the full standard/high performance
matrix. The retained RGB32 equality does not claim bitwise-identical FP64
execution. The shared benchmark runner owns the full-resolution matrix and
separate original-resolution Gold Dragon run.

Earlier failed comparisons remain as diagnostic history, including
`validation/pre-clamp-worker-comparisons.json`. They are not current texture
limitations or successful comparisons under relaxed thresholds. The GPU still
rejects multi-jittered textured glossy materials because their per-texel CPU
sampler allocation has not been implemented.

The previously flagged chair diagnostic had exactly one outlier: pixel `(17,15)`
on the woven-fabric cap, with CPU `v=0.9999999999999999` at a constant-`v=1`
repeat boundary. Its original report is preserved as
`validation/chair-authored.core-comparison.20260907-140828.failed.json`.
The canonical `validation/chair-authored.core-comparison.json` was rerun with
the same 32x32 settings and unchanged tolerances: maximum FP64 error is
`1.09e-14`, RMSE `3.94e-16`, with zero outlier pixels. It now records generation
time and worker/core/input fingerprints. Exact attribution is retained in
`validation/chair-authored-parity-resolution.json`; this same-process diagnostic
does not substitute for independently prepared worker comparisons.

### Nested dielectric shared-edge correction

The additional neutral `Tests/nested-glass-scene.json` exposed 26 black pixels
along a triangulated cube-face diagonal at 64x64. The secondary refracted ray
missed the outer boundary in both CUDA BVH and CUDA linear traversal, while
scalar intersections of the downloaded identical ray hit it. The resulting
miss applied infinite-distance absorption while still inside colored glass.
The failed image comparison is preserved in
`validation/neutral-worker-parity.nested-diagonal.failed.json`.

Actual CUDA edge arithmetic returned the same `-5.2319442657565975e-17` for
both orientations of one mathematically collinear edge. This violates the
antisymmetry required for watertight shared-edge classification. The GPU now
orders edge endpoints canonically before evaluating the determinant and
applying the orientation sign; coincident projected endpoints return zero.
No geometry epsilon, transport equation, tolerance, or CPU code was changed.
The explicitly invoked `--edge-test` diagnostic retains generated PTX and
checks 192 boundary queries plus 65 edge pairs, including coincident endpoints.
It runs from the repository root and is not part of the rendering path.

The current pinned workers independently reload all 24 primary scene/material
pairs at 64x64, the originally flagged authored chair at 32x32, the nested-glass
scene at 64x64, and the sampled-light/multi-jittered fixture. All **27 pairs**
match exactly over **311,568 retained RGB32 components**, at the unchanged
`1e-6` maximum / `1e-8` RMSE limits. Reports and input/binary fingerprints are
`validation/neutral-worker-parity.json`,
`validation/shared-edge-self-test.json`, and
`validation/nested-glass-parity-resolution.json`. This is a small correctness
gate, not a new full-resolution performance result.

### Primary clamp freeze refresh

`validation/primary-clamp-refresh.json` records a fresh **29-pair** comparison
using separately built, pinned current CPU/GPU workers and the shared PFM
writer. All **340,164 RGB32 components** agree exactly at the unchanged
`1e-6` maximum / `1e-8` RMSE limits. The required 24 primary scene/material pairs
use `Tests/primary-freeze-settings.json`: 64x64, four camera/light/glossy samples,
four bounces, and regular sampling. Additional cases retain the originally
flagged 32x32 chair, nested media, sampled lights with multi-jittered tables,
odd dimensions with a different seed, and a partial final GPU batch.
The pinned CPU reference already includes `RenderOptions.BvhOptions` with
scene-BVH leaf size 1/bin count 16. Shared/global defaults, including the GPU
host builder and mesh BLAS defaults, remain leaf size 4/bin count 16.
A subsequent current-core rebuild produced identical worker/core fingerprints;
reading both defaults from that pinned core confirmed the retune was already
included in these comparisons.

`validation/clamp-refresh-self-test.json` contains **24 analytical CUDA cases**,
including exact `u/v=1` and `nextDown(1)` coordinates. The one-ULP cases use
512-column/row textures and select the last column/top row with zero RGB error.
`validation/clamp-refresh-edge-test.json` retains all 192 boundary queries and
65 edge-pair cases. The refreshed canonical chair FP64 diagnostic still has
maximum error `1.09e-14`, RMSE `3.94e-16`, and no outliers.

These results cover the primary correctness boundary, not the parent's
512/1024 performance matrix. Gold is **not** a dependency of this gate.
Existing authored-only Gold support remains separate; the earlier Gold preview
reports are historical and do not establish a completed current
1024x768/original-sampling GPU run. Input/source/binary fingerprints and exact
output paths are retained in the refresh report; earlier reports remain intact.

```sh
/home/movj/.dotnet/dotnet RayTracer.Gpu/bin/Release/net10.0/RayTracer.Gpu.dll \
  --self-test RayTracer.Gpu/validation/clamp-refresh-self-test.json
/home/movj/.dotnet/dotnet RayTracer.Gpu/bin/Release/net10.0/RayTracer.Gpu.dll \
  --scene benchmarks/scenes/chair.json --material authored \
  --settings RayTracer.Gpu/Tests/primary-freeze-settings.json \
  --output RayTracer.Gpu/validation/freeze-chair.png \
  --linear RayTracer.Gpu/validation/freeze-chair.pfm \
  --metrics RayTracer.Gpu/validation/freeze-chair.metrics.json
```

### Exact common-orchestrator regression

`Tests/common-orchestrator-settings.json` retains the separate reported fixture:
64x64, camera 4/light 4/glossy 1, depth 4, 22 threads, tile size 16, seed 2026.
The actual shared orchestrator completed **48 successful CPU/GPU worker trials**
and **24 exact primary image comparisons** using these settings. All 294,912
retained RGB32 components agree, with no tolerance or texture-policy change.
Results are under `validation/common-orchestrator-current/`.

The cited failures in `artifacts/cpu-gpu-integration/differences.json` belong to
a run captured at **2026-09-07 14:22 UTC**, before the later adapter/shader
corrections. Its recorded source hashes differ from the current shared adapter,
GPU interpolation, geometry, transport, and host preparation. A stable source
fingerprint during that earlier run does not identify the current sources.
`validation/orchestrator-primary-resolution.json` records both run identities,
the exact settings, formerly failing pixel values, current worker fingerprints,
and the distinction between historical observations and current results.
The original failed files are untouched. No new near-integer texel snapping or
epsilon rule is justified by the current results, and none was introduced.

```sh
TMPDIR="$PWD/RayTracer.Gpu/.work" LD_LIBRARY_PATH=/usr/lib/wsl/lib \
  /home/movj/.dotnet/dotnet fsi scripts/benchmark.fsx -- \
  --presets quick --scenes all --materials all --engines cpu,gpu \
  --settings RayTracer.Gpu/Tests/common-orchestrator-settings.json \
  --repeats 1 --warmups 0 --skip-build --skip-prepare \
  --dotnet /home/movj/.dotnet/dotnet \
  --cpu-worker RayTracer.Gpu/validation/clamp-refresh-cpu/BenchmarkRunner.dll \
  --gpu-worker RayTracer.Gpu/validation/clamp-refresh-gpu/RayTracer.Gpu.dll \
  --output RayTracer.Gpu/validation/common-orchestrator-current
```

These commands use the existing rebuilt/pinned workers without resuming old
trials. The snapshots and output reports are local ignored artifacts; rebuild
and pin the workers first when reproducing from a clean checkout. This tiny
acceptance run is not the full 512/1024 performance matrix and does not run Gold.

Failure-contract probes also verified missing CUDA (`CUDA_VISIBLE_DEVICES=-1`),
unsupported float32 rendering, unsafe stack/work bounds, non-square sample
counts, unsupported thin-lens and textured multi-jittered glossy requests, the
gold authored-only policy, and output/input/asset alias rejection. All ten
returned nonzero status, structured errors, and no replacement
CPU images; see `validation/failure-contract.json`.

### Protected failure-output regression

The later early-failure probes exposed eight asset/link corruptions in twelve
cases against the previous worker, using only disposable GPU-owned inputs.
`validation/worker-guards.before.json` preserves that evidence. The worker now
checks the original specification before enabling failure metrics, then reuses
that parsed specification for preparation. Invalid settings, unsupported
precision, invalid output extensions, and asset-load failures cannot bypass
the protected-path preflight. Safe unrelated metrics destinations still
receive the shared failure record.

`validation/worker-guards.after.json` records all twelve passing cases, including
input hashes, symlink preservation, parsed structured errors, and absence of
success images. The GPU shadow endpoint already had no unit-sized epsilon
floor; its formula was not changed. A new receiver/light/blocker case at
`1e-16` scale gives exactly zero radiance and is included in the **25 analytical
CUDA cases** in `validation/protected-output-self-test.json`.

The resulting worker SHA-256 is
`dde1f621f5afdd52e18f4e670e1348509eb561adb33db83f67f57b118055cfac`.
`validation/guarded-worker-smoke.json` records successful real-worker PNG/PFM/
metrics output for authored and glass chair fixtures, both byte-identical in
PFM to the independent CPU reference. This host-only preparation/output fix
does not replace or expand the separately pinned 24-pair primary
common-orchestrator result above.

```sh
/home/movj/.dotnet/dotnet fsi RayTracer.Gpu/Tests/worker-guard-tests.fsx -- \
  RayTracer.Gpu/bin/Release/net10.0/RayTracer.Gpu.dll \
  RayTracer.Gpu/validation/worker-guards.after.json
LD_LIBRARY_PATH=/usr/lib/wsl/lib /home/movj/.dotnet/dotnet \
  RayTracer.Gpu/bin/Release/net10.0/RayTracer.Gpu.dll \
  --self-test RayTracer.Gpu/validation/protected-output-self-test.json
```

## F# kernel compiler constraints

Kernels use sequential value structs, `ArrayView` buffers, static F# functions,
and imperative loops. They do not contain reference objects, ordinary F#
tuples, recursive calls, closures, exceptions, or discriminated unions.

CUDA device discovery is performed once per process. ILGPU 1.5.3 installs a WSL
assembly DLL resolver during discovery and throws if discovery is repeated.
`CudaRuntime` retains only device descriptions and creates/disposes explicit
CUDA accelerators and contexts for subsequent renders; it does not retain
per-render GPU buffers or introduce a CPU accelerator.

The project intentionally sets `<Optimize>false</Optimize>` for the F# IL
compiler. F# 10's optimizer extracted a synthetic `$cont` helper taking
`FSharp.Core.Unit`; its `ldnull` argument was rejected by ILGPU. Disabling that
IL transformation made the full kernel compile and execute successfully.
The helper was compiler-generated from static F# code; an authored closure is
not required to encounter this limitation. This is a locally validated
workaround for the pinned toolchain, not an upstream-documented universal fix
or a claim that the defect is exclusive to .NET 10. The setting is isolated to
this optional GPU project and does not disable CPU or legacy optimization.
ILGPU's own O2 device compilation still applies; this is not a request for CPU
execution. With F# IL optimization disabled, even tuple destructuring can leave
reference `System.Tuple` locals, so kernel locals are bound separately.

No fast-math or float32 substitution is enabled. The gate and self-test must be
rerun when upgrading the SDK, F# compiler, ILGPU, GPU driver, or kernel layouts.

## Licenses

The repository retains GPLv2. Both pinned ILGPU packages use the permissive
University of Illinois/NCSA license, compatible with GPLv2 redistribution with
the required notices. The actual 1.5.3 NuGet archives were checked; both point to
repository commit `2efd942cf020da83c32278a6666d57c1d8505077`.
See `THIRD-PARTY-NOTICES.txt`. Shared host/image dependencies retain their
existing notices. No proprietary CUDA driver binaries are bundled.
