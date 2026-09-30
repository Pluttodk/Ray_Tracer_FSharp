# Reproducible classic-renderer comparisons

The two CPU-side workers are **separate executables**:

| Requested engine | Project | Report label |
| --- | --- | --- |
| `legacy` | `benchmarks/legacy/Runner/LegacyRunner.fsproj` | `legacy-port` |
| `cpu` | `BenchmarkRunner/BenchmarkRunner.fsproj` | `cpu` |
| `gpu` | `RayTracer.Gpu/RayTracer.Gpu.fsproj` | `gpu`, or explicit failure/unavailability |

The legacy and modern workers link the **same**
`BenchmarkRunner/ScenePolicy.fs`, `SceneBuilder.fs`, `Worker.fs`, and `Program.fs`. They never
load both versions of the original `Tracer` namespaces into one process.
The frozen source and its audited portability changes are described in
[legacy/README.md](legacy/README.md). No benchmark worker repairs the legacy
renderer to force agreement.

## Run the requested matrix

Use the SDK pinned by `global.json`, from the repository root. On the
development machine its host is `/home/movj/.dotnet/dotnet`; add
`/home/movj/.dotnet` to `PATH` if necessary.

```sh
dotnet fsi scripts/benchmark.fsx -- \
  --presets standard,high --scenes all --materials all \
  --engines legacy,cpu,gpu --output artifacts/benchmarks
```

This command prepares/validates the original procedural assets offline,
verifies the frozen legacy source checksums, and builds the participating
workers in Release **before measuring**. It then runs workers **serially**:

- Four scenes: `chair`, `roman-bust`, `space-sentinel`, `sky-arena`.
- Six subject variants: `matte`, `phong`, `mirror`, `glossy`, `glass`, `authored`.
- Two required presets: `standard` and `high`.
- **48 cases per engine; five measured repeats per case**: 240 measured
  processes per engine. The default also runs one separate warmup per case.
- An explicitly requested unavailable GPU remains unavailable. There is no
  CPU fallback, replacement image, or silently omitted case.

Scripts and tiny diagnostic renders are not evidence that this complete
matrix has run. Inspect `run-manifest.json`, `summary.json`, the expected
repeat counts, and all failures before describing a workload as complete.

### Presets and targeted checks

The authoritative values are `SceneFiles.preset` in `SceneFormat/SceneFormat.fs`.
Sample counts are **actual samples**, not sampler constructor dimensions.

| Preset | Image | Camera samples | Light samples | Glossy samples | Bounces | Measured repeats |
| --- | --- | --- | --- | --- | --- | --- |
| quick | 256 × 256 | 4 | 1 | 1 | 2 | 1 |
| standard | 512 × 512 | 16 | 4 | 1 | 3 | 5 |
| high | 1024 × 1024 | 64 | 4 | 1 | 4 | 5 |

All defaults use seed 2026, `regular`, `float64`, `gamma2`, 16-pixel tiles,
and the runtime's processor count. Standard/high are fixed-work comparisons;
quick is only a smoke preset.

### Calibrated full-image collection

The initial settings above are deliberately retained as the exhaustive defaults.
The local workload pilot rendered all 24 scene/material combinations through
both CPU-side engines at 96x96, four camera/light samples and depth four.
Legacy tracing totaled about 141 seconds for those 24 small images. Simple
pixel/sample scaling puts the original five-repeat high preset on the order of
days, before allowing for thermal/load variation. The pilot ran while source
integration continued, so its report correctly marks changed inputs: use it
for workload planning, **not as frozen speedup evidence**.

The practical full-image collection preserves the standard 512x512 and high
1024x1024 sizes, their respective bounce limits, all four scenes and all six
materials. It uses four camera samples and one cold measured observation per
case, identically for every engine:

```sh
dotnet fsi scripts/benchmark.fsx -- \
  --presets standard,high --scenes all --materials all \
  --engines legacy,cpu,gpu --camera-samples 4 --repeats 1 --warmups 0 \
  --output artifacts/benchmarks
```

These are still 48 cases per engine, not omitted or substituted cases.
The manifest records every override. One observation does not establish
per-case variance or a statistically robust speedup; the report's count-one
distributions must not be presented otherwise. Use extra repeats for focused
timing/legacy-variability experiments, or remove the overrides for the much
larger original sampling/repetition workload. The native Gold Dragon retains
its original 16 camera/light/glossy samples independently of this calibration.

```sh
dotnet fsi scripts/benchmark.fsx -- \
  --presets quick --scenes chair --materials matte --engines legacy,cpu \
  --width 32 --height 32 --camera-samples 1 --light-samples 1 \
  --threads 1 --warmups 0 --timeout-seconds 120 \
  --output artifacts/benchmark-smoke
```

Use `--scene path/to/scene.json` for one neutral diagnostic scene instead of
`--scenes`; custom scenes skip procedural generation unless
`--prepare-assets` is explicitly supplied. `--scene-dir` chooses existing
scene descriptions; the standard procedural generator still writes its
documented repository-root locations.

`--settings file.json` supplies a base `RenderSettings`. Individual overrides
include image dimensions, camera/light/glossy samples, bounces, threads, tile
size, seed, precision, sampler and transfer. `--repeats` and `--warmups` are
explicit experimental-design overrides, recorded in each case.
`--acceleration bvh|kdtree|grid|brute` selects the modern CPU **scene-level**
structure; `bvh` is the flat binned-SAH implementation. Mesh BLAS policy
remains the corrected mesh renderer's FlatBVH.

Run `dotnet fsi scripts/benchmark.fsx -- --help` for all switches.
`--skip-build` and explicit `--cpu-worker`, `--legacy-worker`, or `--gpu-worker`
assembly paths are advanced integration/testing controls: binary hashes are
retained, but that invocation cannot establish source-to-binary build
correspondence. Do not use them to hide a stale build.

### Optional Gold Dragon bonus

`gold-dragon` is an **extra authored/gold-only case**. It is deliberately
excluded from `--scenes all`, which remains exactly four IDs and 48
standard/high scene-material cases per engine. The Stanford reconstruction
has holes: requesting another material or the whole material sweep for this
bonus fails explicitly rather than treating the scan as solid glass.

```sh
dotnet fsi scripts/prepare-gold-dragon.fsx
dotnet fsi SceneAssets/GoldDragon.fsx
dotnet fsi scripts/benchmark.fsx -- \
  --scenes gold-dragon --materials authored --engines legacy,cpu \
  --presets quick --settings artifacts/scene-assets/gold-dragon/original-settings.json \
  --repeats 1 --warmups 0 --output artifacts/gold-dragon-benchmark
```

The prepared neutral scene belongs at
`benchmarks/scenes/gold-dragon.json`, or `gold-dragon.json` inside an explicit
`--scene-dir`. **`benchmarks/gold-dragon.json` is provenance, not a
`SceneSpec`**. The preparation commands supply the verified Stanford asset,
neutral scene, and original `RenderSettings` JSON before the benchmark;
the benchmark itself never downloads the bonus asset. A bonus-only
selection does not run the unrelated procedural scene generator.

Reproducing the original example requires 1024x768, two bounces,
`multi-jittered`, and **16 actual camera, light and glossy samples** (the
original constructor argument was 4, with 83 sets). Its authored glossy
gold, two point lights, environment emitter and blue reflective plane must
remain in the prepared scene. The scene selector does not silently change
presets or settings: the explicit original settings file overrides the
`quick` settings in the example above, and every resulting value is recorded.
The single-repeat example is an image reproduction, not a variance estimate.
It is a substantial workload, particularly on legacy-port. For a bounded
before/after preview, add `--width 256 --height 192 --camera-samples 4
--light-samples 4 --glossy-samples 1 --sampler regular`; these identical
overrides are recorded and do not reproduce the original sampling workload.
The neutral scene uses a separately generated finite 200000x200000 staging
slab, not the native example's truly infinite plane. The original direct
render remains available through `TracerTest --gold-dragon`.

The authored colors are independent: diffuse lemon `[1,1,0.3]`, ambient
orange `[1,0.75,0.5]`, and white specular/reflection colors. Unspecialized
`MaterialSpec` records use null optional color arrays; the shared helpers
default ambient to diffuse and specular/reflection to white.

Credit **Stanford University Computer Graphics Laboratory** under the
[official dataset terms](https://graphics.stanford.edu/data/3Dscanrep/).
The optional dataset allows attributed noncommercial research/free
redistribution; commercial use requires permission. Keep the downloaded
dataset and derived images in ignored artifacts, and do not relicense them
under the renderer's GPL. The verified original `dragon_recon.tar.gz` archive
SHA-256 is
`74ac1d90989c9b1732edee82d57e9ce71452144cf4355f108d8c9c616d28d02f`;
this is distinct from the reconstructed PLY file hash recorded in the
bonus provenance.

Bonus provenance is included in case fingerprints. An unsupported GPU
environment/sampler/geometry combination remains a failure or unavailable
case; no CPU fallback or substitute image is produced.

## Worker contract

```sh
dotnet BenchmarkRunner/bin/Release/net10.0/BenchmarkRunner.dll \
  --scene benchmarks/scenes/chair.json --material authored \
  --settings artifacts/settings.json --output artifacts/chair.png \
  --linear artifacts/chair.pfm --metrics artifacts/chair.metrics.json \
  --acceleration bvh
```

The required options are exactly `--scene`, `--material`, `--settings`,
`--output`, `--linear`, and `--metrics`. `--acceleration` belongs to modern
CPU only. `--help` prints the contract. Duplicate, unknown, missing and
unsupported options fail explicitly.

Success requires a valid PNG, a retained linear RGB PFM, and schema-conforming
`RenderMetrics`, with correct scene/material/settings and zero invalid
pixels. A failure emits structured metrics only after the metrics and
metadata destinations have been checked against every input; malformed
arguments, unreadable scenes and unsafe destinations report exclusively to
stderr rather than risk overwriting an input. Every failure has a nonzero exit.
The CPU-side workers also
write `<metrics>.worker.json` containing capabilities, limitations, completed
phase names and timing-boundary notes.

Both CPU-side workers currently reject `float32` instead of pretending to
honor it. Modern output supports `gamma2`, `srgb`, and `linear`; legacy
rejects anything except its original `gamma2`. Both `regular` and
`multi-jittered` require square sample counts: a requested 16 samples calls
the original constructor with 4, not 16. Impossible counts fail; they are not
rounded. Multi-jittered uses 83 sample sets.

For a direct legacy invocation, set `DOTNET_PROCESSOR_COUNT` to the requested
`settings.Threads` before launching:

```sh
DOTNET_PROCESSOR_COUNT=1 dotnet \
  benchmarks/legacy/Runner/bin/Release/net10.0/LegacyRunner.dll \
  --scene benchmarks/scenes/chair.json --material matte \
  --settings artifacts/one-thread-settings.json --output artifacts/legacy.png \
  --linear artifacts/legacy.pfm --metrics artifacts/legacy.metrics.json
```

The orchestrator sets this for every worker. Legacy retains its original
`Parallel.For` scheduling: the setting is not a hard count of OS threads.
Its ineffective full RNG seeding, schedule-dependent sampling, shared
triangle barycentric races, old transport and forced cleanup GC remain
explicit limitations. Even regular sampling cannot repair the triangle race.
The original sampler also checks its thread-index array capacity before
taking a mutex, then computes its growth count after locking. Concurrent
growth can make that count negative and abort a legacy worker. Such failures
remain visible; the baseline is not patched or silently rerun on another
engine. An explicit resume may retry failed cases with unchanged settings
while preserving their failed-attempt records.
The original PLY triangle shader also swaps its interpolated U/V values;
that defect remains in legacy-port, so textured before/after images can differ
in orientation even though both adapters receive the same pixels and lookup rule.

### Observed legacy image variability

The retained `artifacts/legacy-variability/` diagnostic uses three independent
processes per engine, the authored chair at 192x192, four regular camera/light
samples, one glossy sample, depth three, seed 2026 and 22 requested workers.
Reproduce it with:

```sh
dotnet fsi scripts/benchmark.fsx -- \
  --presets quick --scenes chair --materials authored --engines legacy,cpu \
  --width 192 --height 192 --camera-samples 4 --light-samples 4 \
  --glossy-samples 1 --max-bounces 3 --threads 22 --seed 2026 \
  --repeats 3 --warmups 0 --output artifacts/legacy-variability
```

All three modern PFM files were byte-identical. All three legacy PFM hashes
were different: compared with the predetermined first repeat, repeats two
and three changed 934 and 956 of 36864 pixels, with maximum linear component
differences of 0.00444 and 0.00657. Regular samples and a fixed seed therefore
do not establish legacy image determinism. This is a bounded image-stability
diagnostic, not a substitute for the standard/high collection or a claim
about every legacy scene. Every repeat is retained; none was cherry-picked.

## Shared geometric and material input

- Objects are PLY mesh instances made with `mkPLY`, `mkShape`, and
  `API.transform`. One mesh/material shape is shared by repeated instances.
  Each new `mkPLY`/`mkShape` pair finishes before loading another mesh because
  the frozen API assigns its global mesh ID during load.
- Row-major affine matrices use the declared forward matrix and an actual
  double-precision affine inverse, including shear and reflected/nonuniform
  transforms. The historical `.fsi` hides its constructors; cached
  FSharp.Core reflection constructs the **exact original** `QuickMatrix`
  record and `T(forward,inverse)` representation. This is the original
  `mkTransformation` representation, not a replacement transformation
  implementation. Identical code is used in both workers.
- Ambient/diffuse/specular/reflection coefficients, independent colors,
  exponents, emission, filter and IOR map to the corresponding original
  material-family constructors. Mirror and glossy retain their Phong
  highlights. Glass uses the declared inner filter and IOR with white/IOR 1
  exterior; it does not substitute a diffuse approximation.
- Texture pixels are decoded by `Tracer.Imaging.RgbImage`, copied once,
  and converted using the original `fromColor` **gamma-2 decode**. Nearest,
  clamped UV lookup uses bottom-left UVs and top-down pixel storage.
  Coordinates are clamped to [0,1], so v=1 selects the top row rather than
  wrapping to the bottom after a one-ULP arithmetic difference.
  Texture color multiplies ambient/diffuse color, emissive color or glass
  filter; it does not discard the declared tint. Specular/reflection colors
  retain their declared values. Material lookup uses an immutable pixel
  palette after preparation, without per-hit image locks.
- `SceneFiles.applyMaterialVariant` changes only subject IDs. Floors,
  staging, lights, camera and non-subject materials remain authored.
  Open meshes requested as solid glass fail explicitly, including authored
  glass materials.
- Directional `Direction` points **from the shading point toward the source**.
  Rectangle `Direction` is its outward emitting normal; `Position` is its
  center and `Size` is width/height. A matching orthonormal frame transforms
  the canonical original rectangle and its emissive material.
- The four-scene shared suite has a black background. Optional environment
  bonuses use the same `mkEnvironmentLight` input in both workers, with the
  positive historical sphere radius in `LightSpec.Size[0]`. Legacy retains its
  enclosing-emitter geometry and old sampling; modern CPU/CUDA use corrected
  environment sampling and miss radiance. These are explicitly
  correctness-changed comparisons, not matching background implementations.

## Timings, statistics and artifacts

**Cold wall time** is measured around the complete isolated process,
including startup/JIT, load, acceleration, GPU compilation/transfers/
synchronization, tracing, output encoding, metrics, cleanup and exit.
GPU trace/kernel-only latency is never labeled end-to-end speedup.

Worker phases are additional diagnostics. Public API boundaries are coarse:
`mkPLY` may construct mesh acceleration inside “load”. The legacy worker uses
`RenderCaptured`: build includes the original single `PreProcessing` call,
plus renderer construction and shared-adapter preparation. Trace retains the
original per-pixel display conversion and optional raw capture alongside
tracing. Buffer setup, the original final vertical flip and float64 packing
are output preparation, included in encode by the worker.

Earlier retained legacy workers included scene-tree construction in their
coarse trace timing. Consult each worker's recorded phase notes; do not mix
those subdivisions with the newer boundaries. Legacy and modern phase
subdivisions are still **not** directly matched algorithm measurements.
Worker totals exclude final metrics serialization; cold wall time includes it.

Every repeat is a new process. Warmups can warm OS/driver caches, but cannot
remove JIT/compilation from a different measured process. On partial resume,
the warmups for a case are rerun before new measurements; a fully completed,
verified case does not need to execute again.

The summary retains counts, medians, quartiles/IQR, min/max and median
absolute deviation. No slow successful observation is trimmed. Failed,
unavailable, cancelled, incomplete and timed-out trials remain rows, never
fabricated zero-valued timings. Missing optional phases/memory measurements
are null/unavailable. Managed allocated bytes, host peak working set, and
GPU peak device bytes are distinct metrics.

Artifacts beneath the selected output directory include:

```text
index.html                         local gallery, tables, failures and links
run-manifest.json                  latest requested matrix and metadata
trials.json / trials.csv            every trial, including separate warmups
summary.json / summary.csv          measured-only statistics and counts
differences.json / diffs/*.png      unclamped linear errors + display previews
contexts/<context-hash>/metadata.json
runs/<invocation-id>.json           retained provenance for earlier runs
preparation/<invocation-id>/        build/generation stdout, stderr and results
<preset>/<scene>/<material>/<engine>/<full-case-fingerprint>/
  repeat-001/attempt-001/           PNG, PFM, settings, metrics, stdout, stderr
  repeat-001/trial.json             atomic pointer to the latest attempt
  warmup-001/...
```

The self-contained HTML uses embedded CSS and relative images/data, with no
external scripts, chart packages or uploads. It links **every** retained
repeat. Gallery images use the lowest successful repeat, not a visually or
temporally cherry-picked result.

PFM files are `PF`, little-endian RGB float32 with scale `-1.0`, bottom-up on
disk, unclamped linear HDR values. Rendering precision is recorded
separately; writing a float64 render to PFM introduces float32 storage
rounding. PNG is a display derivative, not the source for error statistics.
All three worker projects link the same `BenchmarkRunner/LinearOutput.fs`
source. Its `LinearOutput.writePfm path width height topDownRgb` accepts packed
top-down float64 values, rejects invalid dimensions/counts and values that
cannot convert to finite float32 before opening the output, and applies no
display transfer. Modern CPU and GPU PNG conversion reuse the core
`Colour.ToDisplayColor`; legacy keeps the original image returned by its
capture hook.
The report computes linear MAE, RMSE, max absolute error and relative RMSE;
relative RMSE is unavailable for nonzero error over an all-black reference.
Difference previews apply `sqrt(4 * abs(error))` and display clipping only.
Legacy bug-fix differences are diagnostic, not an arbitrary global accuracy
gate.

## Resume, cancellation and failures

Append `--resume` to the **same command**. Reuse requires:

- The identical full fingerprint: working-tree source contents (including
  dirty and nonignored untracked source), git SHA, SDK/runtime, dependency
  assembly hashes, device/environment information, scene/assets, full
  settings, worker, acceleration, repeats/warmups and timeout.
- Successful matching metrics, correct PNG/PFM dimensions/contents, and
  intact recorded artifact SHA-256 hashes.

Timestamps and `--resume` itself are not render inputs. Changing code,
inputs, binaries or settings creates a different case directory. Failed
attempts are retried in a **new** attempt directory; existing images and raw
buffers are not overwritten. The script checks the frozen input/source/
dependency hashes again at completion; concurrent changes make the run
incomplete rather than silently mislabeling its provenance. Finish edits
before performance measurements.

The default worker deadline is 14,400 seconds; preparation/build commands
default to 1,200 seconds. `--timeout-seconds` changes the controlled process
deadline, not a renderer sampling limit. Timeout, Ctrl+C or SIGTERM kills
only the current owned process tree, waits for exit, and retains completed
results. An unavailable GPU is a visible incomplete requested matrix, not a
reason to copy CPU output.

Failures return nonzero. Early preparation/configuration failures also leave
`last-invocation-error.json` when an output directory is available. Later
failures retain trial rows, logs and the final report. Rebuild a local report
without starting workers using:

```sh
dotnet fsi scripts/benchmark.fsx -- --report-only --output artifacts/benchmarks
```

Report-only mode checks retained artifact hashes and labels corrupted or
missing outputs incomplete.

## Targeted validation

No additional test framework is used:

```sh
dotnet build BenchmarkRunner/BenchmarkRunner.fsproj -c Release
dotnet build benchmarks/legacy/Runner/LegacyRunner.fsproj -c Release
dotnet fsi scripts/benchmark-tests.fsx
dotnet fsi scripts/benchmark-selection-tests.fsx
dotnet fsi scripts/benchmark-builder-tests.fsx
dotnet fsi --define:LEGACY scripts/benchmark-builder-tests.fsx
dotnet fsi scripts/benchmark-worker-tests.fsx
```

The selection tests drive the actual CLI with explicitly unavailable workers
and empty routing fixtures: they assert the 48-case primary matrix, authored-only
bonus routing, no bonus downloads/generation, and explicit failures without
producing replacement images. The pure contract tests cover linear/PNG integrity, nullable statistics,
HTML/CSV/JSON, file hashing, exact process timeout and cancellation. The linked
builder tests check affine inverse/row-major input, rectangle frames and
actual sample counts against each isolated assembly. Worker integration
tests create a tiny original textured tetrahedron/floor fixture, render all
six variants on both workers, check unsupported settings/open-glass errors,
modern thread/tile determinism and scene acceleration alternatives, and
exercise actual orchestration/resume/corrupted-output recovery. Diagnostic
images remain under `artifacts/benchmark-worker-checks/`; they are not the
required presentation matrix. `--skip-orchestrator` limits that last test
script to workers when the rest of the working tree is actively changing.
