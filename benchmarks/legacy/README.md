# `legacy-port` baseline

The renderer in `original/` is the frozen commit
`066c59eba68d222d63b373b7e25d408796eb1d12`, including its original GPLv2 license.
**Never edit `original/` or replace this baseline with modern renderer code.**
This measures original algorithms on the pinned modern .NET runtime; it is not
a reproduction of the historical Windows/.NET Framework timing.

```sh
dotnet restore benchmarks/legacy/LegacyRayTracer.fsproj --locked-mode
dotnet build benchmarks/legacy/LegacyRayTracer.fsproj -c Release --no-restore
dotnet fsi benchmarks/legacy/proof.fsx
dotnet fsi benchmarks/legacy/prepare-port.fsx -- --check
```

The project links the authoritative original compile inputs in their original
order. Inactive root `API.fs`, `Program2.fs`, and `TriangleMeshes/*` stay inactive.
It references only FParsec 1.1.1 and the shared managed image codec project,
**not** the modern renderer, scene math, acceleration, or sampling.

## Auditable adaptations

`prepare-port.fsx` verifies every `source.sha256` entry, generates replacement
files from frozen text using explicitly listed transformations, and writes
`portability.diff` plus `portability.json`. The JSON records exact source/adapted
SHA256s and the compile list. The build runs `--check` and fails on changed
originals, unexplained replacement edits, wrong compile order, or stale audit
files. Regenerate only after reviewing a necessary portability adjustment:

```sh
dotnet fsi benchmarks/legacy/prepare-port.fsx
git diff -- benchmarks/legacy/prepare-port.fsx benchmarks/legacy/portability.diff
```

| Original file | Permitted replacement |
| --- | --- |
| `Sampling/Sampling.fsi` | Byte-identical copy beside its implementation, required for F# signature pairing. |
| `Sampling/Sampling.fs` | Only visualizer bitmap ownership/output and filename type annotations. All RNG, thread indexing, shuffles, samples, mappings, and dispatch remain original. |
| `Core/Vector.fs` | Explicit `Vector` return type on `DivideByInt` resolves current compiler inference. Its expression is unchanged. |
| `Core/Foundation.fs` | Remove unused `System.Numerics` import so its newly available `Vector` cannot shadow the tracer's own type. |
| `Transformation/Transformation.fsi` | Expose the original immutable `QuickMatrix` fields and existing `mkTransformation` constructor for shared affine16/inverse input. Signature visibility only; no math changes. |
| `Transformation/Transformation.fs` | Byte-identical copy beside the adapted signature, required for F# signature pairing. |
| `Acceleration/RegularGrids.fs` | Whitespace around two comparisons prevents interpretation as generic arguments. |
| `Render.fs` | Replace Windows bitmap/preview I/O with RGB8 PNG output; optional exact pre-conversion pixel capture and coarse phase clocks. No tracing, shading, acceleration, or sampling repair. |

The original per-pixel `Parallel.For`, camera ray construction, array mapping,
averaging, `Colour.ToColor` gamma-2 approximation, and final vertical flip are
retained. Image channels are stored as RGB rather than Windows BGRA. Preview
throws a clear `PlatformNotSupportedException`. `Clean` still disposes the image,
clears `Acceleration.listOfAccel`, and forces `GC.Collect()`.

## Library/output contract

Assembly: `LegacyRayTracer.dll`; API namespace: `Tracer.API`.
Run it in a separate process from the modern assembly.

- `Render.RenderParallel` returns a caller-owned `Tracer.Imaging.RgbImage`.
- `Render.RenderWithLinear` returns `(image, Colour[])`. The array stores exact
  original averaged colors before `ToColor`, **top-left row-major**, matching the
  vertically flipped PNG. Its optional per-pixel capture is output instrumentation,
  not a rendering algorithm change; record it in measurements.
- `Render.RenderCaptured` returns `LegacyRenderFilm`, with `Width`, `Height`,
  top-left packed RGB `LinearRgb: float[]`, `Image: RgbImage`, `BuildMs`,
  `TraceMs`, `OutputPreparationMs`, and `PhaseNotes: string[]`.
  `BuildMs` times the original single `PreProcessing` call. `TraceMs` includes
  the original per-pixel gamma/RGB8 conversion and optional raw capture as well
  as tracing; do **not** label it pure ray-tracing time. Output preparation
  covers buffer setup, the original final Y flip, and float64 packing.
  Renderer construction/bound partitioning, file encoding, and cleanup are
  outside these fields; the caller must time them separately and retain an
  overall wall-clock duration. No per-pixel timers or extra preprocessing calls
  are introduced.
- Encode/copy outputs before `Render.Clean(image)`. That method retains the
  baseline's global cleanup/GC behavior.
- Scene assets should be passed as resolved paths by the neutral scene adapter.
  The baseline does not silently rewrite missing paths or repair missing assets.
- `Tracer.Basics.Transformation.QuickMatrix` exposes the original sixteen
  `Pos1x1` through `Pos4x4` float fields. The existing
  `mkTransformation : QuickMatrix * QuickMatrix -> Transformation` stores a
  caller-supplied forward/inverse pair unchanged, allowing the same exact
  affine16 input and shared host inverse in both workers without reflection.
  Original inversion/shear/normal routines are not repaired or replaced.

`proof.fsx` renders one tiny diagnostic sphere and validates an analytic hit,
original gamma, exact linear-to-PNG orientation, PNG round-trip, coarse timing
fields, typed affine16 constructor visibility, compatibility with
`RenderWithLinear`, and cleanup.
Artifacts go to `artifacts/foundation-validation/legacy-proof/` by default.
This is only a portability probe, **not** a second scene suite or a benchmark.
