# Measured CPU tuning

These are targeted **same-corrected-renderer** experiments, not ratios between
the buggy historical transport and the corrected one. They ran locally on
2026-09-07 using the Intel Core Ultra 9 185H, with 22 visible logical processors.
The retained JSON includes the actual core/worker assembly fingerprints.

```sh
dotnet build BenchmarkRunner -c Release
dotnet fsi scripts/tune-cpu.fsx -- --output artifacts/cpu-tuning
dotnet fsi scripts/tune-cpu.fsx -- \
  --sweep bvh-settings --output artifacts/cpu-bvh-tuning
```

Both experiments used the authored chair and space-sentinel scenes at
192x192, four regular camera/light samples, one glossy sample and depth three.
Each scene was prepared once. Every configuration had one unmeasured in-process
warmup and three measured renders, with no forced garbage collection.
This excludes scene loading and image encoding; it is not the process-isolated
cold benchmark or a guaranteed speedup on another machine.

## Scene traversal

The initial comparison used four-primitive, 16-bin flat-BVH leaves and
brute-force **scene-level** queries. Mesh BLAS remained flat BVH in both cases.
All 54 measured films across nine acceleration/thread/tile configurations
matched the corresponding brute-reference FP64 film hash exactly.

| Scene | Brute median trace | Flat BVH median trace | Trace ratio |
|---|---:|---:|---:|
| Chair | 5581.56 ms | 1113.08 ms | 5.01x |
| Space sentinel | 6583.65 ms | 1276.72 ms | 5.16x |

The corrected tree BVH was slightly faster than the initial flat configuration
on these fixtures. Flat layout alone was therefore not treated as proof of an
improvement over every repaired accelerator.

## Retained refinement: smaller scene leaves

The next experiment varied scene leaf size and bin count while keeping the
same prepared geometry, integrator, samples and mesh BLAS. All 36 measured
films matched their brute-reference FP64 hashes exactly.

| Scene | Leaf 4 / bins 16 | Leaf 1 / bins 16 | Allocations, leaf 4 -> 1 |
|---|---:|---:|---:|
| Chair | 1064.05 ms | 685.50 ms | 2818.6 -> 1758.8 MiB |
| Space sentinel | 1170.34 ms | 795.05 ms | 3048.0 -> 2136.3 MiB |

One-primitive scene leaves reduced trace time by about 32-36% and managed
allocation by about 30-38% in this paired experiment. A scene primitive can
involve an inverse instance transform and a mesh traversal, so its cost is
quite different from a cheap packed bounding-box test.

The modern default now uses **scene leaf size 1, 16 bins**. Mesh BLAS and the
GPU's world-triangle builder keep their existing leaf-size-4 defaults; this
experiment does not establish a benefit from changing those different layers.
`RenderOptions.BvhOptions` exposes leaf/bin/depth settings for focused library
experiments, and worker timing notes record the actual scene and mesh settings.

Eight versus sixteen versus all visible CPU workers, and tile sizes 8/16/32,
did not yield a consistent winner across both fixtures. Defaults therefore
remain the runtime processor count and 16-pixel tiles. Thirty-two bins helped
the chair but regressed the sentinel relative to sixteen bins; sixteen remains
the conservative shared choice.

Do not multiply these ratios into a claimed end-to-end legacy/GPU speedup.
The full before/after report measures a different boundary, including runtime
startup, loading, encoding and device costs, and also discloses the intentional
lighting and geometry corrections.
