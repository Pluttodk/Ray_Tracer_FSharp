# Phase 2 measured results: FP32 CUDA renderer

Measured 2026-09-10 on the NVIDIA RTX 3000 Ada Laptop GPU (SM 8.9, 8 GB),
driver 596.47, under WSL2. Four benchmark scenes at 256x256, 4 camera samples,
4 light samples, depth 4, regular sampler.

The FP64 reference renders were captured *before* the conversion and retained
under `artifacts/fp64-reference/`; the FP32 renders are in `artifacts/fp32-gpu/`.

## Speedup

Two changes, measured separately. All three renders are bit-identical between
the FP32 columns, so the occupancy change is pure scheduling.

| Scene | FP64 | FP32 | FP32 + occupancy | Total |
|---|---:|---:|---:|---:|
| chair | 173.1 ms | 49.6 ms | 34.3 ms | **5.05x** |
| roman-bust | 134.3 ms | 42.0 ms | 26.0 ms | **5.17x** |
| space-sentinel | 270.9 ms | 83.6 ms | 41.0 ms | **6.61x** |
| sky-arena | 167.1 ms | 51.6 ms | 33.4 ms | **5.00x** |
| **Total** | **745.5 ms** | **226.9 ms** | **134.7 ms** | **5.53x** |

- FP32 alone: **3.29x**, and 35% less device memory.
- Removing the occupancy cap: a further **1.68x**.

### The occupancy cap

Batch size was capped at a flat 8,192 lanes - about 1.8 threads per CUDA core
on this GPU, far too few to hide memory latency in a traversal-bound kernel.
That cap was never the real constraint: the batch is *already* bounded by actual
free device memory. Removing it and letting memory decide gives 1.68x for a
one-line change, and FP32 compounds with it by halving the per-lane state so
roughly twice as many lanes fit in the same budget.

## The plan predicted 10-30x. It is 3.3x. Why.

Ada really does run FP64 at 1/64 the FP32 rate, and that figure is not wrong -
but it only converts into end-to-end speedup for a kernel whose time is
dominated by FP64 *arithmetic*. This one is not:

- BVH traversal is pointer-chasing through global memory.
- Per-lane ray, medium and traversal stacks are strided `lane * capacity`, so
  every push and pop is a 32-way uncoalesced scatter.
- The megakernel runs all camera samples in one thread, so a warp advances at
  the speed of its worst lane.
- Occupancy is capped at 8,192 threads, roughly 1.8 per CUDA core.

Against that, halving every value cuts arithmetic cost *and* bandwidth - the
35% drop in device memory is the visible half of the same effect - but it cannot
remove a stall that was never arithmetic in the first place.

**This predicted the occupancy result.** The cost that FP32 could not remove was
scheduling and memory, so attacking it directly gave another 1.68x - more than
half again what FP32 itself delivered, for one line. The uncoalesced lane-strided
stacks and the hot/cold triangle split remain unexploited and target the same
bottleneck.

## Accuracy: FP32 is faithful, and max-error is the wrong way to check

Per-pixel error against the FP64 reference, 65,536 pixels per scene:

| Scene | median | p99 | > 1e-2 | RMSE |
|---|---:|---:|---:|---:|
| chair | 1.5e-8 | 2.9e-6 | 14 (0.021%) | 3.5e-4 |
| sky-arena | 0.0 | 1.1e-6 | 15 (0.023%) | 3.8e-4 |

Half of sky-arena's pixels are bit-identical and 99% of both scenes agree to
about 1e-6. But the maximum absolute error is 5.4e-2, and that number is not a
defect: those ~15 pixels are silhouettes, where a ray passing within one FP32
ULP of a triangle edge lands on different geometry and returns a different
colour. No max-error bound can separate that from a genuine fault.

The CPU/GPU parity gate was therefore rebuilt around **RMSE plus a cap on how
many pixels may flip** (`CoreComparison.fs`), replacing the old 1e-6 max /
1e-8 RMSE gate that only ever held because both sides were FP64.

## Validation

All four GPU diagnostics pass under FP32:

| Check | Result |
|---|---|
| `--gate` | passed (FP32 max error 4.77e-7) |
| `--self-test` | passed |
| `--edge-test` | 0 failures across 192 boundary queries |
| `--compare-core` (chair vs CPU) | passed - RMSE 3.5e-4, 14 edge-flip pixels (0.021%) |

All four were re-run after the occupancy change and still pass.

CPU suite unchanged at 1284/1284.

## Tolerances, and how they were chosen

Not by relaxing until things passed. Each was derived:

- **Material self-test, 9.5e-7.** The FP32 device agreed with the FP64 analytic
  reference to 1.19e-7 - exactly one FP32 epsilon. Bound set at eight epsilons.
- **Transport self-test, 5e-5.** The coloured-absorption case missed by 1.47e-5.
  Back-solving through `d(value)/d(distance) ~ 0.32` gives a path-length deficit
  of 4.6e-5, about three times the FP32 surface offset (128 ULP ~ 1.53e-5 at
  unit scale) - i.e. three offset ray-spawns along the path. Offset-induced, and
  it scales with precision, so the bound is set from that mechanism.
- **CPU/GPU RMSE, 2e-3.** Measured 3.5e-4, an order of magnitude of headroom.

## Precision-sensitive changes worth knowing about

Three places where a blind `double -> float32` would have been silently wrong:

- **`DeviceMath.moveOffset`** does one-ULP outward rounding through raw bit
  patterns. The FP64 constants (`1UL`, `0x8000000000000001UL`) had to become
  their FP32 equivalents (`1u`, `0x80000001u`), or the nudge steps by an
  entirely wrong magnitude.
- **Machine epsilon.** Surface offsets are expressed in multiples of it. The
  FP64 value (2.2e-16) is ~9 orders of magnitude too small for FP32 and would
  have produced shadow acne everywhere. Now `DeviceMath.Epsilon`.
- **BVH bounds padding** was a fixed 1e-6, which is smaller than one FP32 ULP
  once coordinates exceed ~8 - so it would have stopped being conservative on
  any large scene and dropped hits along shared edges. Now scales with the
  coordinate.

Reference computations in the self-tests deliberately stayed in FP64 and are
narrowed only at the end: computing them at FP32 too would let both sides make
the same rounding error and agree for the wrong reason.

---

# Phase 2 (CPU): hot-path allocation

The CPU path tracer allocated **4,374 GB** of managed memory rendering one
1024x768 / 256 spp dragon frame - about 21 KB per camera path. That is the
figure that motivated this work.

## What was actually allocating

Guessing was wrong twice, so it was measured. Instrumenting `HitPoint`
construction on a 120x90 / 64 spp render:

| Quantity | Value |
|---|---:|
| HitPoint constructions | 10,403,221 |
| ...of which **misses** | 9,126,789 (87.7%) |
| Total allocated | 6.24 GB |
| Bytes per HitPoint construction | 599 |
| HitPoint's own share (~112 bytes each) | **~19%** |

So `HitPoint` - the obvious suspect, allocated once per primitive test including
every miss - is under a fifth of the total. The remaining 81% is the colour and
ray arithmetic around it.

## Results

Gold Dragon, 200x150, 64 spp, depth 6, five runs each:

| Change | min trace | allocated | gen0 GCs | Verdict |
|---|---:|---:|---:|---|
| baseline | 11.79 s | 45.5 GB | 847 | - |
| `Vector` as struct | 12.65 s | 46.3 GB | 874 | **no gain, reverted** |
| `Colour` as struct | **8.96 s** | **37.9 GB** | 710 | **kept: -24% time, -17% allocation** |

Output is bit-identical in both cases, and the full suite stays at 1284/1284.

`Colour` is the win because the integrator performs several colour operations
per path vertex - throughput multiply, radiance accumulate, and a handful per
light sample - and as a reference type each one was a heap allocation.

`Vector` was not, which is worth recording: .NET 10 performs escape analysis and
can stack-allocate short-lived non-escaping objects, so many of the temporary
vectors were already never reaching the heap. Making it a struct only added copy
cost. **Caveat: that verdict rests on two runs on a machine whose run-to-run
spread is 8.96-13.48 s for the same work (P-core/E-core scheduling). It deserves
re-measuring before being treated as settled.**

## A measurement error worth recording

The first `Colour` measurement showed no improvement. The instrumentation above
was still compiled in - two `Interlocked.Increment` calls on shared counters per
`HitPoint` construction, 10M+ per frame across 22 threads, all contending on one
cache line. Removing it moved the minimum from 11.13 s to 8.96 s.

The lesson is procedural: measurement scaffolding has to come out before the
numbers it produces are trusted, and a result that contradicts a sound
hypothesis is a reason to check the apparatus.

## Colour validation moved

`Colour`'s constructor used to reject non-finite and negative components. A
struct cannot have a `do` binding - the default constructor would skip it - and
the check also ran on every colour operation in the hot path. It now lives in
`Colour.Checked`. Rendered output is still validated at the boundary: the worker
rejects any non-finite, negative or non-float32-representable pixel before
writing an image, so a bad value cannot reach a file unnoticed.
