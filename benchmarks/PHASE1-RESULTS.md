# Phase 1 measured results

Path tracing, denoising and adaptive sampling, measured on the Intel Core Ultra
9 185H (22 logical processors) / NVIDIA RTX 3000 Ada, 2026-09-10. Every number
below is measured on this machine, not quoted from a paper.

## Denoiser (Intel Open Image Denoise 2.3.3)

Gold Dragon, 200x150, depth 6, multi-jittered, compared against a 1024-spp
render of the same scene. Guide buffers are albedo and shading normal captured
at the first non-specular hit.

| spp | noisy relRMSE | denoised relRMSE | equal-error spp | gain |
|---:|---:|---:|---:|---:|
| 4 | 0.25048 | 0.11414 | ~19 | **4.8x** |
| 16 | 0.12575 | 0.07704 | ~43 | **2.7x** |
| 64 | 0.06162 | 0.04639 | ~113 | **1.8x** |
| 256 | 0.02896 | 0.02637 | ~309 | **1.2x** |

"Equal-error spp" is how many un-denoised samples would be needed to reach the
denoised error, given that Monte Carlo error falls as 1/sqrt(N).

**The commonly quoted "4-16x fewer samples" does not hold at production sample
counts.** It is 4.8x at 4 spp and only 1.2x at 256 spp: the benefit shrinks as
the render approaches convergence, because there is progressively less noise to
remove. Plan for a large win on previews and a small one on final frames.

Two caveats on the method:

- RMSE **understates** the perceptual benefit. The residual error after
  denoising is smooth and low-frequency, which the eye tolerates far better than
  the same magnitude of grain. Compare
  `artifacts/path-tracer-first/dragon-16spp-denoise-{off,on}.png`: the measured
  gain at 16 spp is 2.7x, but the visible difference is much larger than that
  ratio suggests.
- The 1024-spp reference is not noise-free. At 256 spp both images approach the
  reference's own noise floor, so the 1.2x figure is compressed by the metric.

Cost is small and outside the trace phase: about 2.4 s on a 14 s, 400x300
render, including device setup and CUDA kernel compilation.

## Adaptive sampling — measured as a NET LOSS, left off by default

Per-pixel Welford variance on luminance; a pixel stops once the standard error
of its mean falls below `threshold * mean`.

Sphere-on-plane under an environment light plus a point light, 160x120, 256 spp,
against a 1024-spp reference. Efficiency is `1 / (variance * time)` normalized
to uniform sampling, so **above 1.00x means adaptive was worth it**:

| threshold | min spp | time (s) | mean spp | relRMSE | efficiency |
|---:|---:|---:|---:|---:|---:|
| off | - | 39.72 | 256.0 | 0.00716 | 1.00x |
| 0.005 | 16 | 28.98 | 179.9 | 0.00970 | 0.75x |
| 0.010 | 16 | 13.95 | 89.3 | 0.03066 | 0.16x |
| 0.020 | 16 | 7.20 | 45.9 | 0.04537 | 0.14x |
| 0.005 | 64 | 27.11 | 181.2 | 0.00957 | 0.82x |
| 0.010 | 64 | 18.05 | 117.1 | 0.02008 | 0.28x |
| 0.020 | 64 | 12.65 | 83.4 | 0.02640 | 0.23x |
| 0.005 | 128 | 29.44 | 193.1 | 0.00929 | 0.80x |
| 0.010 | 128 | 24.31 | 160.0 | 0.01310 | 0.49x |
| 0.020 | 128 | 19.48 | 136.6 | 0.01694 | 0.36x |

**Every configuration loses.** It does cut time - at threshold 0.02 the render
is 5.5x faster - but variance rises faster than time falls, so the image is
worse than simply rendering fewer uniform samples for the same cost.

Raising the minimum sample count clearly helps (0.16x to 0.49x at threshold
0.01), which shows part of the loss is an unreliable variance estimate over few
samples. But it never reaches parity, so this is not merely a tuning problem:
the technique does not pay on scenes whose variance is fairly uniform across the
frame. These test scenes have no caustics, no small bright emitters and no
deep-shadow regions - exactly the heterogeneity adaptive sampling exists to
exploit.

This matches the literature's own caveat rather than its headline: Cycles quotes
10-30% savings, and Arnold users report cases where adaptive sampling made
renders slower.

Kept as opt-in (`--adaptive <threshold>`, default 0 = off) because it costs
nothing when disabled and may pay on scenes with strong variance heterogeneity.
It should not be enabled by default, and it is not a Phase 1 win.

## What this changes about the plan

The plan estimated 4-16x for denoising and 1.2-2x for adaptive sampling.
Measured: denoising is **1.2-4.8x depending on sample count**, and adaptive
sampling is **below 1.0x on these scenes**. The denoiser remains the single
largest Phase 1 win after the path tracer itself; adaptive sampling should not
be counted towards the total.
