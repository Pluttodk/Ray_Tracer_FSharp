# OptiX feasibility probe

Answers one question before any work is committed to hardware ray tracing:
**does OptiX actually initialize on this machine?**

```sh
git clone --depth 1 https://github.com/NVIDIA/optix-dev
./build-and-run.sh optix-dev
```

Requires no CUDA toolkit and no root. `shim/cuda.h` supplies the two opaque
handle types `optix_host.h` wants; the driver API is resolved at runtime from
`libcuda.so.1`.

## Result on this machine, 2026-09-10

**OptiX is NOT usable.** Recorded so it is not re-investigated from scratch.

```
header OPTIX_VERSION = 90100 (9.1.0)
CUDA device 0: NVIDIA RTX 3000 Ada Generation Laptop GPU   <- CUDA is fine
FAIL: optixInit -> 7805 (OPTIX_ERROR_ENTRY_SYMBOL_NOT_FOUND)
```

Driver 596.47 (R590+) satisfies OptiX 9.1's requirement, and CUDA initializes
normally, so this is specific to OptiX.

### Why

On a native Linux driver install there are two libraries:

| Library | Role | Size |
|---|---|---|
| `libnvoptix_loader.so.1` | thin loader, exports `optixQueryFunctionTable` | ~72 KB |
| `libnvoptix.so.1` | the actual OptiX implementation | tens of MB |

The WSL driver ships **only the first**, and the name `libnvoptix.so.1` in
`/usr/lib/wsl/lib` is a 10 KB dxcore shim exporting nothing but `dxcore_*`. The
real implementation exists solely as `nvoptix.dll` in
`/usr/lib/wsl/drivers/*/` - a Windows PE that a Linux process cannot load.

So the chain breaks at the last link: the loader dlopens `libnvoptix.so.1`,
receives the dxcore shim, finds no OptiX entry point, and returns 7805.

Steps taken to rule out simpler explanations:

- `dlsym` genuinely finds `optixQueryFunctionTable` - the failure comes from
  *inside* that function, not from symbol lookup.
- Called it directly across ABI versions 60-118: all return 7805, so it is not
  an ABI mismatch.
- Tried all three older `libnvoptix_loader.so.1` copies still present in the
  driver store (Mar/May/Jul 2025), each paired with its own `nvoptix.bin`.
- Pre-loaded the real loader before `cuInit` to win the shared
  `libnvoptix_loader.so.1` SONAME, confirming via `dlinfo` that the correct file
  was returned. Still 7805.
- Scanned every `.so` under `/usr/lib/wsl` for `optixQueryFunctionTable`: only
  loaders, no implementation.

### Consequence

The plan's Phase 3 (native OptiX shim + SER) cannot proceed under WSL2. The two
documented fallbacks are:

1. **Build and run on Windows**, where `nvoptix.dll` is present and OptiX would
   work. Costs the Linux workflow.
2. **FP32 + wavefront in ILGPU** with software traversal. Keeps everything as
   is and still captures the largest share of the available speedup - FP32 alone
   is worth 10-30x on Ada, against roughly 1.5-3x for RT cores over good
   software traversal.

Re-run this probe after any driver update; if NVIDIA ships a Linux OptiX
implementation for WSL, it will start passing.
