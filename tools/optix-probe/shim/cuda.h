// Minimal stand-in for the CUDA toolkit's cuda.h.
//
// optix_host.h includes <cuda.h> purely for two opaque handle types. The full
// toolkit is only needed to COMPILE DEVICE CODE; the host API and the driver
// live in libcuda.so, which WSL already exposes. Declaring the handles here
// lets the feasibility probe build with plain g++ and no toolkit install.
//
// CUdeviceptr is deliberately absent: optix_types.h defines it unconditionally,
// so declaring it here would be a duplicate definition.
#pragma once
typedef struct CUctx_st*    CUcontext;
typedef struct CUstream_st* CUstream;
