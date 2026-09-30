// OptiX feasibility probe (Milestone 0).
//
// Answers one question: does hardware-accelerated OptiX actually initialize on
// this machine, under WSL2, with this driver? Everything in Phase 3 of the plan
// is contingent on the answer, so it is settled before any code is written
// against it.
//
// Deliberately has no CUDA-toolkit dependency. The driver API is resolved at
// runtime out of libcuda.so.1, and OptiX resolves itself out of
// libnvoptix.so.1 via optixInit(), both of which WSL maps in from the Windows
// driver. Build: g++ -I<optix>/include -Ishim probe.cpp -ldl

#include <cstdio>
#include <cstring>
#include <cstdlib>
#include <dlfcn.h>
#include <optix.h>
#include <optix_stubs.h>
#include <optix_function_table_definition.h>

typedef int CUresult;
typedef int CUdevice;

static void* cudaLib = nullptr;
template <typename T> static T sym(const char* name) {
    void* p = dlsym(cudaLib, name);
    if (!p) { std::printf("  ! libcuda is missing %s\n", name); }
    return reinterpret_cast<T>(p);
}

static void logCallback(unsigned int level, const char* tag, const char* message, void*) {
    std::printf("  [optix %u][%s] %s\n", level, tag ? tag : "", message ? message : "");
}

int main() {
    std::printf("OptiX feasibility probe\n");
    std::printf("  header OPTIX_VERSION = %d (%d.%d.%d)\n",
                OPTIX_VERSION, OPTIX_VERSION / 10000,
                (OPTIX_VERSION % 10000) / 100, OPTIX_VERSION % 100);

    // Claim the libnvoptix_loader.so.1 SONAME with the REAL OptiX loader before
    // anything else touches it.
    //
    // WSL ships a dxcore shim at /usr/lib/wsl/lib/libnvoptix.so.1 that carries
    // the same SONAME but exports no OptiX entry point. cuInit() loads it, and
    // from then on any dlopen of that SONAME returns the shim, so optixInit()
    // fails with OPTIX_ERROR_ENTRY_SYMBOL_NOT_FOUND. Loading the real loader
    // first means every later request resolves to it instead.
    if (const char* preload = getenv("OPTIX_REAL_LOADER")) {
        void* pre = dlopen(preload, RTLD_NOW | RTLD_GLOBAL);
        std::printf("  preloaded real OptiX loader: %s\n", pre ? "ok" : dlerror());
    }

    cudaLib = dlopen("libcuda.so.1", RTLD_NOW);
    if (!cudaLib) { std::printf("FAIL: cannot dlopen libcuda.so.1: %s\n", dlerror()); return 2; }
    std::printf("  libcuda.so.1 loaded\n");

    auto cuInit          = sym<CUresult(*)(unsigned)>("cuInit");
    auto cuDeviceGet     = sym<CUresult(*)(CUdevice*, int)>("cuDeviceGet");
    auto cuDeviceGetName = sym<CUresult(*)(char*, int, CUdevice)>("cuDeviceGetName");
    auto cuCtxCreate     = sym<CUresult(*)(CUcontext*, unsigned, CUdevice)>("cuCtxCreate_v2");
    if (!cuInit || !cuDeviceGet || !cuDeviceGetName || !cuCtxCreate) return 2;

    if (cuInit(0) != 0) { std::printf("FAIL: cuInit\n"); return 2; }
    CUdevice device = 0;
    if (cuDeviceGet(&device, 0) != 0) { std::printf("FAIL: cuDeviceGet\n"); return 2; }
    char name[256] = {0};
    cuDeviceGetName(name, sizeof(name), device);
    std::printf("  CUDA device 0: %s\n", name);

    // optixInit MUST run before a CUDA context exists.
    //
    // Creating a context loads the WSL dxcore shim, which is also named
    // libnvoptix.so.1 and carries SONAME libnvoptix_loader.so.1. Once that
    // SONAME is in the process, a later dlopen of the real OptiX loader just
    // returns the already-loaded shim - which exports no optixQueryFunctionTable
    // and so fails with OPTIX_ERROR_ENTRY_SYMBOL_NOT_FOUND. Loading OptiX first
    // wins the name.
    OptixResult result = optixInit();
    if (result != OPTIX_SUCCESS) {
        std::printf("FAIL: optixInit -> %d (%s)\n", (int)result, optixGetErrorName(result));
        std::printf("  This is the go/no-go for hardware RT on this machine.\n");
        return 1;
    }
    std::printf("  optixInit OK -> compiled against ABI version %d\n", OPTIX_ABI_VERSION);

    CUcontext context = nullptr;
    if (cuCtxCreate(&context, 0, device) != 0) { std::printf("FAIL: cuCtxCreate\n"); return 2; }
    std::printf("  CUDA context created\n");

    OptixDeviceContextOptions options = {};
    options.logCallbackFunction = &logCallback;
    options.logCallbackLevel = 4;
    OptixDeviceContext optixContext = nullptr;
    result = optixDeviceContextCreate(context, &options, &optixContext);
    if (result != OPTIX_SUCCESS) {
        std::printf("FAIL: optixDeviceContextCreate -> %d (%s)\n", (int)result, optixGetErrorName(result));
        return 1;
    }
    std::printf("  optixDeviceContextCreate OK\n");

    unsigned int rtcoreVersion = 0;
    optixDeviceContextGetProperty(optixContext, OPTIX_DEVICE_PROPERTY_RTCORE_VERSION,
                                  &rtcoreVersion, sizeof(rtcoreVersion));
    unsigned int maxTraceDepth = 0, maxPrimsPerGas = 0, maxInstancesPerIas = 0;
    optixDeviceContextGetProperty(optixContext, OPTIX_DEVICE_PROPERTY_LIMIT_MAX_TRACE_DEPTH,
                                  &maxTraceDepth, sizeof(maxTraceDepth));
    optixDeviceContextGetProperty(optixContext, OPTIX_DEVICE_PROPERTY_LIMIT_MAX_PRIMITIVES_PER_GAS,
                                  &maxPrimsPerGas, sizeof(maxPrimsPerGas));
    optixDeviceContextGetProperty(optixContext, OPTIX_DEVICE_PROPERTY_LIMIT_MAX_INSTANCES_PER_IAS,
                                  &maxInstancesPerIas, sizeof(maxInstancesPerIas));

    std::printf("\nRESULT: OptiX is USABLE on this machine.\n");
    std::printf("  RT core version      : %u  (0 would mean software fallback, i.e. no RT cores)\n", rtcoreVersion);
    std::printf("  max trace depth      : %u\n", maxTraceDepth);
    std::printf("  max primitives / GAS : %u\n", maxPrimsPerGas);
    std::printf("  max instances / IAS  : %u\n", maxInstancesPerIas);

    optixDeviceContextDestroy(optixContext);
    return 0;
}
