namespace Tracer.Gpu

open System
open ILGPU
open ILGPU.Algorithms
open ILGPU.Runtime.Cuda

module CudaRuntime =
    // ILGPU 1.5.3's WSL CUDA discovery installs an assembly resolver and cannot run twice.
    let private devices =
        lazy
            (use discovery = Context.Create(fun builder -> builder.Cuda() |> ignore)
             Array.init (discovery.GetCudaDevices().Count) (fun index -> discovery.GetCudaDevice index))

    let device () =
        let devices = devices.Value
        if devices.Length = 0 then
            raise (NotSupportedException "No CUDA device is available. This worker never selects a CPU accelerator or falls back to CPU rendering.")
        devices.[0]

    let createContext () =
        let device = device ()
        let context =
            Context.Create(fun builder ->
                builder.EnableAlgorithms().Optimize(OptimizationLevel.O2).Math(MathMode.Default) |> ignore)
        context, device
