namespace Tracer.Gpu

open System
open System.Diagnostics
open System.IO
open System.Reflection
open System.Runtime.InteropServices
open System.Text.Json
open ILGPU
open ILGPU.Algorithms
open ILGPU.Runtime
open ILGPU.Runtime.Cuda

[<Struct; StructLayout(LayoutKind.Sequential)>]
type GateVector =
    val X: double
    val Y: double
    val Z: double
    new(x, y, z) = { X = x; Y = y; Z = z }

[<Struct; StructLayout(LayoutKind.Sequential)>]
type GateInput =
    val Origin: GateVector
    val Direction: GateVector
    val Scale: single
    val Identity: int
    new(origin, direction, scale, identity) =
        { Origin = origin; Direction = direction; Scale = scale; Identity = identity }

[<Struct; StructLayout(LayoutKind.Sequential)>]
type GateOutput =
    val TriangleTime: double
    val BoxTime: double
    val Fp64: double
    val Fp32: single
    val Identity: int
    new(triangleTime, boxTime, fp64, fp32, identity) =
        { TriangleTime = triangleTime; BoxTime = boxTime; Fp64 = fp64
          Fp32 = fp32; Identity = identity }

module GateKernel =
    let dot (a: GateVector) (b: GateVector) = a.X * b.X + a.Y * b.Y + a.Z * b.Z

    let cross (a: GateVector) (b: GateVector) =
        GateVector(a.Y * b.Z - a.Z * b.Y, a.Z * b.X - a.X * b.Z, a.X * b.Y - a.Y * b.X)

    let subtract (a: GateVector) (b: GateVector) = GateVector(a.X - b.X, a.Y - b.Y, a.Z - b.Z)

    let triangle (origin: GateVector) (direction: GateVector) =
        let a = GateVector(-1., -1., 2.)
        let e1 = GateVector(2., 0., 0.)
        let e2 = GateVector(0., 2., 0.)
        let p = cross direction e2
        let determinant = dot e1 p
        let mutable time = -1.
        if XMath.Abs determinant > 1.e-14 then
            let inverse = 1. / determinant
            let offset = subtract origin a
            let u = dot offset p * inverse
            let q = cross offset e1
            let v = dot direction q * inverse
            let t = dot e2 q * inverse
            if u >= 0. && v >= 0. && u + v <= 1. && t >= 0. then time <- t
        time

    let box (origin: GateVector) (direction: GateVector) =
        let mutable near = 0.
        let mutable far = Double.PositiveInfinity
        let mutable valid = true
        for axis = 0 to 2 do
            let o = if axis = 0 then origin.X elif axis = 1 then origin.Y else origin.Z
            let d = if axis = 0 then direction.X elif axis = 1 then direction.Y else direction.Z
            let low = if axis = 2 then 2. else -1.
            let high = if axis = 2 then 3. else 1.
            if d = 0. then
                if o < low || o > high then valid <- false
            else
                let t0 = (low - o) / d
                let t1 = (high - o) / d
                near <- XMath.Max(near, XMath.Min(t0, t1))
                far <- XMath.Min(far, XMath.Max(t0, t1))
        if valid && near <= far then near else -1.

    let evaluate (item: GateInput) =
        let mutable fp64 = double item.Scale
        let mutable fp32 = item.Scale
        for step = 1 to 7 do
            fp64 <- XMath.Sqrt(fp64 * fp64 + double step) / 1.125
            fp32 <- XMath.Sqrt(fp32 * fp32 + single step) / 1.125f
        GateOutput(triangle item.Origin item.Direction, box item.Origin item.Direction,
                   fp64, fp32, (item.Identity * 1664525 + 1013904223) ^^^ 0x13579bdf)

    let run (index: Index1D) (input: ArrayView<GateInput>) (output: ArrayView<GateOutput>) =
        output.[index.X] <- evaluate input.[index.X]

module FeasibilityGate =
    let private version (assembly: Assembly) =
        let informational = assembly.GetCustomAttribute<AssemblyInformationalVersionAttribute>()
        if isNull informational then assembly.GetName().Version.ToString() else informational.InformationalVersion

    let run (reportPath: string) =
        let total = Stopwatch.StartNew()
        let createdContext, device = CudaRuntime.createContext ()
        use context = createdContext
        use accelerator = device.CreateCudaAccelerator context
        let clock = Stopwatch.StartNew()
        let kernel =
            accelerator.LoadAutoGroupedStreamKernel<Index1D, ArrayView<GateInput>, ArrayView<GateOutput>>(
                Action<Index1D, ArrayView<GateInput>, ArrayView<GateOutput>>(GateKernel.run))
        accelerator.Synchronize()
        let compileMs = clock.Elapsed.TotalMilliseconds
        let count = 4096
        let inputs =
            Array.init count (fun index ->
                let origin =
                    match index % 8 with
                    | 0 -> GateVector(-0.5, -0.5, 0.)
                    | 1 -> GateVector(3., 0., 0.)
                    | 2 -> GateVector(-1., -1., 0.)
                    | 3 -> GateVector(0., 0., 4.)
                    | 4 -> GateVector(0., -0.75, 0.)
                    | 5 -> GateVector(1., 1., 0.)
                    | 6 -> GateVector(0., 0., 2.5)
                    | _ -> GateVector(-2., -0.5, 2.)
                let direction =
                    if index % 8 = 3 then GateVector(0., 0., -1.)
                    elif index % 8 = 7 then GateVector(1., 0., 0.)
                    else GateVector(0., 0., 1.)
                GateInput(origin, direction, 0.5f + single (index % 17) / 19.f, index))
        let expected = inputs |> Array.map GateKernel.evaluate
        clock.Restart()
        use input = accelerator.Allocate1D<GateInput>(int64 count)
        use output = accelerator.Allocate1D<GateOutput>(int64 count)
        input.CopyFromCPU inputs
        accelerator.Synchronize()
        let uploadMs = clock.Elapsed.TotalMilliseconds
        clock.Restart()
        kernel.Invoke(Index1D count, input.View.BaseView, output.View.BaseView)
        accelerator.Synchronize()
        let firstLaunchMs = clock.Elapsed.TotalMilliseconds
        let repetitions = 40
        clock.Restart()
        for _ = 1 to repetitions do
            kernel.Invoke(Index1D count, input.View.BaseView, output.View.BaseView)
        accelerator.Synchronize()
        let launchMs = clock.Elapsed.TotalMilliseconds / float repetitions
        clock.Restart()
        let actual = output.GetAsArray1D()
        accelerator.Synchronize()
        let downloadMs = clock.Elapsed.TotalMilliseconds
        let mutable fp64Error = 0.
        let mutable fp32Error = 0.
        let mutable geometryError = 0.
        let mutable indexingErrors = 0
        for index = 0 to count - 1 do
            let cpu = expected.[index]
            let gpu = actual.[index]
            if not (Double.IsFinite gpu.Fp64 && Single.IsFinite gpu.Fp32) then
                invalidOp $"CUDA returned a non-finite arithmetic value at index {index}."
            fp64Error <- max fp64Error (abs (cpu.Fp64 - gpu.Fp64))
            fp32Error <- max fp32Error (double (abs (cpu.Fp32 - gpu.Fp32)))
            geometryError <- max geometryError (max (abs (cpu.TriangleTime - gpu.TriangleTime)) (abs (cpu.BoxTime - gpu.BoxTime)))
            if cpu.Identity <> gpu.Identity then indexingErrors <- indexingErrors + 1
        let passed = fp64Error <= 1.e-12 && fp32Error <= 2.e-6 && geometryError <= 1.e-12 && indexingErrors = 0
        let report =
            {| status = if passed then "passed" else "failed"
               accelerator = accelerator.AcceleratorType.ToString()
               device = device.Name
               cudaDriverApiVersion = device.DriverVersion.ToString()
               computeCapability = accelerator.Architecture.ToString()
               deviceDescription = device.ToString()
               deviceMemoryBytes = device.MemorySize
               runtime = RuntimeInformation.FrameworkDescription
               runtimeVersion = Environment.Version.ToString()
               ilgpu = version typeof<Context>.Assembly
               algorithms = version typeof<XMath>.Assembly
               fsharpCore = version typeof<Microsoft.FSharp.Core.Unit>.Assembly
               inputSizeBytes = Marshal.SizeOf<GateInput>()
               outputSizeBytes = Marshal.SizeOf<GateOutput>()
               cases = count
               compileMs = compileMs
               uploadAndAllocationMs = uploadMs
               firstLaunchAndSynchronizationMs = firstLaunchMs
               meanWarmLaunchMs = launchMs
               launchRepetitions = repetitions
               downloadAndSynchronizationMs = downloadMs
               totalMs = total.Elapsed.TotalMilliseconds
               maxFp64AbsoluteError = fp64Error
               maxFp32AbsoluteError = fp32Error
               maxGeometryAbsoluteError = geometryError
               indexingErrors = indexingErrors
               note = "F# static kernel; actual CUDA accelerator; precise default math; CPU only computes validation reference." |}
        let options = JsonSerializerOptions(WriteIndented = true)
        let json = JsonSerializer.Serialize(report, options)
        let directory = Path.GetDirectoryName(Path.GetFullPath reportPath)
        Directory.CreateDirectory directory |> ignore
        File.WriteAllText(reportPath, json + Environment.NewLine)
        printfn "%s" json
        if not passed then invalidOp "F# CUDA feasibility kernel failed numerical agreement."
