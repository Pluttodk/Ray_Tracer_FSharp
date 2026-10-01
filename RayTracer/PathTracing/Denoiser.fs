namespace Tracer.Basics.PathTracing

open System
open System.Runtime.InteropServices

/// Intel Open Image Denoise, bound directly through its C API.
///
/// A path tracer's error falls as 1/sqrt(N), so the last factor of two in noise
/// costs four times the samples. A denoiser breaks that curve: for
/// perceptually-equal quality it typically buys back 4-16x in sample count,
/// which is a larger wall-clock win than anything else in Phase 1.
///
/// The native library is OPTIONAL. If it cannot be loaded the renderer carries
/// on and returns the raw image - denoising must never be the reason a render
/// fails. Point OIDN_LIBRARY_PATH or LD_LIBRARY_PATH at the OIDN `lib`
/// directory to enable it.
module Denoiser =

    [<Literal>]
    let private Library = "OpenImageDenoise"

    // Device type 0 selects the best available device, which prefers CUDA when
    // the CUDA backend and a supported GPU are both present.
    [<DllImport(Library, CallingConvention = CallingConvention.Cdecl)>]
    extern nativeint private oidnNewDevice(int deviceType)

    [<DllImport(Library, CallingConvention = CallingConvention.Cdecl)>]
    extern void private oidnCommitDevice(nativeint device)

    [<DllImport(Library, CallingConvention = CallingConvention.Cdecl)>]
    extern void private oidnReleaseDevice(nativeint device)

    [<DllImport(Library, CallingConvention = CallingConvention.Cdecl)>]
    extern nativeint private oidnNewFilter(nativeint device, string filterType)

    [<DllImport(Library, CallingConvention = CallingConvention.Cdecl)>]
    extern void private oidnReleaseFilter(nativeint filter)

    [<DllImport(Library, CallingConvention = CallingConvention.Cdecl)>]
    extern void private oidnSetSharedFilterImage(
        nativeint filter, string name, nativeint pointer, int format,
        unativeint width, unativeint height,
        unativeint byteOffset, unativeint pixelByteStride, unativeint rowByteStride)

    [<DllImport(Library, CallingConvention = CallingConvention.Cdecl)>]
    extern nativeint private oidnNewBuffer(nativeint device, unativeint byteSize)

    [<DllImport(Library, CallingConvention = CallingConvention.Cdecl)>]
    extern void private oidnReleaseBuffer(nativeint buffer)

    [<DllImport(Library, CallingConvention = CallingConvention.Cdecl)>]
    extern void private oidnWriteBuffer(nativeint buffer, unativeint byteOffset, unativeint byteSize, nativeint source)

    [<DllImport(Library, CallingConvention = CallingConvention.Cdecl)>]
    extern void private oidnReadBuffer(nativeint buffer, unativeint byteOffset, unativeint byteSize, nativeint destination)

    [<DllImport(Library, CallingConvention = CallingConvention.Cdecl)>]
    extern void private oidnSetFilterImage(
        nativeint filter, string name, nativeint buffer, int format,
        unativeint width, unativeint height,
        unativeint byteOffset, unativeint pixelByteStride, unativeint rowByteStride)

    [<DllImport(Library, CallingConvention = CallingConvention.Cdecl)>]
    extern void private oidnSetFilterBool(nativeint filter, string name, [<MarshalAs(UnmanagedType.I1)>] bool value)

    [<DllImport(Library, CallingConvention = CallingConvention.Cdecl)>]
    extern void private oidnSetFilterInt(nativeint filter, string name, int value)

    [<DllImport(Library, CallingConvention = CallingConvention.Cdecl)>]
    extern void private oidnCommitFilter(nativeint filter)

    [<DllImport(Library, CallingConvention = CallingConvention.Cdecl)>]
    extern void private oidnExecuteFilter(nativeint filter)

    [<DllImport(Library, CallingConvention = CallingConvention.Cdecl)>]
    extern int private oidnGetDeviceError(nativeint device, nativeint& outMessage)

    [<Literal>]
    let private FormatFloat3 = 3

    [<Literal>]
    let private QualityHigh = 6

    /// Where the library was found when the default probing (LD_LIBRARY_PATH, the
    /// application directory, the system paths) misses it: OIDN_LIBRARY_PATH (a file
    /// or a directory), then common install prefixes and Blender bundles, which ship
    /// OIDN with its CPU and CUDA devices. Without this the denoiser silently never
    /// ran on a machine whose only OIDN is Blender's, and every "denoised" frame was
    /// the raw 64 spp film.
    let private candidates () =
        let names =
            if OperatingSystem.IsWindows() then [ "OpenImageDenoise.dll" ]
            elif OperatingSystem.IsMacOS() then [ "libOpenImageDenoise.dylib"; "libOpenImageDenoise.2.dylib" ]
            else [ "libOpenImageDenoise.so"; "libOpenImageDenoise.so.2" ]
        let inDirectory (directory: string) =
            names |> List.map (fun name -> IO.Path.Combine(directory, name))
        let home = Environment.GetFolderPath Environment.SpecialFolder.UserProfile
        let subdirectories (root: string) (pattern: string) =
            try
                if String.IsNullOrEmpty root || not (IO.Directory.Exists root) then []
                else IO.Directory.GetDirectories(root, pattern) |> Array.sortDescending |> List.ofArray
            with _ -> []
        let explicit =
            match Environment.GetEnvironmentVariable "OIDN_LIBRARY_PATH" with
            | null | "" -> []
            | path when IO.File.Exists path -> [ path ]
            | path -> inDirectory path @ inDirectory (IO.Path.Combine(path, "lib"))
        let prefixes =
            [ IO.Path.Combine(home, ".local", "lib"); "/usr/local/lib"; "/opt/homebrew/lib" ]
            @ (subdirectories home "oidn*" |> List.map (fun d -> IO.Path.Combine(d, "lib")))
            @ (subdirectories "/opt" "oidn*" |> List.map (fun d -> IO.Path.Combine(d, "lib")))
            @ (subdirectories home "blender*" @ subdirectories "/opt" "blender*"
               |> List.map (fun d -> IO.Path.Combine(d, "lib")))
        explicit @ List.collect inDirectory prefixes

    /// Resolve "OpenImageDenoise" for this assembly's P/Invokes. Installed once, before the
    /// first native call.
    let private resolver =
        lazy (
            try
                NativeLibrary.SetDllImportResolver(
                    Reflection.Assembly.GetExecutingAssembly(),
                    DllImportResolver(fun name assembly searchPath ->
                        if name <> Library then 0n
                        else
                            let mutable handle = 0n
                            let explicitFirst =
                                // An explicit OIDN_LIBRARY_PATH file wins over the default probe.
                                match Environment.GetEnvironmentVariable "OIDN_LIBRARY_PATH" with
                                | null | "" -> false
                                | _ -> true
                            if not explicitFirst then NativeLibrary.TryLoad(name, assembly, searchPath, &handle) |> ignore
                            if handle = 0n then
                                for path in candidates () do
                                    if handle = 0n && IO.File.Exists path then
                                        NativeLibrary.TryLoad(path, &handle) |> ignore
                            if handle = 0n && explicitFirst then
                                NativeLibrary.TryLoad(name, assembly, searchPath, &handle) |> ignore
                            handle))
            with :? InvalidOperationException -> () // a resolver was already set for this assembly
        )

    /// Whether the native library can actually be loaded. Probed once; a failure
    /// is remembered rather than retried per frame.
    let available =
        lazy (
            resolver.Force()
            try
                let device = oidnNewDevice 0
                if device = 0n then false
                else
                    oidnReleaseDevice device
                    true
            with
            | :? DllNotFoundException | :? EntryPointNotFoundException | :? BadImageFormatException -> false)

    let mutable private warned = 0

    /// Print (once per process) that requested denoising did not run: a silently undenoised
    /// film looks exactly like a noisy renderer.
    let warnOnce (message: string) =
        if Threading.Interlocked.Exchange(&warned, 1) = 0 then
            eprintfn "warning: denoising was requested but did not run (%s); frames are raw. Set OIDN_LIBRARY_PATH to the OIDN library or its directory." message

    let private deviceError device =
        let mutable message = 0n
        let code = oidnGetDeviceError(device, &message)
        if code = 0 then None
        else Some(if message = 0n then sprintf "OIDN error %d" code else Marshal.PtrToStringAnsi message)

    /// Denoise a linear HDR image.
    ///
    /// `colour` is interleaved RGB, one float32 triple per pixel, row-major from
    /// the top. `albedo` and `normal` are optional guide buffers in the same
    /// layout; supplying them lets the network keep texture detail and geometric
    /// edges it would otherwise smooth away, and they must be captured at the
    /// first non-specular hit or mirrors and glass denoise to mush.
    ///
    /// Returns None when the library is unavailable or the filter reports an
    /// error, so callers can fall back to the raw image.
    let denoiseWithDiagnostics (width: int) (height: int)
                   (colour: float32 array) (albedo: float32 array option) (normal: float32 array option) =
        if width <= 0 || height <= 0 then Error "Image dimensions must be positive."
        elif colour.Length < width * height * 3 then
            invalidArg (nameof colour) "The colour buffer is smaller than the image."
        elif not available.Value then Error "The Open Image Denoise library could not be loaded."
        else
            // Images go through OIDN buffers rather than pinned .NET arrays.
            //
            // With device type 0 OIDN picks the best device, which here is CUDA,
            // and a GPU device cannot read host memory: oidnSetSharedFilterImage
            // fails with "image data not accessible by the device". Buffers are
            // allocated by the device and work uniformly for CPU and GPU, so
            // this keeps the CUDA path available instead of forcing CPU.
            let output = Array.zeroCreate<float32> (width * height * 3)
            let byteSize = unativeint (width * height * 3 * sizeof<float32>)
            let buffers = ResizeArray<nativeint>()
            let mutable device = 0n
            let mutable filter = 0n
            try
                try
                    device <- oidnNewDevice 0
                    if device = 0n then Error "oidnNewDevice returned null."
                    else
                        oidnCommitDevice device
                        match deviceError device with
                        | Some message -> Error("commit device: " + message)
                        | None ->
                        filter <- oidnNewFilter(device, "RT")
                        if filter = 0n then Error "oidnNewFilter(RT) returned null."
                        else
                            let upload (name: string) (data: float32 array) =
                                let buffer = oidnNewBuffer(device, byteSize)
                                buffers.Add buffer
                                let handle = GCHandle.Alloc(data, GCHandleType.Pinned)
                                try oidnWriteBuffer(buffer, 0un, byteSize, handle.AddrOfPinnedObject())
                                finally handle.Free()
                                oidnSetFilterImage(
                                    filter, name, buffer, FormatFloat3,
                                    unativeint width, unativeint height, 0un, 0un, 0un)
                                buffer
                            upload "color" colour |> ignore
                            albedo |> Option.iter (fun a -> upload "albedo" a |> ignore)
                            normal |> Option.iter (fun n -> upload "normal" n |> ignore)
                            let outputBuffer = oidnNewBuffer(device, byteSize)
                            buffers.Add outputBuffer
                            oidnSetFilterImage(
                                filter, "output", outputBuffer, FormatFloat3,
                                unativeint width, unativeint height, 0un, 0un, 0un)
                            // The film is linear HDR, unclamped and not tone
                            // mapped, which is exactly what "hdr" means here.
                            oidnSetFilterBool(filter, "hdr", true)
                            oidnSetFilterInt(filter, "quality", QualityHigh)
                            // Guide buffers are written deterministically at the
                            // first hit, so they carry no Monte Carlo noise and
                            // OIDN can skip prefiltering them.
                            if albedo.IsSome || normal.IsSome then
                                oidnSetFilterBool(filter, "cleanAux", true)
                            oidnCommitFilter filter
                            oidnExecuteFilter filter
                            match deviceError device with
                            | Some message -> Error message
                            | None ->
                                let handle = GCHandle.Alloc(output, GCHandleType.Pinned)
                                try oidnReadBuffer(outputBuffer, 0un, byteSize, handle.AddrOfPinnedObject())
                                finally handle.Free()
                                match deviceError device with
                                | Some message -> Error message
                                | None -> Ok output
                with
                | :? DllNotFoundException | :? EntryPointNotFoundException | :? BadImageFormatException as error ->
                    Error("native library unavailable: " + error.Message)
            finally
                for buffer in buffers do if buffer <> 0n then oidnReleaseBuffer buffer
                if filter <> 0n then oidnReleaseFilter filter
                if device <> 0n then oidnReleaseDevice device

    /// Convenience wrapper: None on any failure, so a denoiser problem can never
    /// be the reason a render fails.
    let tryDenoise width height colour albedo normal =
        match denoiseWithDiagnostics width height colour albedo normal with
        | Ok image -> Some image
        | Error _ -> None
