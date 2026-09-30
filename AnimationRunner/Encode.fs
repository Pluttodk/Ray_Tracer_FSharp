module AnimationRunner.Encode

open System
open System.Diagnostics
open System.IO

let private run (fileName: string) (arguments: string list) =
    let info = ProcessStartInfo(fileName, RedirectStandardOutput = true, RedirectStandardError = true, UseShellExecute = false)
    for argument in arguments do info.ArgumentList.Add argument
    use proc = Process.Start info
    let stderr = proc.StandardError.ReadToEndAsync()
    proc.StandardOutput.ReadToEnd() |> ignore
    proc.WaitForExit()
    proc.ExitCode, stderr.Result

let ffmpegAvailable () =
    try fst (run "ffmpeg" [ "-hide_banner"; "-version" ]) = 0
    with _ -> false

let arguments (directory: string) (fps: float) (startNumber: int) (audio: string option) (output: string) =
    [ yield! [ "-y"; "-hide_banner"; "-loglevel"; "error"
               "-framerate"; string fps
               "-start_number"; string startNumber
               "-i"; Path.Combine(directory, "frame_%05d.png") ]
      match audio with
      | Some wav -> yield! [ "-i"; wav; "-c:a"; "aac"; "-b:a"; "192k"; "-shortest" ]
      | None -> ()
      yield! [ "-c:v"; "libx264"; "-pix_fmt"; "yuv420p"; "-crf"; "18"
               // H.264 in yuv420p needs even dimensions.
               "-vf"; "pad=ceil(iw/2)*2:ceil(ih/2)*2"
               "-movflags"; "+faststart"
               output ] ]

/// Decodes any audio file ffmpeg understands to 48 kHz stereo float samples.
let decodeAudio (path: string) : Tracer.Animation.Audio.Buffer =
    let info = ProcessStartInfo("ffmpeg", RedirectStandardOutput = true, RedirectStandardError = true, UseShellExecute = false)
    for a in [ "-hide_banner"; "-loglevel"; "error"; "-i"; path; "-f"; "f32le"; "-ac"; "2"; "-ar"; string Tracer.Animation.Audio.sampleRate; "-" ] do
        info.ArgumentList.Add a
    use proc = Process.Start info
    let stderr = proc.StandardError.ReadToEndAsync()
    use memory = new MemoryStream()
    proc.StandardOutput.BaseStream.CopyTo memory
    proc.WaitForExit()
    if proc.ExitCode <> 0 then failwithf "ffmpeg could not decode %s: %s" path (stderr.Result.Trim())
    let bytes = memory.ToArray()
    let frames = bytes.Length / 8
    { Left = Array.init frames (fun i -> float (BitConverter.ToSingle(bytes, 8 * i)))
      Right = Array.init frames (fun i -> float (BitConverter.ToSingle(bytes, 8 * i + 4))) }

let private quote (argument: string) =
    if argument |> Seq.exists (fun c -> Char.IsWhiteSpace c || c = '%' || c = '*' || c = '(') then $"'{argument}'" else argument

/// Encodes the numbered frames to an H.264 MP4. Returns false (and prints the command) if ffmpeg is missing or fails.
let toMp4 (directory: string) (fps: float) (startNumber: int) (audio: string option) (output: string) =
    let args = arguments directory fps startNumber audio output
    let command = "ffmpeg " + String.Join(" ", args |> List.map quote)
    if not (ffmpegAvailable ()) then
        eprintfn "ffmpeg was not found on PATH; encode the frames yourself with:\n  %s" command
        false
    else
        let code, stderr = run "ffmpeg" args
        if code = 0 then true
        else
            eprintfn "ffmpeg failed (exit %d): %s\nCommand: %s" code (stderr.Trim()) command
            false
