namespace Tracer.Benchmarks.Reporting

open System
open System.IO
open System.Net
open System.Text

module Report =
    let private label = function "legacy" -> "legacy-port" | engine -> engine
    let private html (value: string) = WebUtility.HtmlEncode(if isNull value then "" else value)
    let private url root path =
        Path.GetRelativePath(root, path).Replace('\\', '/').Split('/')
        |> Array.map Uri.EscapeDataString |> String.concat "/"
    let private format (value: float) = value.ToString("0.###", Data.invariant)
    let private optional (value: Nullable<float>) = if value.HasValue then format value.Value else "unavailable"
    let private measurement name (summary: CaseSummary) =
        summary.Measurements |> Array.find (fun value -> value.Name = name) |> fun value -> value.Distribution
    let private hasValue value = not (obj.ReferenceEquals(value, null))

    let private differences root (summaries: CaseSummary array) =
        summaries
        |> Array.groupBy (fun summary -> summary.Preset, summary.Scene, summary.Material)
        |> Array.collect (fun (_, summaries) ->
            [| for referenceName, comparedName in [ "legacy", "cpu"; "cpu", "gpu"; "legacy", "gpu" ] do
                   let reference = summaries |> Array.tryFind (fun summary -> summary.Engine = referenceName)
                   let compared = summaries |> Array.tryFind (fun summary -> summary.Engine = comparedName)
                   match reference, compared with
                   | Some reference, Some compared ->
                       let initial =
                           { Preset = reference.Preset; Scene = reference.Scene; Material = reference.Material
                             ReferenceEngine = referenceName; ComparedEngine = comparedName
                             ReferencePath = ""; ComparedPath = ""; DiffPath = ""; Status = "unavailable"; Error = ""
                             Mae = Nullable(); Rmse = Nullable(); MaxAbsolute = Nullable(); RelativeRmse = Nullable(); DisplayScale = 4. }
                       if not (hasValue reference.Representative && hasValue compared.Representative) then
                           yield { initial with Error = "At least one engine has no successful output. No substitute image was generated." }
                       elif Data.serialize reference.Settings <> Data.serialize compared.Settings then
                           yield { initial with Error = "Settings differ; this is not a matched linear-image comparison." }
                       else
                           let a, b = reference.Representative, compared.Representative
                           let initial = { initial with ReferencePath = a.LinearPath; ComparedPath = b.LinearPath }
                           try
                               let first, second = LinearImage.read a.LinearPath, LinearImage.read b.LinearPath
                               let mae, rmse, maximum, relative, pixels = LinearImage.compare first second
                               let key = Data.hashText (Tracer.SceneFormat.SceneFiles.hashFile a.LinearPath + Tracer.SceneFormat.SceneFiles.hashFile b.LinearPath)
                               let path = Path.Combine(root, "diffs", key + ".png")
                               LinearImage.writePng path first.Width first.Height pixels
                               yield { initial with DiffPath = path; Status = "success"; Mae = Nullable mae; Rmse = Nullable rmse
                                                    MaxAbsolute = Nullable maximum; RelativeRmse = relative }
                           with ex -> yield { initial with Status = "failure"; Error = ex.Message }
                   | _ -> () |])

    let writeTrials root (trials: Trial array) =
        Data.writeJson (Path.Combine(root, "trials.json")) trials
        let header =
            [| "preset"; "scene"; "material"; "engine"; "fingerprint"; "repeat"; "warmup"; "attempt"; "status"; "error"; "exit_code"
               "cold_ms"; "worker_total_ms"; "load_ms"; "build_ms"; "compile_ms"; "upload_ms"; "trace_synchronized_ms"
               "download_ms"; "encode_ms"; "cleanup_ms"; "allocated_bytes"; "peak_working_set_bytes"; "peak_device_bytes"
               "gc_gen0"; "gc_gen1"; "gc_gen2"; "invalid_pixels"; "device"; "backend"; "width"; "height"
               "camera_samples"; "light_samples"; "glossy_samples"; "max_bounces"; "threads"; "tile_size"; "seed"
               "precision"; "sampler"; "transfer"; "png"; "linear"; "metrics"; "stdout"; "stderr"; "trial_json" |]
        let row (trial: Trial) =
            let c, m = trial.Case, trial.Metrics
            let success = trial.Status = "success" && hasValue m
            let value selector = if success then Data.number (selector m) else ""
            let opt selector = if success then Data.optionalNumber (selector m) else ""
            let integer selector = if success then string (selector m) else ""
            let optionalInteger selector =
                if success then let value: Nullable<int64> = selector m in if value.HasValue then string value.Value else ""
                else ""
            let gc index = if success && not (isNull m.GcCollections) && m.GcCollections.Length > index then string m.GcCollections.[index] else ""
            [| c.Preset; c.Scene; c.Material; label c.Engine; c.Fingerprint; string trial.Repeat; string trial.Warmup; string trial.Attempt
               trial.Status; trial.Error; (if trial.ExitCode.HasValue then string trial.ExitCode.Value else "")
               Data.optionalNumber trial.ColdMs
               value (fun m -> m.Timings.TotalMs); value (fun m -> m.Timings.LoadMs); opt (fun m -> m.Timings.BuildMs)
               opt (fun m -> m.Timings.CompileMs); opt (fun m -> m.Timings.UploadMs); value (fun m -> m.Timings.TraceMs)
               opt (fun m -> m.Timings.DownloadMs); value (fun m -> m.Timings.EncodeMs); value (fun m -> m.Timings.CleanupMs)
               optionalInteger (fun m -> m.AllocatedBytes); integer (fun m -> m.PeakWorkingSetBytes); optionalInteger (fun m -> m.PeakDeviceBytes)
               gc 0; gc 1; gc 2; integer (fun m -> m.InvalidPixels)
               (if hasValue m then m.Device else ""); (if hasValue m then m.Backend else "")
               string c.Settings.Width; string c.Settings.Height; string c.Settings.CameraSamples; string c.Settings.LightSamples
               string c.Settings.GlossySamples; string c.Settings.MaxBounces; string c.Settings.Threads; string c.Settings.TileSize
               string c.Settings.Seed; c.Settings.Precision; c.Settings.Sampler; c.Settings.Transfer
               trial.OutputPath; trial.LinearPath; trial.MetricsPath; trial.StdoutPath; trial.StderrPath; trial.TrialPath |]
        Seq.append [ header ] (trials |> Seq.map row)
        |> Data.csv |> Data.writeText (Path.Combine(root, "trials.csv"))

    let write root (manifest: RunManifest) (trials: Trial array) =
        Directory.CreateDirectory root |> ignore
        writeTrials root trials
        let summaries = Data.summarize trials
        let diffs = differences root summaries
        Data.writeJson (Path.Combine(root, "summary.json")) summaries
        Data.writeJson (Path.Combine(root, "differences.json")) diffs
        let columns = [| "cold"; "worker-total"; "load"; "build"; "compile"; "upload"; "trace-synchronized"; "download"; "encode"; "cleanup"; "allocated"; "peak-working-set"; "peak-device" |]
        let summaryHeader =
            Array.append [| "preset"; "scene"; "material"; "engine"; "fingerprint"; "status"; "required"; "completed"; "failed" |]
                (columns |> Array.collect (fun column -> [| column + "_median"; column + "_min"; column + "_max"; column + "_p25"; column + "_p75"; column + "_mad"; column + "_n" |]))
        let summaryRow (summary: CaseSummary) =
            Array.append
                [| summary.Preset; summary.Scene; summary.Material; label summary.Engine; summary.Fingerprint; summary.Status
                   string summary.Required; string summary.Completed; string summary.Failed |]
                (columns |> Array.collect (fun column ->
                    let stat = measurement column summary
                    if not (hasValue stat) then Array.create 7 ""
                    else [| Data.number stat.Median; Data.number stat.Min; Data.number stat.Max; Data.number stat.P25
                            Data.number stat.P75; Data.number stat.Mad; string stat.Count |]))
        Seq.append [ summaryHeader ] (summaries |> Seq.map summaryRow)
        |> Data.csv |> Data.writeText (Path.Combine(root, "summary.csv"))
        let text = StringBuilder()
        let add (value: string) = text.Append(value).AppendLine() |> ignore
        let link path title = if String.IsNullOrWhiteSpace path then html title else $"<a href=\"{html (url root path)}\">{html title}</a>"
        let distributionCell name summary =
            let stat = measurement name summary
            if not (hasValue stat) then "<td class=\"muted\">unavailable</td>"
            else
                let scale = if name = "allocated" || name.StartsWith("peak-") then 1. / 1048576. else 1.
                let unit = if scale = 1. then "ms" else "MiB"
                $"<td title=\"min {format (stat.Min * scale)}, max {format (stat.Max * scale)}, MAD {format (stat.Mad * scale)}, n={stat.Count}\">{format (stat.Median * scale)} {unit}<small>IQR {format (stat.P25 * scale)}–{format (stat.P75 * scale)}</small></td>"
        let measured = trials |> Array.filter (fun trial -> not trial.Warmup)
        let good = measured |> Array.filter (fun trial -> trial.Status = "success") |> Array.length
        let complete = good = manifest.RequestedMeasuredTrials && manifest.Status = "complete"
                       && not (diffs |> Array.exists (fun diff -> diff.Status = "failure"))
        add "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\"><meta name=\"viewport\" content=\"width=device-width,initial-scale=1\"><title>Classic F# renderer benchmarks</title><style>"
        add "body{font:15px system-ui,sans-serif;margin:0;background:#12161d;color:#e4eaf2}main{max-width:1600px;margin:auto;padding:1.5rem}a{color:#83bcff}h1,h2,h3{line-height:1.2}small{display:block;color:#adbacb;font-size:.77rem}table{border-collapse:collapse;width:100%;font-size:.85rem}td,th{padding:.55rem;text-align:left;border-bottom:1px solid #354052;vertical-align:top}th{position:sticky;top:0;background:#19212d}.scroll{overflow:auto}.muted{color:#adbacb}.good{color:#90e1ac}.bad{color:#ffb09b}.notice{padding:1rem;border:1px solid #596880;border-radius:.5rem;background:#1d2736}.gallery{display:grid;grid-template-columns:repeat(auto-fit,minmax(240px,1fr));gap:1rem}figure{margin:0;padding:.7rem;background:#1a2330;border-radius:.5rem}figure img{width:100%;height:auto;image-rendering:auto;background:black}figcaption{padding:.6rem 0}.case{border-top:2px solid #3b4960;margin-top:2rem;padding-top:1rem}pre{white-space:pre-wrap;overflow-wrap:anywhere;font-size:.8rem}details{margin:.6rem 0}summary{cursor:pointer}.bar{display:inline-block;height:.65rem;background:#73a8df;min-width:1px;margin-right:.4rem}code{overflow-wrap:anywhere}footer{margin-top:2rem}"
        add "</style></head><body><main><h1>Classic F# renderer: before / after</h1>"
        add $"""<p class="{if complete then "good" else "bad"}"><strong>{if complete then "Requested run complete" else "INCOMPLETE requested run"}: {good}/{manifest.RequestedMeasuredTrials} measured trials succeeded.</strong> Warmups are separate and never enter statistics.</p>"""
        if manifest.Cases |> Array.exists (fun scene -> scene.Scene = "gold-dragon") then
            add "<p class=\"notice\"><strong>Gold Dragon is an explicitly selected authored-only bonus.</strong> It is not part of the four-scene, six-material standard/high matrix. Its open scan must not enter the glass sweep. Consult recorded settings to distinguish original 1024x768, 16-sample multi-jittered settings from a targeted preview."
            add "<br><strong>Composition distinction:</strong> The neutral benchmark uses a finite 200000x200000 slab floor; the direct original reproduction uses a truly infinite plane. Matching original settings does not make these two geometries identical."
            add "<br>Dragon dataset credit: <strong>Stanford University Computer Graphics Laboratory</strong>. Optional attributed noncommercial research/free redistribution; commercial use requires Stanford permission. The dataset and its rendered images are not relicensed under the renderer's GPL. <a href=\"https://graphics.stanford.edu/data/3Dscanrep/\">Official Stanford terms</a>.</p>"
        add "<div class=\"notice\"><strong>Comparison contract.</strong> “legacy-port” is the frozen renderer with documented Linux/headless adaptations on this modern .NET runtime, not historical Windows/.NET Framework performance. Corrected CPU/GPU images intentionally differ from legacy defects. Wall-time ratios describe the whole modernization, not a pure algorithm speedup. No CPU fallback or copied image is used for an unavailable GPU."
        add "<p>Cold = isolated subprocess wall time, including startup, JIT, loading, acceleration, GPU compilation/transfers/synchronization, tracing, encoding, cleanup and process exit. Trace is a synchronized phase, <em>not</em> GPU end-to-end latency. Each repeat starts a new process: separate warmups warm OS/device caches, not the measured process’s JIT. Null/unexecuted phases are unavailable, never zero-valued observations. Median, IQR, full min/max and MAD retain every successful measured repeat; failed/slow/timeout cases remain visible.</p>"
        add "<p>Phase boundaries remain coarse: mkPLY may include mesh acceleration in load. Current legacy captures time the original single PreProcessing call in build; trace still includes original per-pixel display conversion and raw capture. Buffer setup, final vertical flip and float packing contribute to encode preparation. Earlier legacy captures also included scene-tree construction in trace. Consult each worker's capability/phase notes before comparing subdivisions; cold wall time includes all of them and remains the inclusive comparison.</p>"
        add "<p>Linear comparisons use retained RGB PFM (little-endian float32 storage, unclamped linear radiance, bottom-up on disk). Render arithmetic precision is listed separately. Difference previews are sqrt(4 × absolute linear error), clipped only for display; MAE/RMSE/max are computed before display clipping. Legacy differences are diagnostic, not a global correctness test. Images select the lowest successful repeat, not the prettiest or fastest trial. Every repetition is retained below.</p></div>"
        add $"""<p>{link (Path.Combine(root, "run-manifest.json")) "Run manifest"} · {link (Path.Combine(root, "trials.json")) "All trial JSON"} · {link (Path.Combine(root, "trials.csv")) "Trial CSV"} · {link (Path.Combine(root, "summary.csv")) "Summary CSV"} · {link (Path.Combine(root, "differences.json")) "Linear differences"}</p>"""
        add $"<details><summary>Reproducibility: SDK, source/asset/binary hashes, dependencies, device and limitations</summary><pre>{html (Data.serialize manifest.Metadata)}</pre></details>"
        add "<h2>Measured phase and memory summaries</h2><p class=\"muted\">Cells: median and interquartile range. Hover for min/max, median absolute deviation and number of observations. Allocation is process-wide managed allocation; memory peaks are not interchangeable with allocations.</p><div class=\"scroll\"><table><thead><tr><th>Case</th><th>Engine / completion</th>"
        for column in columns do add $"<th>{html column}</th>"
        add "</tr></thead><tbody>"
        for summary in summaries do
            add $"""<tr><td>{html summary.Preset} / {html summary.Scene} / {html summary.Material}<small>{summary.Settings.Width}×{summary.Settings.Height}, camera {summary.Settings.CameraSamples}, light {summary.Settings.LightSamples}, glossy {summary.Settings.GlossySamples}, depth {summary.Settings.MaxBounces}; {summary.Settings.Threads} threads, tile {summary.Settings.TileSize}; {html summary.Settings.Precision}, {html summary.Settings.Sampler}, {html summary.Settings.Transfer}</small></td><td class="{if summary.Status = "complete" then "good" else "bad"}">{html (label summary.Engine)}<small>{summary.Completed}/{summary.Required} successful</small></td>"""
            for column in columns do add (distributionCell column summary)
            add "</tr>"
        add "</tbody></table></div><h2>Before / after gallery</h2>"
        for (preset, scene, material), group in summaries |> Array.groupBy (fun summary -> summary.Preset, summary.Scene, summary.Material) do
            add $"<section class=\"case\"><h3>{html preset} / {html scene} / {html material}</h3>"
            let successful = group |> Array.filter (fun summary -> hasValue (measurement "cold" summary))
            if successful.Length > 0 then
                let maxTime = successful |> Array.map (fun summary -> (measurement "cold" summary).Median) |> Array.max |> max 0.001
                for summary in successful do
                    let stat = measurement "cold" summary
                    add $"<p><span class=\"bar\" style=\"width:{format (stat.Median / maxTime * 70.)}%%\"></span>{html (label summary.Engine)}: cold median {format stat.Median} ms</p>"
            for baselineName, candidateName in [ "legacy", "cpu"; "cpu", "gpu" ] do
                let baseline = group |> Array.tryFind (fun summary -> summary.Engine = baselineName && summary.Status = "complete")
                let candidate = group |> Array.tryFind (fun summary -> summary.Engine = candidateName && summary.Status = "complete")
                match baseline, candidate with
                | Some baseline, Some candidate when Data.serialize baseline.Settings = Data.serialize candidate.Settings ->
                    let a, b = measurement "cold" baseline, measurement "cold" candidate
                    if hasValue a && hasValue b && b.Median > 0. then
                        add $"<p>Matched cold end-to-end median ratio {html (label baselineName)}/{html (label candidateName)}: <strong>{format (a.Median / b.Median)}×</strong> (whole-render comparison, not kernel-only throughput).</p>"
                | _ -> ()
            add "<div class=\"gallery\">"
            for summary in group do
                add $"<figure><figcaption><strong>{html (label summary.Engine)}</strong> — {summary.Completed}/{summary.Required} measured outputs</figcaption>"
                if hasValue summary.Representative then
                    let trial = summary.Representative
                    add $"""<a href="{html (url root trial.OutputPath)}"><img loading="lazy" src="{html (url root trial.OutputPath)}" alt="{html (scene + " " + material + " " + label summary.Engine)}"></a>"""
                    add $"""<figcaption>Repeat {trial.Repeat}: {link trial.LinearPath "raw linear PFM"} · {link trial.MetricsPath "worker metrics"}<small>{html trial.Metrics.Device}; {html trial.Metrics.Backend}</small></figcaption>"""
                else add "<p class=\"bad\">No successful output. No replacement image.</p>"
                if summary.Errors.Length > 0 then add $"""<details><summary class="bad">{summary.Errors.Length} unsuccessful trials</summary><pre>{html (String.concat "\n" summary.Errors)}</pre></details>"""
                add "</figure>"
            for diff in diffs |> Array.filter (fun diff -> diff.Preset = preset && diff.Scene = scene && diff.Material = material) do
                add $"<figure><figcaption><strong>Difference: {html (label diff.ReferenceEngine)} ↔ {html (label diff.ComparedEngine)}</strong></figcaption>"
                if diff.Status = "success" then
                    add $"<a href=\"{html (url root diff.DiffPath)}\"><img loading=\"lazy\" src=\"{html (url root diff.DiffPath)}\" alt=\"4x linear difference\"></a><figcaption>Linear MAE {optional diff.Mae}; RMSE {optional diff.Rmse}; max {optional diff.MaxAbsolute}; relative RMSE {optional diff.RelativeRmse}<small>Display: gamma 2, absolute error ×4. Relative RMSE is unavailable for a black reference with nonzero error.</small></figcaption>"
                else add $"<p class=\"bad\">{html diff.Error}</p>"
                add "</figure>"
            add "</div></section>"
        add "<h2>Every trial, including failures and separate warmups</h2><div class=\"scroll\"><table><thead><tr><th>Case / engine</th><th>Repeat</th><th>Status</th><th>Cold wall</th><th>Retained artifacts</th><th>Diagnostic</th></tr></thead><tbody>"
        for trial in trials do
            add $"""<tr><td>{html trial.Case.Preset} / {html trial.Case.Scene} / {html trial.Case.Material} / {html (label trial.Case.Engine)}</td><td>{if trial.Warmup then "warmup " else "measured "}{trial.Repeat}<small>attempt {trial.Attempt}</small></td><td class="{if trial.Status = "success" then "good" else "bad"}">{html trial.Status}</td><td>{optional trial.ColdMs} ms</td><td>"""
            for path, title in [ trial.OutputPath, "PNG"; trial.LinearPath, "PFM"; trial.MetricsPath, "metrics"; trial.TrialPath, "trial"; trial.WorkerMetadataPath, "capabilities"; trial.StdoutPath, "stdout"; trial.StderrPath, "stderr" ] do
                if not (String.IsNullOrWhiteSpace path) && File.Exists path then add (link path title + " ")
            add $"</td><td><pre>{html trial.Error}</pre></td></tr>"
        add "</tbody></table></div><footer><p>Local-only self-contained HTML/CSS; relative images and data. Nothing is uploaded. Quick is a smoke preset, not evidence of established speedups. Standard/high normally require five measured repeats each; targeted overrides are explicitly recorded.</p></footer></main></body></html>"
        Data.writeText (Path.Combine(root, "index.html")) (text.ToString())
        diffs |> Array.exists (fun diff -> diff.Status = "failure")
